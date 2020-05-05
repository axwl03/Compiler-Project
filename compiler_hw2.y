/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

	/* Symbol table */
	entry *symbol_table[100];
	int current_scope = 0;

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
	static void insert_symbol(char *id, char *type, char *element_type);
    static char *lookup_symbol(char *id);
    static void dump_symbol();
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
	bool b_val;
	char *type;
	char *id;
	char *output;
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING
%token INC DEC
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token '(' ')' '{' '}'
%token ';' ',' NEWLINE
%token PRINT PRINTLN IF ELSE FOR

%left LOR
%left LAND
%left '>' '<' GEQ LEQ EQL NEQ
%left '+' '-'
%left '*' '/' '%'
%nonassoc POS NEG '!'
%nonassoc '[' ']'

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <b_val> BOOL_LIT
%token <id> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <type> Type TypeName ArrayType
%type <output> Expression UnaryExpr PrimaryExpr Operand binary_op add_op cmp_op mul_op

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

Type
	: TypeName | ArrayType
;

TypeName
	: INT | FLOAT | BOOL | STRING
;

ArrayType
	: '[' Expression ']' Type
;

Expression
	: UnaryExpr { $$ = $1; } | Expression binary_op Expression { printf("%s%s%s", $1, $3, $2); }
;

UnaryExpr
	: PrimaryExpr { $$ = $1; } | unary_op UnaryExpr
;

binary_op
	: LOR | LAND | cmp_op { $$ = $1; } | add_op { $$ = $1; } | mul_op { $$ = $1; }
;
	
cmp_op
	: EQL { $$ = "EQL\n"; } | NEQ { $$ = "NEQ\n"; } | '<' { $$ = "\n"; } | LEQ { $$ = "LEQ\n"; } | '>' { $$ = "GTR\n"; } | GEQ { $$ = "GEQ\n"; }
;

add_op
	: '+' { $$ = "ADD\n"; } | '-' { $$ = "SUB\n"; }
;

mul_op
	: '*' { $$ = "MUL\n"; } | '/' { $$ = "QUO\n"; } | '%' { $$ = "REM\n"; }
;

unary_op
	: '+' %prec POS { printf("POS\n"); } | '-' %prec NEG { printf("NEG\n"); } | '!' { printf("NOT\n"); }
;

PrimaryExpr
	: Operand { $$ = $1; } | IndexExpr | ConversionExpr
;

Operand
	: Literal | IDENT { $$ = lookup_symbol($1); } | '(' Expression ')'
;

Literal
	: INT_LIT { printf("INT_LIT\n"); } | FLOAT_LIT { printf("FLOAT_LIT\n"); } | BOOL_LIT { printf("BOOL_LIT\n"); } | STRING_LIT { printf("STRING_LIT\n"); }
;

IndexExpr
	: PrimaryExpr '[' Expression ']'
;

ConversionExpr
	: Type '(' Expression ')'
;

Statement
	: DeclarationStmt NEWLINE
	| SimpleStmt NEWLINE
	| Block NEWLINE { printf("Block\n"); }
	| IfStmt NEWLINE { printf("IfStmt\n"); }
	| ForStmt NEWLINE { printf("ForStmt\n"); }
	| PrintStmt NEWLINE { printf("PrintStmt\n"); }
	| NEWLINE
;

SimpleStmt
	: AssignmentStmt | ExpressionStmt | IncDecStmt
;

DeclarationStmt
	: VAR IDENT Type { insert_symbol($2, $3, NULL); }
	| VAR IDENT Type '=' Expression
;

AssignmentStmt
	: Expression assign_op Expression
;

assign_op
	: '=' | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | QUO_ASSIGN | REM_ASSIGN
;

ExpressionStmt
	: Expression
;

IncDecStmt
	: Expression INC { printf("%sINC\n", $1); }
	| Expression DEC { printf("%sDEC\n", $1); }
;

Block
	: '{' StatementList '}'
;

StatementList
    : StatementList Statement
    | Statement
;

IfStmt
	: IF Condition Block
	| IF Condition Block ELSE IfStmt
	| IF Condition Block ELSE Block
;

Condition
	: Expression
;

ForStmt
	: FOR Condition Block
	| FOR ForClause Block
;

ForClause
	: InitStmt ';' Condition ';' PostStmt
;

InitStmt
	: SimpleStmt
;

PostStmt
	: SimpleStmt
;

PrintStmt
	: PRINT '(' Expression ')'
	| PRINTLN '(' Expression ')'
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
	symbol_table[0] = NULL;

    yylineno = 0;
    yyparse();

	dump_symbol();
	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
	symbol_table[current_scope] = NULL;	
}

static void insert_symbol(char *id, char *type, char *element_type) {
	entry *tail = symbol_table[current_scope], *ptr;
	ptr = malloc(sizeof(entry));
	if(!ptr){
		printf("malloc failed\n");
		exit(1);
	}
	if(tail == NULL){
		ptr->index = 0;
		ptr->address = 0;
		symbol_table[current_scope] = ptr;
	}
	else{
		while(tail->next != NULL) tail = tail->next;
		ptr->index = tail->index + 1;
		ptr->address = tail->address + 1;
		tail->next = ptr;
	}
	strcpy(ptr->name, id);
	strcpy(ptr->type, type);
	ptr->lineno = yylineno;
	if(element_type == NULL)
		strcpy(ptr->element_type, "-");
	ptr->next = NULL;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", id, 0);
}

static char *lookup_symbol(char *id) {
	char *str;
	for(entry *ptr = symbol_table[current_scope]; ptr != NULL; ptr = ptr->next){
		if(strcmp(id, ptr->name) == 0){
			str = malloc(100*sizeof(char));
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			sprintf(str, "IDENT (name=%s, address=%d)\n", ptr->name, ptr->address);
			return str;
		}
	}
	return NULL;
}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", 0);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");	
	for(entry *ptr = symbol_table[current_scope]; ptr != NULL; ptr = ptr->next){
    printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            ptr->index, ptr->name, ptr->type, ptr->address, ptr->lineno, ptr->element_type);
	}
}
