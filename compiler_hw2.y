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
    static void insert_symbol();
    static void lookup_symbol();
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
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING
%token '+' '-' '*' '/' '%' INC DEC
%token '>' '<' GEQ LEQ EQL NEQ
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR '!'
%token '(' ')' '[' ']' '{' '}'
%token ';' ',' NEWLINE
%token PRINT PRINTLN IF ELSE FOR
%token <id> IDENT

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <b_val> BOOL_LIT

/* Nonterminal with return, which need to sepcify type */
%type <type> Type TypeName ArrayType

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
	: UnaryExpr | Expression binary_op Expression
;

UnaryExpr
	: PrimaryExpr | unary_op UnaryExpr
;

binary_op
	: LOR | LAND | cmp_op | add_op | mul_op
;
	
cmp_op
	: EQL | NEQ | '<' | LEQ | '>' | GEQ
;

add_op
	: '+' { printf("ADD\n"); } | '-' { printf("SUB\n"); }
;

mul_op
	: '*' { printf("MUL\n"); } | '/' { printf("QUO\n"); } | '%' { printf("REM\n"); }
;

unary_op
	: '+' | '-' | '!'
;

PrimaryExpr
	: Operand | IndexExpr | ConversionExpr
;

Operand
	: Literal | IDENT { printf("IDENT\n"); } | '(' Expression ')'
;

Literal
	: INT_LIT | FLOAT_LIT | BOOL_LIT | STRING_LIT
;

IndexExpr
	: PrimaryExpr '[' Expression ']'
;

ConversionExpr
	: Type '(' Expression ')'
;

Statement
	: DeclarationStmt NEWLINE { printf("DeclarationStmt\n"); }
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
	: VAR IDENT Type
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
	: Expression INC { printf("INC\n"); }
	| Expression DEC { printf("DEC\n"); }
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

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
	symbol_table[current_scope] = NULL;	
}

static void insert_symbol() {
	entry *tail = symbol_table[current_scope], *ptr;
	for(; tail != NULL; tail = tail->next);
	ptr = malloc(sizeof(entry));
	if(!ptr){
		printf("malloc failed\n");
		exit(1);
	}
	ptr->index = tail->index + 1;
	strcpy(ptr->name, yylval.id);
	ptr->address = tail->address + 1;
	tail->next = ptr;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", yylval.id, 0);
}

static void lookup_symbol() {
}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", 0);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            0, "name", "type", 0, 0, "element type");
}
