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

	/* dynamic string concatenation */
	char *dynamic_strcat(int n, ...);

	/* evaluate expression's return type */
	int evaluate_type(int type1, int type2);
	
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
	struct {
		char *msg;
		int type;
	} output;
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
%type <output> Expression UnaryExpr PrimaryExpr Operand Literal unary_op

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

Type
	: TypeName { $$ = $1; } | ArrayType
;

TypeName
	: INT { $$ = strdup("int32"); } | FLOAT { $$ = strdup("float32"); } | BOOL { $$ = strdup("bool"); } | STRING { $$ = strdup("string"); } 
;

ArrayType
	: '[' Expression ']' Type
;

Expression
	: UnaryExpr { $$ = $1; } 
	| Expression LOR Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LOR\n")); $$.type = BOOL; }
	| Expression LAND Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LAND\n")); $$.type = BOOL; }
	| Expression EQL Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("EQL\n")); $$.type = BOOL; }
	| Expression NEQ Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("NEQ\n")); $$.type = BOOL; }
	| Expression '<' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LSR\n")); $$.type = BOOL; }
	| Expression LEQ Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LEQ\n")); $$.type = BOOL; }
	| Expression '>' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("GTR\n")); $$.type = BOOL; }
	| Expression GEQ Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("GEQ\n")); $$.type = BOOL; }
	| Expression '+' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("ADD\n")); }
	| Expression '-' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("SUB\n")); }
	| Expression '*' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("MUL\n")); }
	| Expression '/' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("QUO\n")); }
	| Expression '%' Expression { $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("REM\n")); }
;

UnaryExpr
	: PrimaryExpr { $$ = $1; } | unary_op UnaryExpr { $$.msg = dynamic_strcat(2, $2.msg, $1.msg); }
;

unary_op
	: '+' %prec POS { $$.msg = strdup("POS\n"); } | '-' %prec NEG { $$.msg = strdup("NEG\n"); } | '!' { $$.msg = strdup("NOT\n"); }
;

PrimaryExpr
	: Operand { $$ = $1; } | IndexExpr | ConversionExpr
;

Operand
	: Literal { $$ = $1; } | IDENT { $$.msg = lookup_symbol($1); if(!($$.msg)) printf("undefined variable\n"); } | '(' Expression ')' { $$ = $2; }
;

Literal
	: INT_LIT 
		{	char num[50];
			sprintf(num, "INT_LIT %d\n", $1);
			$$.msg = strdup(num); 
		} 
	| FLOAT_LIT
		{	char num[50];
			sprintf(num, "FLOAT_LIT %.6f\n", $1);
			$$.msg = strdup(num); 
		} 
	| BOOL_LIT 
		{	if($1 == true)
				$$.msg = strdup("TRUE\n");
			else $$.msg = strdup("FALSE\n");
		} 
	| STRING_LIT { $$.msg = strdup("STRING_LIT\n"); }
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
	| PrintStmt NEWLINE
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
	: Expression INC 
		{	char *str = dynamic_strcat(2, $1.msg, strdup("INC\n")); 
			printf("%s", str);
			free(str);
		}
	| Expression DEC
		{	char *str = dynamic_strcat(2, $1.msg, strdup("DEC\n")); 
			printf("%s", str);
			free(str);
		}
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
		{	char *type;
			switch($3.type)
			{
			case INT: type = "int32";
				break;
			case FLOAT: type = "float32";
				break;
			case BOOL: type = "bool";
				break;
			case STRING: type = "string";
				break;
			default: type = "error";
			}
			printf("%sPRINT %s\n", $3.msg, type); 
		}
	| PRINTLN '(' Expression ')'
		{	char *type;
			switch($3.type)
			{
			case INT: type = "int32";
				break;
			case FLOAT: type = "float32";
				break;
			case BOOL: type = "bool";
				break;
			case STRING: type = "string";
				break;
			default: type = "error";
			}
			printf("%sPRINTLN %s\n", $3.msg, type); 
		}
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

char *dynamic_strcat(int n, ...){	// remaining argument should be char * type and is dynamic allocated memory
	if(n < 2){
		printf("dynamic_strcat's arguments should be at least 2 strings\n");
		exit(1);
	}
	va_list list;
	int length = 0;
	char *current, *result;

	va_start(list, n);
	for(int i = 0; i < n; ++i){
		current = va_arg(list, char *);
		length += strlen(current);
	} length += 1;
	va_end(list);

	result = malloc(sizeof(char)*length);
	if(!result){
		printf("error malloc\n");
		exit(1);
	}

	va_start(list, n);
	current = va_arg(list, char *);
	strcpy(result, current);
	free(current);
	for(int i = 1; i < n; ++i){
		current = va_arg(list, char *);
		strcat(result, current);
		free(current);
	}
	va_end(list);
	return result;
}

int evaluate_type(int type1, int type2){
	if(type1 == FLOAT || type2 == FLOAT)
		return FLOAT;
	else if(type1 == INT || type2 == INT)
		return INT;
	else if(type1 == BOOL || type2 == BOOL)
		return BOOL;
	else return 0;
}
