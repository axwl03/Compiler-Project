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
	int current_address = -1;

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
	static void insert_symbol(char *id, char *type, char *element_type);
    static entry *lookup_symbol(char *id);
    static void dump_symbol();

	/* dynamic string concatenation */
	char *dynamic_strcat(int n, ...);

	/* evaluate expression's return type */
	int evaluate_type(int type1, int type2);
	
	/* change type string into type int */
	int type_atoi(char *type);

	/* change type value(int) to type string(string) */
	char *type_toString(int type);

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
	struct {
		int type;
		int element_type;
	} type;
	char *id;
	struct {
		char *msg;
		int exprType;
	} output;
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING ARRAY
%token INC DEC
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token '(' ')' '{' '}' '"'
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
%type <output> Expression UnaryExpr PrimaryExpr Operand Literal unary_op IndexExpr ConversionExpr assign_op

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

Type
	: TypeName { $$ = $1; } | ArrayType { $$ = $1; }
;

TypeName
	: INT { $$.type = INT; $$.element_type = -1; } 
	| FLOAT { $$.type = FLOAT; $$.element_type = -1; } 
	| BOOL { $$.type = BOOL; $$.element_type = -1; } 
	| STRING { $$.type = STRING; $$.element_type = -1; } 
;

ArrayType
	: '[' Expression ']' Type { printf("%s", $2.msg); free($2.msg); $$.type = ARRAY; $$.element_type = $4.type; }
;

Expression
	: UnaryExpr { $$ = $1; } 
	| Expression LOR Expression 
		{ 	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LOR\n")); 
			$$.exprType = BOOL; 
		}
	| Expression LAND Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LAND\n")); 
			$$.exprType = BOOL; 
		}
	| Expression EQL Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("EQL\n")); 
			$$.exprType = BOOL; 
		}
	| Expression NEQ Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("NEQ\n")); 
			$$.exprType = BOOL; 
		}
	| Expression '<' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LSS\n")); 
			$$.exprType = BOOL; 
		}
	| Expression LEQ Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LEQ\n")); 
			$$.exprType = BOOL; 
		}
	| Expression '>' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("GTR\n")); 
			$$.exprType = BOOL; 
		}
	| Expression GEQ Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("GEQ\n")); 
			$$.exprType = BOOL; 
		}
	| Expression '+' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("ADD\n")); 
			$$.exprType = evaluate_type($1.exprType, $3.exprType);
		}
	| Expression '-' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("SUB\n")); 
			$$.exprType = evaluate_type($1.exprType, $3.exprType); 
		}
	| Expression '*' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("MUL\n")); 
			$$.exprType = evaluate_type($1.exprType, $3.exprType); 
		}
	| Expression '/' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("QUO\n")); 
			$$.exprType = evaluate_type($1.exprType, $3.exprType); 
		}
	| Expression '%' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("REM\n")); 
			$$.exprType = evaluate_type($1.exprType, $3.exprType); 
		}
;

UnaryExpr
	: PrimaryExpr { $$ = $1; } | unary_op UnaryExpr { $$.msg = dynamic_strcat(2, $2.msg, $1.msg); }
;

unary_op
	: '+' %prec POS { $$.msg = strdup("POS\n"); } | '-' %prec NEG { $$.msg = strdup("NEG\n"); } | '!' { $$.msg = strdup("NOT\n"); }
;

PrimaryExpr
	: Operand { $$ = $1; } | IndexExpr { $$ = $1; } | ConversionExpr { $$ = $1; }
;

Operand
	: Literal { $$ = $1; } 
	| IDENT 
		{
			entry *variable = lookup_symbol($1);
			if(!variable) printf("undefined variable\n");
			else{
				char *str = malloc(100*sizeof(char));
				if(!str){
					printf("malloc failed\n");
					exit(1);
				}
				sprintf(str, "IDENT (name=%s, address=%d)\n", variable->name, variable->address);
				$$.msg = str;
				$$.exprType = type_atoi(variable->type);
				if($$.exprType == -1)
					$$.exprType = type_atoi(variable->element_type);
			}
		} 
	| '(' Expression ')' { $$ = $2; }
;

Literal
	: INT_LIT 
		{	char num[50];
			sprintf(num, "INT_LIT %d\n", $1);
			$$.msg = strdup(num); 
			$$.exprType = INT;
		} 
	| FLOAT_LIT
		{	char num[50];
			sprintf(num, "FLOAT_LIT %.6f\n", $1);
			$$.msg = strdup(num);
			$$.exprType = FLOAT;
		} 
	| BOOL_LIT 
		{	if($1 == true)
				$$.msg = strdup("TRUE\n");
			else $$.msg = strdup("FALSE\n");
			$$.exprType = BOOL;
		} 
	| '"' STRING_LIT '"'
		{	char str[100];
			sprintf(str, "STRING_LIT %s\n", $2);
			$$.msg = strdup(str);
			$$.exprType = STRING;
		} 
;

IndexExpr
	: PrimaryExpr '[' Expression ']' { $$.msg = dynamic_strcat(2, $1.msg, $3.msg); }
;

ConversionExpr
	: Type '(' Expression ')'
		{
			char str[10], s, d;
			if($1.type == INT)
				d = 'I';
			else if($1.type == FLOAT)
				d = 'F';
			if($3.exprType == INT)
				s = 'I';
			else if($3.exprType == FLOAT)
				s = 'F';
			sprintf(str, "%c to %c\n", s, d);
			$$.exprType = $1.type;
			$$.msg = dynamic_strcat(2, $3.msg, strdup(str));
		}
;

Statement
	: DeclarationStmt NEWLINE
	| SimpleStmt NEWLINE
	| Block NEWLINE
	| IfStmt NEWLINE
	| ForStmt NEWLINE
	| PrintStmt NEWLINE
	| NEWLINE
;

SimpleStmt
	: AssignmentStmt | ExpressionStmt | IncDecStmt
;

DeclarationStmt
	: VAR IDENT Type { insert_symbol($2, type_toString($3.type), type_toString($3.element_type)); }
	| VAR IDENT Type '=' Expression { printf("%s", $5.msg); free($5.msg); insert_symbol($2, type_toString($3.type), type_toString($3.element_type)); }
;

AssignmentStmt
	: Expression assign_op Expression { printf("%s%s%s", $1.msg, $3.msg, $2.msg); free($1.msg); free($3.msg); free($2.msg); }
;

assign_op
	: '=' {$$.msg = strdup("ASSIGN\n"); } 
	| ADD_ASSIGN {$$.msg = strdup("ADD_ASSIGN\n"); } 
	| SUB_ASSIGN {$$.msg = strdup("SUB_ASSIGN\n"); } 
	| MUL_ASSIGN {$$.msg = strdup("MUL_ASSIGN\n"); } 
	| QUO_ASSIGN {$$.msg = strdup("QUO_ASSIGN\n"); } 
	| REM_ASSIGN {$$.msg = strdup("REM_ASSIGN\n"); } 
;

ExpressionStmt
	: Expression { printf("%s", $1.msg); free($1.msg); }
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
	: '{' { current_scope++; create_symbol(); } StatementList '}' { dump_symbol(); current_scope--; }
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
	: Expression { printf("%s", $1.msg); free($1.msg); }
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
		{	char *type = type_toString($3.exprType);
			printf("%sPRINT %s\n", $3.msg, type);
			free($3.msg);
			free(type);
		}
	| PRINTLN '(' Expression ')'
		{	char *type = type_toString($3.exprType);
			printf("%sPRINTLN %s\n", $3.msg, type);
			free($3.msg);
			free(type);
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

/* type and element_type must be dynamic allocated memories */
static void insert_symbol(char *id, char *type, char *element_type) {
	entry *tail = symbol_table[current_scope], *ptr;
	ptr = malloc(sizeof(entry));
	if(!ptr){
		printf("malloc failed\n");
		exit(1);
	}
	if(tail == NULL){
		ptr->index = 0;
		ptr->address = ++current_address;
		symbol_table[current_scope] = ptr;
	}
	else{
		while(tail->next != NULL) tail = tail->next;
		ptr->index = tail->index + 1;
		ptr->address = ++current_address;
		tail->next = ptr;
	}
	strcpy(ptr->name, id);
	strcpy(ptr->type, type);
	ptr->lineno = yylineno;
	strcpy(ptr->element_type, element_type);
	ptr->next = NULL;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", id, current_scope);
	free(type);
	free(element_type);
}

/* check symbol table <= current_scope */
static entry *lookup_symbol(char *id) {
	for(int i = current_scope; i >= 0; --i){
		for(entry *ptr = symbol_table[i]; ptr != NULL; ptr = ptr->next){
			if(strcmp(id, ptr->name) == 0){
				return ptr;
			}
		}
	}
	return NULL;
}

/* dump symbol and release memory for current_scope symbol table */
static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", current_scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");	
	entry *release, *ptr = symbol_table[current_scope];
	while(ptr != NULL){
	    printf("%-10d%-10s%-10s%-10d%-10d%s\n", ptr->index, ptr->name, ptr->type, ptr->address, ptr->lineno, ptr->element_type);
		release = ptr;
		ptr = ptr->next;
		free(release);
	}
	symbol_table[current_scope] = NULL;
}

/* remaining argument should be char * type and is dynamic allocated memory */
char *dynamic_strcat(int n, ...){	
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
	if(type1 == FLOAT && type2 == FLOAT)
		return FLOAT;
	else if(type1 == INT && type2 == INT)
		return INT;
	else if(type1 == BOOL && type2 == BOOL)
		return BOOL;
	else return -1;
}

int type_atoi(char *type){
	if(strcmp(type, "int32") == 0)
		return INT;
	else if(strcmp(type, "float32") == 0)
		return FLOAT;
	else if(strcmp(type, "bool") == 0)
		return BOOL;
	else if(strcmp(type, "string") == 0)
		return STRING;
	else return -1;
}

char *type_toString(int type){
	if(type == INT)
		return strdup("int32");
	else if(type == FLOAT)
		return strdup("float32");
	else if(type == BOOL)
		return strdup("bool");
	else if(type == STRING)
		return strdup("string");
	else if(type == ARRAY)
		return strdup("array");
	else if(type == -1)
		return strdup("-");
	else return NULL;
}
