/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    char *yyerror (char const *s)
    {
		char *str = malloc(sizeof(char)*200);
		if(!str){
			printf("malloc failed\n");
			exit(1);
		}
        sprintf(str, "error:%d: %s\n", yylineno, s);
		return str;
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

	/* error function */
	char *type_mismatched(char *op, int type1, int type2);
	char *op_type_not_defined(char *op, int type);
	bool HAS_ERROR = false;
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
		bool isVar;
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
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType != BOOL){
				int type;
				if($1.exprType == FLOAT || $3.exprType == FLOAT)
					type = FLOAT;
				else if($1.exprType == INT || $3.exprType == INT)
					type = INT;
				else if($1.exprType == STRING || $3.exprType == STRING)
					type = STRING;
				char *error_str = op_type_not_defined("LOR", type);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("LOR\n")); 
				free(error_str);
			}
			else $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LOR\n")); 
			$$.isVar = false;
		}
	| Expression LAND Expression 
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType != BOOL){
				int type;
				if($1.exprType == FLOAT || $3.exprType == FLOAT)
					type = FLOAT;
				else if($1.exprType == INT || $3.exprType == INT)
					type = INT;
				else if($1.exprType == STRING || $3.exprType == STRING)
					type = STRING;
				char *error_str = op_type_not_defined("LAND", type);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("LAND\n")); 
				free(error_str);
			}
			else $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LAND\n")); 
			$$.isVar = false;
		}
	| Expression EQL Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("EQL\n")); 
			$$.exprType = BOOL; 
			$$.isVar = false;
		}
	| Expression NEQ Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("NEQ\n")); 
			$$.exprType = BOOL; 
			$$.isVar = false;
		}
	| Expression '<' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LSS\n")); 
			$$.exprType = BOOL; 
			$$.isVar = false;
		}
	| Expression LEQ Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("LEQ\n")); 
			$$.exprType = BOOL; 
			$$.isVar = false;
		}
	| Expression '>' Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("GTR\n")); 
			$$.exprType = BOOL; 
			$$.isVar = false;
		}
	| Expression GEQ Expression 
		{	$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("GEQ\n")); 
			$$.exprType = BOOL; 
			$$.isVar = false;
		}
	| Expression '+' Expression 
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType == -1){
				char *error_str = type_mismatched("ADD", $1.exprType, $3.exprType);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("ADD\n")); 
				free(error_str);
			}
			else{
				if($$.exprType == INT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("iadd\n")); 
				else if($$.exprType == FLOAT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("fadd\n")); 
			}
			$$.isVar = false;
		}
	| Expression '-' Expression 
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType == -1){
				char *error_str = type_mismatched("SUB", $1.exprType, $3.exprType);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("SUB\n")); 
				free(error_str);
			}
			else{
				if($$.exprType == INT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("isub\n")); 
				else if($$.exprType == FLOAT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("fsub\n")); 
			}
			$$.isVar = false;
		}
	| Expression '*' Expression 
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType == -1){
				char *error_str = type_mismatched("MUL", $1.exprType, $3.exprType);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("MUL\n")); 
				free(error_str);
			}
			else{
				if($$.exprType == INT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("imul\n"));
				else if($$.exprType == FLOAT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("fmul\n"));
			}
			$$.isVar = false;
		}
	| Expression '/' Expression 
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType == -1){
				char *error_str = type_mismatched("QUO", $1.exprType, $3.exprType);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("QUO\n")); 
				free(error_str);
			}
			else{
				if($$.exprType == INT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("idiv\n")); 
				else if($$.exprType == FLOAT)
					$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("fdiv\n")); 
			}
			$$.isVar = false;
		}
	| Expression '%' Expression 
		{	$$.exprType = evaluate_type($1.exprType, $3.exprType);
			if($$.exprType != INT){		// error
				int type;
				if($1.exprType == FLOAT || $3.exprType == FLOAT)
					type = FLOAT;
				else if($1.exprType == BOOL || $3.exprType == BOOL)
					type = BOOL;
				else if($1.exprType == STRING || $3.exprType == STRING)
					type = STRING;
				char *error_str = op_type_not_defined("REM", type);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("REM\n")); 
				free(error_str);
			}
			else $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("irem\n")); 
			$$.isVar = false;
		}
;

UnaryExpr
	: PrimaryExpr { $$ = $1; } 
	| unary_op UnaryExpr 
		{	$$.msg = dynamic_strcat(2, $2.msg, $1.msg); 
			$$.exprType = $2.exprType; 
			$$.isVar = false; 
		}
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
			if(!variable){	// variable undefine
				char str[200], *error_str;
				sprintf(str, "undefined: %s", $1);
				yylineno++;
				error_str = yyerror(str);
				yylineno--;
				$$.msg = error_str;
				$$.exprType = -1;
			}
			else{
				char *str = malloc(100*sizeof(char));
				if(!str){
					printf("malloc failed\n");
					exit(1);
				}
				$$.exprType = type_atoi(variable->type);
				if($$.exprType == INT)
					sprintf(str, "iload %d\n", variable->address);
				else if($$.exprType == FLOAT)
					sprintf(str, "fload %d\n", variable->address);
				else if($$.exprType == STRING)
					sprintf(str, "aload %d\n", variable->address);
				else if($$.exprType == -1)	// ARRAY not handled yet
					$$.exprType = type_atoi(variable->element_type);
				$$.msg = str;
			}
			$$.isVar = true;
		} 
	| '(' Expression ')' { $$ = $2; }
;

Literal
	: INT_LIT 
		{	char num[50];
			sprintf(num, "ldc %d\n", $1);
			$$.msg = strdup(num); 
			$$.exprType = INT;
			$$.isVar = false;
		} 
	| FLOAT_LIT
		{	char num[50];
			sprintf(num, "ldc %.6f\n", $1);
			$$.msg = strdup(num);
			$$.exprType = FLOAT;
			$$.isVar = false;
		} 
	| BOOL_LIT 
		{	if($1 == true)
				$$.msg = strdup("iconst_1\n");
			else $$.msg = strdup("iconst_0\n");
			$$.exprType = BOOL;
			$$.isVar = false;
		} 
	| '"' STRING_LIT '"'
		{	char str[100];
			sprintf(str, "ldc \"%s\"\n", $2);
			$$.msg = strdup(str);
			$$.exprType = STRING;
			$$.isVar = false;
		} 
;

IndexExpr
	: PrimaryExpr '[' Expression ']' { $$.msg = dynamic_strcat(2, $1.msg, $3.msg); $$.exprType = $1.exprType; $$.isVar = $1.isVar; }
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
			$$.isVar = false;
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
	: VAR IDENT Type 
		{	entry *variable = NULL;
			// find if the variable is declared in current_scope			
			for(entry *ptr = symbol_table[current_scope]; ptr != NULL; ptr = ptr->next){
				if(strcmp($2, ptr->name) == 0){
					variable = ptr;
					break;
				}
			}
			// if not declared
			if(!variable){
				insert_symbol($2, type_toString($3.type), type_toString($3.element_type)); 
				if($3.type == INT)
					printf("ldc 0\nistore %d\n", current_address);
				else if($3.type == FLOAT)
					printf("ldc 0\nfstore %d\n", current_address);
				else if($3.type == STRING)
					printf("ldc \"\"\nastore %d\n", current_address);
				else if($3.type == ARRAY)
					printf("ARRAY not handled yet\n");
			}
			else{
				char str[200], *error_str;
				sprintf(str, "%s redeclared in this block. previous declaration at line %d", $2, variable->lineno);
				error_str = yyerror(str);
				printf("%s", error_str);
				free(error_str);
			}
		}
	| VAR IDENT Type '=' Expression
		{	entry *variable = NULL;
			for(entry *ptr = symbol_table[current_scope]; ptr != NULL; ptr = ptr->next){
				if(strcmp($2, ptr->name) == 0){
					variable = ptr;
					break;
				}
			}
			if(!variable){
				printf("%s", $5.msg);
				free($5.msg);
				insert_symbol($2, type_toString($3.type), type_toString($3.element_type));
				if($3.type == INT)
					printf("istore %d\n", current_address);
				else if($3.type == FLOAT)
					printf("fstore %d\n", current_address);
				else if($3.type == STRING)
					printf("astore %d\n", current_address);
				else if($3.type == ARRAY)
					printf("ARRAY not handled yet\n");
			}
			else{
				char str[200], *error_str;
				sprintf(str, "%s redeclared in this block. previous declaration at line %d", $2, variable->lineno);
				error_str = yyerror(str);
				printf("%s", error_str);
				free(error_str);
			}
		}
;

AssignmentStmt
	: Expression assign_op Expression 
		{	int type = evaluate_type($1.exprType, $3.exprType);
			if($1.exprType == -1){	// $1 undefined
				printf("%s%s%s\n", $1.msg, $3.msg, $2.msg); 
				free($1.msg);
				free($3.msg);
				free($2.msg);
			}
			else if($1.isVar != true){	// check if $1 is variable
				char str[200], *type = type_toString($1.exprType), *error_str;
				sprintf(str, "cannot assign to %s", type);
				free(type);
				error_str = yyerror(str);
				printf("%s%s%s%s\n", $1.msg, $3.msg, error_str, $2.msg);
				free(error_str);
				free($1.msg);
				free($3.msg);
				free($2.msg);
			}
			else if(type == -1){	// type mismatched
				char *msg, *error_str = type_mismatched($2.msg, $1.exprType, $3.exprType);
				msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), $2.msg);
				printf("%s\n", msg); 
				free(error_str);
				free(msg);
			}
			else{
				printf("%s%s%s\n", $1.msg, $3.msg, $2.msg); 
				free($1.msg);
				free($3.msg);
				free($2.msg);
			}
		}
;

assign_op
	: '=' { $$.msg = strdup("ASSIGN"); } 
	| ADD_ASSIGN { $$.msg = strdup("ADD_ASSIGN"); } 
	| SUB_ASSIGN { $$.msg = strdup("SUB_ASSIGN"); } 
	| MUL_ASSIGN { $$.msg = strdup("MUL_ASSIGN"); } 
	| QUO_ASSIGN { $$.msg = strdup("QUO_ASSIGN"); } 
	| REM_ASSIGN { $$.msg = strdup("REM_ASSIGN"); } 
;

ExpressionStmt
	: Expression { printf("%s", $1.msg); free($1.msg); }
;

IncDecStmt
	: Expression INC 
		{	int address;
			char type;
			if(sscanf($1.msg, "%cload %d\n", &type, &address)){
				if(type == 'i')
					printf("%sldc 1\niadd\nistore %d\n", $1.msg, address);
				else if(type == 'f')
					printf("%sldc 1.0\nfadd\nfstore %d\n", $1.msg, address);
				free($1.msg);
				/*char *str = dynamic_strcat(2, $1.msg, strdup("INC\n")); 
				printf("%s", str);
				free(str);*/
			}
		}
	| Expression DEC
		{	int address;
			char type;
			if(sscanf($1.msg, "%cload %d\n", &type, &address)){
				if(type == 'i')
					printf("%sldc 1\nisub\nistore %d\n", $1.msg, address);
				else if(type == 'f')
					printf("%sldc 1.0\nfsub\nfstore %d\n", $1.msg, address);
				free($1.msg);
				/*char *str = dynamic_strcat(2, $1.msg, strdup("DEC\n")); 
				printf("%s", str);
				free(str);*/
			}
		}
;

Block
	: '{' { current_scope++; create_symbol(); } StatementList '}' { /*dump_symbol();*/ current_scope--; }
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
		{	if($1.exprType != BOOL){
				char str[200], *type = type_toString($1.exprType), *error_str;
				sprintf(str, "non-bool (type %s) used as for condition", type);
				yylineno++;
				error_str = yyerror(str);
				yylineno--;
				printf("%s%s", $1.msg, error_str);
				free(error_str);
				free(type);
			}
			else printf("%s", $1.msg); 
			free($1.msg); 
		}
;

ForStmt
	: FOR Condition Block
	| FOR ForClause Block
;

ForClause
	: SimpleStmt ';' Condition ';' SimpleStmt
;

PrintStmt
	: PRINT '(' Expression ')' 
		{	/*char *type = type_toString($3.exprType);
			printf("%sPRINT %s\n", $3.msg, type);
			free($3.msg);
			free(type);*/
			printf("%s", $3.msg);
			if($3.exprType == INT)
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(I)V\n");
			else if($3.exprType == FLOAT)
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(F)V\n");
			else if($3.exprType == STRING)
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
			else if($3.exprType == BOOL)	// not handled yet
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
			free($3.msg);
		}
	| PRINTLN '(' Expression ')'
		{	/*char *type = type_toString($3.exprType);
			printf("%sPRINTLN %s\n", $3.msg, type);
			free($3.msg);
			free(type);*/
			printf("%s", $3.msg);
			if($3.exprType == INT)
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(I)V\n");
			else if($3.exprType == FLOAT)
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(F)V\n");
			else if($3.exprType == STRING)
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
			else if($3.exprType == BOOL)	// not handled yet
				printf("getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
			free($3.msg);
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

	printf(".source hw3.j\n.class public Main\n.super java/lang/Object\n.method public static main([Ljava/lang/String;)V\n.limit stack 100\n.limit locals 100\n");

    yylineno = 0;
    yyparse();

	//dump_symbol();
	//printf("Total lines: %d\n", yylineno);
    fclose(yyin);
	printf("return\n.end method\n");
    if (HAS_ERROR) {
        remove("hw3.j");
    }
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
    //printf("> Insert {%s} into symbol table (scope level: %d)\n", id, current_scope);
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
	else if(type1 == STRING && type2 == STRING)
		return STRING;
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
	else return -1;		// ARRAY
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

char *type_mismatched(char *op, int type1, int type2){
	char *str = malloc(sizeof(char)*200), *s_type1 = type_toString(type1), *s_type2 = type_toString(type2);
	if(!str){
		printf("malloc failed\n");
		exit(1);
	}
	sprintf(str, "invalid operation: %s (mismatched types %s and %s)", op, s_type1, s_type2);
	free(s_type1);
	free(s_type2);
	return str;
}

char *op_type_not_defined(char *op, int type){
	char *str = malloc(sizeof(char)*200), *s_type = type_toString(type);
	if(!str){
		printf("malloc failed\n");
		exit(1);
	}
	sprintf(str, "invalid operation: (operator %s not defined on %s)", op, s_type);
	free(s_type);
	return str;
}
