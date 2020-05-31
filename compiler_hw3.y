/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;
	
    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
	FILE *output;
	bool HAS_ERROR = false;

    char *yyerror (char const *s)
    {
		char *str = malloc(sizeof(char)*200);
		if(!str){
			printf("malloc failed\n");
			exit(1);
		}
        sprintf(str, "error:%d: %s\n", yylineno, s);
		HAS_ERROR = true;
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

	/* get variable by address */
	entry *get_symbol(int address);

	/* dynamic string concatenation */
	char *dynamic_strcat(int n, ...);

	/* evaluate expression's return type */
	int evaluate_type(int type1, int type2);
	
	/* change type string into type int */
	int type_atoi(char *type);

	/* change type value(int) to type string(string) */
	char *type_toString(int type);

	/* change IDENT... string to load or store instruction */
	int ident_to_instruction(char *str, char instruction_type);

	/* error function */
	char *type_mismatched(char *op, int type1, int type2);
	char *op_type_not_defined(char *op, int type);

	/* get branch tag */
	char *get_branch_label();
	int label_count = 0;
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
	struct {
		char *l0;
		char *l1;
		long int fpos;
		char *fstr;
	} branch_label;
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING
%token INC DEC
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token '(' ')' '{' '}' '"'
%token ';' ',' NEWLINE
%token PRINT PRINTLN IF ELSE FOR
%token ARRAY ARRAY_I ARRAY_F ARRAY_B ARRAY_S

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
%type <output> Expression UnaryExpr PrimaryExpr Operand Literal unary_op IndexExpr ConversionExpr assign_op Condition
%type <branch_label> IfPrefix ForClause

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
	: '[' Expression ']' Type { fprintf(output, "%s", $2.msg); free($2.msg); $$.type = ARRAY; $$.element_type = $4.type; }
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
				char *error_str = op_type_not_defined("ior", type);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("ior\n")); 
				free(error_str);
			}
			else $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("ior\n")); 
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
				char *error_str = op_type_not_defined("iand", type);
				$$.msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), strdup("iand\n")); 
				free(error_str);
			}
			else $$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("iand\n")); 
			$$.isVar = false;
		}
	| Expression EQL Expression 
		{	char *l0 = get_branch_label(), *l1 = get_branch_label(), *str = malloc(sizeof(char)*100);
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			if($1.exprType == INT)
				sprintf(str, "isub\nifeq %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			else if($1.exprType == FLOAT)
				sprintf(str, "fcmpg\nifeq %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, str); 
			$$.exprType = BOOL; 
			$$.isVar = false;
			free(l0);
			free(l1);
		}
	| Expression NEQ Expression 
		{	char *l0 = get_branch_label(), *l1 = get_branch_label(), *str = malloc(sizeof(char)*100);
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			if($1.exprType == INT)
				sprintf(str, "isub\nifne %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			else if($1.exprType == FLOAT)
				sprintf(str, "fcmpg\nifne %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, str); 
			$$.exprType = BOOL; 
			$$.isVar = false;
			free(l0);
			free(l1);
		}
	| Expression '<' Expression 
		{	char *l0 = get_branch_label(), *l1 = get_branch_label(), *str = malloc(sizeof(char)*100);
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			if($1.exprType == INT)
				sprintf(str, "isub\niflt %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			else if($1.exprType == FLOAT)
				sprintf(str, "fcmpg\niflt %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, str); 
			$$.exprType = BOOL; 
			$$.isVar = false;
			free(l0);
			free(l1);
		}
	| Expression LEQ Expression 
		{	char *l0 = get_branch_label(), *l1 = get_branch_label(), *str = malloc(sizeof(char)*100);
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			if($1.exprType == INT)
				sprintf(str, "isub\nifle %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			else if($1.exprType == FLOAT)
				sprintf(str, "fcmpg\nifle %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, str); 
			$$.exprType = BOOL; 
			$$.isVar = false;
			free(l0);
			free(l1);
		}
	| Expression '>' Expression 
		{	char *l0 = get_branch_label(), *l1 = get_branch_label(), *str = malloc(sizeof(char)*100);
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			if($1.exprType == INT)
				sprintf(str, "isub\nifgt %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			else if($1.exprType == FLOAT)
				sprintf(str, "fcmpg\nifgt %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, str); 
			$$.exprType = BOOL; 
			$$.isVar = false;
			free(l0);
			free(l1);
		}
	| Expression GEQ Expression 
		{	char *l0 = get_branch_label(), *l1 = get_branch_label(), *str = malloc(sizeof(char)*100);
			if(!str){
				printf("malloc failed\n");
				exit(1);
			}
			if($1.exprType == INT)
				sprintf(str, "isub\nifge %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			else if($1.exprType == FLOAT)
				sprintf(str, "fcmpg\nifge %s\niconst_0\ngoto %s\n%s:\niconst_1\n%s:\n", l0, l1, l0, l1);
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, str); 
			$$.exprType = BOOL; 
			$$.isVar = false;
			free(l0);
			free(l1);
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
		{	if(strcmp($1.msg, "POS\n") == 0)
				$$.msg = $2.msg;
			else if(strcmp($1.msg, "NEG\n") == 0){
				free($1.msg);
				if($2.exprType == INT)
					$$.msg = dynamic_strcat(2, $2.msg, strdup("ineg\n"));
				else if($2.exprType == FLOAT)
					$$.msg = dynamic_strcat(2, $2.msg, strdup("fneg\n"));
			}
			else if(strcmp($1.msg, "NOT\n") == 0){
				free($1.msg);
				$$.msg = dynamic_strcat(2, $2.msg, strdup("iconst_1\nixor\n")); 
			}
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
				sprintf(str, "IDENT (name=%s, address=%d)\n", variable->name, variable->address);
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
	: PrimaryExpr '[' Expression ']' 
		{	int address = ident_to_instruction($1.msg, 'a'), temp;
			$$.msg = dynamic_strcat(3, $1.msg, $3.msg, strdup("**********"));
			entry *variable = get_symbol(address);
			temp = type_atoi(variable->element_type);
			if(temp == INT)
				$$.exprType = ARRAY_I;
			else if(temp == FLOAT)
				$$.exprType = ARRAY_F;
			else if(temp == BOOL)
				$$.exprType = ARRAY_B;
			else if(temp == STRING)
				$$.exprType = ARRAY_S;
			$$.isVar = $1.isVar; 
		}
;

ConversionExpr
	: Type '(' Expression ')'
		{	char str[10], s, d;
			if($1.type == INT || $1.type == ARRAY_I)
				d = 'i';
			else if($1.type == FLOAT || $1.type == ARRAY_F)
				d = 'f';
			if($3.exprType == INT || $3.exprType == ARRAY_I)
				s = 'i';
			else if($3.exprType == FLOAT || $3.exprType == ARRAY_F)
				s = 'f';
			sprintf(str, "%c2%c\n", s, d);
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
				if($3.type == INT)
					fprintf(output, "iconst_0\n");
				else if($3.type == FLOAT)
					fprintf(output, "fconst_0\n");
				else if($3.type == STRING)
					fprintf(output, "ldc \"\"\n");
				else if($3.type == BOOL)
					fprintf(output, "iconst_0\n");
				else if($3.type == ARRAY && $3.element_type == INT)
					fprintf(output, "newarray int\n");
				else if($3.type == ARRAY && $3.element_type == FLOAT)
					fprintf(output, "newarray float\n");
				else if($3.type == ARRAY && $3.element_type == BOOL)
					fprintf(output, "newarray boolean\n");
				else if($3.type == ARRAY && $3.element_type == STRING)
					fprintf(output, "anewarray Ljava/lang/String;\n");
				insert_symbol($2, type_toString($3.type), type_toString($3.element_type)); 
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
				fprintf(output, "%s", $5.msg);
				free($5.msg);
				insert_symbol($2, type_toString($3.type), type_toString($3.element_type));
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
				fprintf(output, "%s%s%s\n", $1.msg, $3.msg, $2.msg); 
				free($1.msg);
				free($3.msg);
				free($2.msg);
			}
			else if($1.isVar != true){	// $1 is not variable
				char str[200], *type = type_toString($1.exprType), *error_str;
				sprintf(str, "cannot assign to %s", type);
				free(type);
				error_str = yyerror(str);
				fprintf(output, "%s%s%s%s\n", $1.msg, $3.msg, error_str, $2.msg);
				free(error_str);
				free($1.msg);
				free($3.msg);
				free($2.msg);
			}
			else if(type == -1){	// type mismatched
				char *msg, *error_str = type_mismatched($2.msg, $1.exprType, $3.exprType);
				msg = dynamic_strcat(4, $1.msg, $3.msg, yyerror(error_str), $2.msg);
				fprintf(output, "%s\n", msg); 
				free(error_str);
				free(msg);
			}
			else{
				ident_to_instruction($3.msg, 'l');
				if(strcmp($2.msg, "ASSIGN") == 0){
					ident_to_instruction($1.msg, 's');
					if($1.exprType == ARRAY_I)
						fprintf(output, "%s%s%s", $1.msg, $3.msg, "iastore\n");
					else if($1.exprType == ARRAY_F)
						fprintf(output, "%s%s%s", $1.msg, $3.msg, "fastore\n");
					else if($1.exprType == ARRAY_B)
						fprintf(output, "%s%s%s", $1.msg, $3.msg, "bastore\n");
					else if($1.exprType == ARRAY_S)
						fprintf(output, "%s%s%s", $1.msg, $3.msg, "aastore\n");
					else
						fprintf(output, "%s%s", $3.msg, $1.msg);	
				}
				else{
					int address = ident_to_instruction($1.msg, 's');
					if(strcmp($2.msg, "ADD_ASSIGN") == 0){
						if($1.exprType == ARRAY_I)
							fprintf(output, "%s%siaload\n%siadd\niastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == ARRAY_F)
							fprintf(output, "%s%sfaload\n%sfadd\nfastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == INT)
							fprintf(output, "iload %d\n%siadd\n%s", address, $3.msg, $1.msg);	
						else if($1.exprType == FLOAT)
							fprintf(output, "fload %d\n%sfadd\n%s", address, $3.msg, $1.msg);	
					}
					else if(strcmp($2.msg, "SUB_ASSIGN") == 0){
						if($1.exprType == ARRAY_I)
							fprintf(output, "%s%siaload\n%sisub\niastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == ARRAY_F)
							fprintf(output, "%s%sfaload\n%sfsub\nfastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == INT)
							fprintf(output, "iload %d\n%sisub\n%s", address, $3.msg, $1.msg);	
						else if($1.exprType == FLOAT)
							fprintf(output, "fload %d\n%sfsub\n%s", address, $3.msg, $1.msg);	
					}
					else if(strcmp($2.msg, "MUL_ASSIGN") == 0){
						if($1.exprType == ARRAY_I)
							fprintf(output, "%s%siaload\n%simul\niastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == ARRAY_F)
							fprintf(output, "%s%sfaload\n%sfmul\nfastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == INT)
							fprintf(output, "iload %d\n%simul\n%s", address, $3.msg, $1.msg);	
						else if($1.exprType == FLOAT)
							fprintf(output, "fload %d\n%sfmul\n%s", address, $3.msg, $1.msg);	
					}
					else if(strcmp($2.msg, "QUO_ASSIGN") == 0){
						if($1.exprType == ARRAY_I)
							fprintf(output, "%s%siaload\n%sidiv\niastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == ARRAY_F)
							fprintf(output, "%s%sfaload\n%sfdiv\nfastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == INT)
							fprintf(output, "iload %d\n%sidiv\n%s", address, $3.msg, $1.msg);	
						else if($1.exprType == FLOAT)
							fprintf(output, "fload %d\n%sfdiv\n%s", address, $3.msg, $1.msg);	
					}
					if(strcmp($2.msg, "REM_ASSIGN") == 0){
						if($1.exprType == ARRAY_I)
							fprintf(output, "%s%siaload\n%sirem\niastore %d\n", $1.msg, $1.msg, $3.msg, address);
						else if($1.exprType == INT)
							fprintf(output, "iload %d\n%sirem\n%s", address, $3.msg, $1.msg);	
					}
				}
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
	: Expression { fprintf(output, "%s", $1.msg); free($1.msg); }
;

IncDecStmt
	: Expression INC 
		{	ident_to_instruction($1.msg, 'l');
			int address;
			char type;
			if(sscanf($1.msg, "%cload %d\n", &type, &address)){
				if(type == 'i')
					fprintf(output, "%sldc 1\niadd\nistore %d\n", $1.msg, address);
				else if(type == 'f')
					fprintf(output, "%sldc 1.0\nfadd\nfstore %d\n", $1.msg, address);
				free($1.msg);
			}
		}
	| Expression DEC
		{	ident_to_instruction($1.msg, 'l');
			int address;
			char type;
			if(sscanf($1.msg, "%cload %d\n", &type, &address)){
				if(type == 'i')
					fprintf(output, "%sldc 1\nisub\nistore %d\n", $1.msg, address);
				else if(type == 'f')
					fprintf(output, "%sldc 1.0\nfsub\nfstore %d\n", $1.msg, address);
				free($1.msg);
			}
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
	: IfPrefix Block 
		{	fprintf(output, "%s:\n", $1.l0);
			free($1.l0);
			free($1.l1);
		}
	| IfPrefix Block { fprintf(output, "goto %s\n%s:\n", $1.l1, $1.l0); } ELSE ElsePrefix { fprintf(output, "%s:\n", $1.l1); free($1.l0); free($1.l1); }
;

IfPrefix
	: IF Condition 
		{	fprintf(output, "%s", $2.msg);
			free($2.msg);
			$$.l0 = get_branch_label();
			$$.l1 = get_branch_label();
			fprintf(output, "ifeq %s\n", $$.l0);
		}
;

ElsePrefix
	: IfStmt
	| Block
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
			else {
				ident_to_instruction($1.msg, 'l');
				$$.msg = $1.msg;
			}
		}
;

ForStmt
	: FOR Condition 
		{	$<branch_label>$.l0 = get_branch_label();
			$<branch_label>$.l1 = get_branch_label();
			fprintf(output, "%s:\n%sifeq %s\n", $<branch_label>$.l0, $2.msg, $<branch_label>$.l1);
		} Block 
		{	fprintf(output, "goto %s\n%s:\n", $<branch_label>3.l0, $<branch_label>3.l1);
			free($<branch_label>3.l0);
			free($<branch_label>3.l1);
			free($2.msg);
		}
	| FOR ForClause { fseek(output, $2.fpos, SEEK_SET); } Block
		{	fprintf(output, "%sgoto %s\n%s:\n", $2.fstr, $2.l0, $2.l1);
			free($2.l0);
			free($2.l1);
			free($2.fstr);
		}
;

ForClause
	: SimpleStmt ';' Condition 
		{	$<branch_label>$.l0 = get_branch_label();
			$<branch_label>$.l1 = get_branch_label();
			fprintf(output, "%s:\n%sifeq %s\n", $<branch_label>$.l0, $3.msg, $<branch_label>$.l1);
			$<branch_label>$.fpos = ftell(output);
			free($3.msg);
		} ';' SimpleStmt 
		{	$$ = $<branch_label>4; 
			fseek(output, $$.fpos, SEEK_SET);
			$$.fstr = malloc(sizeof(char)*200);
			if(!$$.fstr){
				printf("malloc failed\n");
				exit(1);
			}
			//fgets($$.fstr, sizeof($$.fstr), output);
			char ch;
			int i = 0;
			while((ch = fgetc(output)) != EOF && i < 200){
				$$.fstr[i] = ch;
				i++;
			} $$.fstr[i] = '\0';
		}
;

PrintStmt
	: PRINT '(' Expression ')' 
		{
			ident_to_instruction($3.msg, 'l');
			fprintf(output, "%s", $3.msg);
			if($3.exprType == INT || $3.exprType == ARRAY_I)
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(I)V\n");
			else if($3.exprType == FLOAT || $3.exprType == ARRAY_F)
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(F)V\n");
			else if($3.exprType == STRING || $3.exprType == ARRAY_S)
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
			else if($3.exprType == BOOL || $3.exprType == ARRAY_B){
				char *l0 = get_branch_label(), *l1 = get_branch_label();
				fprintf(output, "ifne %s\nldc \"false\"\ngoto %s\n%s:\nldc \"true\"\n%s:\n", l0, l1, l0, l1);
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
				free(l0);
				free(l1);
			}
			free($3.msg);
		}
	| PRINTLN '(' Expression ')'
		{
			ident_to_instruction($3.msg, 'l');
			fprintf(output, "%s", $3.msg);
			if($3.exprType == INT || $3.exprType == ARRAY_I)
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(I)V\n");
			else if($3.exprType == FLOAT || $3.exprType == ARRAY_F)
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(F)V\n");
			else if($3.exprType == STRING || $3.exprType == ARRAY_S)
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
			else if($3.exprType == BOOL || $3.exprType == ARRAY_B){
				char *l0 = get_branch_label(), *l1 = get_branch_label();
				fprintf(output, "ifne %s\nldc \"false\"\ngoto %s\n%s:\nldc \"true\"\n%s:\n", l0, l1, l0, l1);
				fprintf(output, "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
				free(l0);
				free(l1);
			}
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
	output = fopen("hw3.j", "w+");
	symbol_table[0] = NULL;

	fprintf(output, ".source hw3.j\n.class public Main\n.super java/lang/Object\n.method public static main([Ljava/lang/String;)V\n.limit stack 100\n.limit locals 100\n");

    yylineno = 0;
    yyparse();

	dump_symbol();
	printf("Total lines: %d\n", yylineno);
	fprintf(output, "return\n.end method\n");
    fclose(yyin);
	fclose(output);
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
	if(strcmp(type, "int32") == 0)
    	fprintf(output, "istore %d\n", ptr->address);
	else if(strcmp(type, "float32") == 0)
    	fprintf(output, "fstore %d\n", ptr->address);
	else if(strcmp(type, "bool") == 0)
    	fprintf(output, "istore %d\n", ptr->address);
	else if(strcmp(type, "string") == 0)
    	fprintf(output, "astore %d\n", ptr->address);
	else if(strcmp(type, "array") == 0)
    	fprintf(output, "astore %d\n", ptr->address);
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

entry *get_symbol(int address){
	for(int i = current_scope; i >= 0; --i){
		for(entry *ptr = symbol_table[i]; ptr != NULL; ptr = ptr->next){
			if(ptr->address == address){
				return ptr;
			}
		}
	}
	printf("variable with address %d not exist\n", address);
	exit(1);
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
	if((type1 == FLOAT || type1 == ARRAY_F) && (type2 == FLOAT || type2 == ARRAY_F))
		return FLOAT;
	else if((type1 == INT || type1 == ARRAY_I) && (type2 == INT || type2 == ARRAY_I))
		return INT;
	else if((type1 == BOOL || type1 == ARRAY_B) && (type2 == BOOL || type2 == ARRAY_B))
		return BOOL;
	else if((type1 == STRING || type1 == ARRAY_S) && (type2 == STRING || type2 == ARRAY_S))
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
	else return ARRAY;
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

int ident_to_instruction(char *str, char instruction_type){	// instruction_type can be either l, s or a
	char *result = strstr(str, "IDENT"), *temp;
	int len, address;
	while(result != NULL){
		char name[100];
		sscanf(result, "IDENT (name=%[^,], address=%d)\n%n", name, &address, &len);
		temp = result + len;	// point to the remaining string
		temp = strdup(temp);
		entry *ptr = get_symbol(address);
		char *type = ptr->type;
		if(instruction_type == 'l'){
			if(strcmp(type, "int32") == 0)
				sprintf(result, "%s %d\n%n%s", "iload", address, &len, temp);	// change IDENT... to instruction
			else if(strcmp(type, "float32") == 0)
				sprintf(result, "%s %d\n%n%s", "fload", address, &len, temp);
			else if(strcmp(type, "bool") == 0)
				sprintf(result, "%s %d\n%n%s", "iload", address, &len, temp);
			else if(strcmp(type, "string") == 0)
				sprintf(result, "%s %d\n%n%s", "aload", address, &len, temp);
			else if(strcmp(type, "array") == 0){
				char *aster_str = strstr(temp, "**********"), *remain = strdup(aster_str + 10);
				int element_type = type_atoi(ptr->element_type);
				if(element_type == INT)
					sprintf(aster_str, "iaload\n%s", remain);
				else if(element_type == FLOAT)
					sprintf(aster_str, "faload\n%s", remain);
				else if(element_type == BOOL)
					sprintf(aster_str, "baload\n%s", remain);
				else if(element_type == STRING)
					sprintf(aster_str, "aaload\n%s", remain);
				sprintf(result, "%s %d\n%n%s", "aload", address, &len, temp);
			}
		}
		else if(instruction_type == 's'){
			if(strcmp(type, "int32") == 0)
				sprintf(result, "%s %d\n%n%s", "istore", address, &len, temp);	// change IDENT... to instruction
			else if(strcmp(type, "float32") == 0)
				sprintf(result, "%s %d\n%n%s", "fstore", address, &len, temp);
			else if(strcmp(type, "bool") == 0)
				sprintf(result, "%s %d\n%n%s", "istore", address, &len, temp);
			else if(strcmp(type, "string") == 0)
				sprintf(result, "%s %d\n%n%s", "astore", address, &len, temp);
			else if(strcmp(type, "array") == 0){
				char *aster_str = strstr(temp, "**********"), *remain = strdup(aster_str + 10);
				sprintf(aster_str, "%s", remain);
				sprintf(result, "%s %d\n%n%s", "aload", address, &len, temp);
			}
		}
		else if(instruction_type == 'a'){	// do no make changes, only return array variable address
			if(strcmp(type, "array") == 0)
				return address;
			else return -1;
		}
		free(temp);
		result = result + len;
		result = strstr(result, "IDENT");
	}
	return address;		// return last match variable's address
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

char *get_branch_label(){
	char *str = malloc(sizeof(char)*50);
	if(!str){
		printf("malloc failed\n");
		exit(1);
	}
	sprintf(str, "L_cmp_%d", label_count++);
	return str;
}
