%code requires{
    #include "ast.h"
}

%{

    #include <cstdio>
    using namespace std;
    int yylex();
    extern int yylineno;
    void yyerror(const char * s){
        fprintf(stderr, "Line: %d, error: %s\n", yylineno, s);
    }

    #define YYERROR_VERBOSE 1
%}

/*%union{
    const char * string_t;
    float float_t;
    int int_t;
    Expr * expr_t;
    StatementList * statement_list_t;
    Statement * statement_t;
    ParameterList * parameter_list_t;
}*/

%token BREAK MAIN PRINTLN TK_OR TK_AND EOL PACKAGE DOSPUNTOSIGUAL FUNC TK_EQUAL TK_NOT_EQUAL TK_LESS_OR_EQUAL TK_GREATER_OR_EQUAL TK_MINUS_MINUS TK_PLUS_PLUS TK_PLUS_EQUAL TK_MINUS_EQUAL ELSE IF CONTINUE FOR STRING_LIT IMPORT RETN VAR TRUE FALSE INT FLOAT BOOL STRING
%token TK_ID
%token  TK_LIT_FLOAT
%token TK_LIT_INT
/*%type <statement_t> while_stmt external_stmt method_decl statement method_invoc variable_decl assign_stmt
%type<expr_t> term factor rel_expr expr_stmt 
%type<statement_list_t> statements input
%type<parameter_list_t> param_list arg_list*/

%%
start: input/*{
    list<Statement *>::iterator it = $1->begin();
    while(it != $1->end()){
        (*it)->printResult();
        it++;
    }
}*/;

input: packs imports func_list;

packs: PACKAGE func_id spaces;

imports: IMPORT spec spaces;

spec: TK_ID 
    | '(' spaces TK_ID  ';' spaces ')' 
    | '(' spaces STRING_LIT spaces ')' 
    | STRING_LIT 
    ;
spaces: EOL spaces
    | EOL;

func_list: func_list spaces func 
        | func 
        ;

func: FUNC func_id '(' args ')' block;

func_id: MAIN | TK_ID;

args: args ',' arg 
    | arg
    | {/**/}
    ;

arg: TK_ID type
    | TK_ID '[' TK_LIT_INT ']' type;

type: INT | STRING | FLOAT | BOOL;

statements: statements statement spaces
            |  spaces {/**/}
            ;

statement:  decl
        |   if_stmt
        |   return_stmt
        |   break_stmt
        |   continue_stmt
        |   for_stmt
        |   expr_stmt
        |   inc_stmt
        |   dec_stmt
        |   assign_stmt
        |   print_stmt
        ;

print_stmt: PRINTLN '(' value_list ')' ;

decl: var_decl
    | array_decl
    | array_special_decl
    | special_decl
    | asig_decl
    ;

array_special_decl: TK_ID DOSPUNTOSIGUAL '[' TK_LIT_INT ']' type '{' value_list '}';

array_decl: VAR TK_ID '[' TK_LIT_INT ']' type assig_array;


assig_array: '=' '{' value_list '}'
            | {/* empty */}
            ;

value_list: value_list ',' value
    | value
    ;

var_decl: VAR id_list type ass_var;

id_list: id_list ',' TK_ID
        | TK_ID
        ;

assig_list: assig_list ',' value
        | value
        ;

ass_var: '=' assig_list
        | {/* empty */}
        ;

special_decl: TK_ID DOSPUNTOSIGUAL value ;

asig_decl: VAR id_list '=' assig_list;

value: STRING_LIT // preguntar a lou si un value puede ser un TK_ID 
    | expr 
    ;

if_stmt: IF rel_expr block
    | IF rel_expr block ELSE block
    ;

block: '{' statements'}'; /* cosa*/

return_stmt: RETN val_return;

val_return: {/*empty*/}
        | value
        ;
 
break_stmt: BREAK label_empty;

label_empty: TK_ID 
            | {/* empty */}
            ;
            
continue_stmt: CONTINUE label_empty;


for_stmt: FOR partI_for ';' rel_expr ';' post_part_for block
        | FOR rel_expr block
        | FOR block  // EVALUAR SEMANTICO AQUI QUE LLEVE BREAK
        ;
    
partI_for: assign_stmt 
    | special_decl
    ;

post_part_for: inc_stmt 
            | dec_stmt
            | special_expr
            ;

expr_stmt: expr    ;

inc_stmt: TK_PLUS_PLUS TK_ID
    | TK_ID TK_PLUS_PLUS
    ;

dec_stmt: TK_MINUS_MINUS TK_ID
    | TK_ID TK_MINUS_MINUS
    ;


special_expr:TK_ID TK_PLUS_EQUAL value
    | TK_ID TK_MINUS_EQUAL value
    ;

assign_stmt: TK_ID '=' value ;


expr:  expr '+' factor 
    | expr '-' factor 
    | factor 
    ;

factor: factor '*' term 
    | factor '/' term 
    | factor '%' term
    | term 
    ;

term: TK_LIT_FLOAT 
    | TK_ID 
    | TK_LIT_INT
    | TK_ID '[' TK_LIT_INT ']'
    ;


/*rel_expr: rel_expr*/

rel_expr: expr '>' expr
        | expr '<' expr
        | expr TK_GREATER_OR_EQUAL expr
        | expr TK_LESS_OR_EQUAL expr
        | expr TK_EQUAL expr
        | expr TK_NOT_EQUAL expr
        | '!' expr
        | TRUE
        | FALSE
        ;
