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

%union{
    const char * string_t;
    float float_t;
    int int_t;
    Expr * expr_t;
    StatementList * statement_list_t;
    Statement * statement_t;
    ParameterList * parameter_list_t;
}

%token BREAK PACKAGE DOSPUNTOSIGUAL FUNC MAIN ELSE IF CONTINUE FOR STRING_LIT IMPORT _RETURN VAR TRUE FALSE INT FLOAT PRINTLN BOOL STRING
%token<string_t> TK_ID
%token<float_t>  TK_LIT_FLOAT
%token<int_t> TK_LIT_INT
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

packs: PACKAGE TK_ID;

imports: IMPORT spec

spec: TK_ID
    | '(' TK_ID ';' ')'
    | '(' STRING_LIT ')'
    ;

func_list: func_list func
        | func
        ;

func: FUNC TK_ID '(' args ')' '{' statements '}' ;

args: args ',' arg
    | arg
    ;

arg: TK_ID type;

type: INT | STRING | FLOAT | BOOL;

statements: decl
        |   if_stmt
        |   return_stmt
        |   break_stmt
        |   continue_stmt
        |   for_stmt
        |   expr_stmt
        |   inc_stmt
        |   dec_stmt
        |   asig_stmt
        ;
decl: var_decl
    | special_decl
    | asig_decl
    ;

var_decl: VAR TK_ID type ass_var;

ass_var: '=' value
        | {/* empty */}
        ;

special_decl: TK_ID DOSPUNTOSIGUAL value


value: STRING_LIT 
    | TRUE 
    | FALSE 
    | TK_LIT_INT
    | TK_LIT_FLOAT
    | expr 
    ;