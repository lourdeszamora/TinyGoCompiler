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

%token BREAK PACKAGE FUNC MAIN ELSE IF CONTINUE FOR IMPORT RETURN VAR TRUE FALSE INT FLOAT PRINTLN BOOL STRING
%token<string_t> TK_ID
%token<float_t>  TK_LIT_FLOAT
%token<int_t> TK_LIT_INT
/*%type <statement_t> while_stmt external_stmt method_decl statement method_invoc variable_decl assign_stmt
%type<expr_t> term factor rel_expr expr_stmt 
%type<statement_list_t> statements input
%type<parameter_list_t> param_list arg_list*/

%%
start: input{
    list<Statement *>::iterator it = $1->begin();
    while(it != $1->end()){
        (*it)->printResult();
        it++;
    }
}

input: input external_stmt {$$ = $1; $$->push_back($2);}
    | external_stmt {$$ = new StatementList; $$->push_back($1);}
    ;

external_stmt : method_decl {$$ = $1;}
    |  variable_decl {$$ = $1;}
    |  expr_stmt {$$ = new ExprStatement($1, yylineno);}
    |  while_stmt {$$ = $1;}
    |  method_invoc {$$ = $1;}
    ;

method_decl: LET TK_ID '(' param_list ')' '=' statements { 
                    $$ = new MethodDefinition( $2, *$4, *$7, yylineno );
                    delete $4;
                    };

param_list: param_list ',' TK_ID {$$ = $1; $$->push_back(new IdExpr($3, yylineno));}
            | TK_ID  { $$ = new ParameterList; $$->push_back(new IdExpr($1, yylineno)); }
            ;

statements:  statement { $$ = new StatementList; $$->push_back($1); }
    | statements ';' statement   { $$ = $1; $$->push_back($3); }
    ;

statement: variable_decl {$$ = $1;}
    | expr_stmt {$$ = new ExprStatement($1, yylineno);}
    | while_stmt {$$ = $1;}
    | method_invoc {$$ = $1;}
    | assign_stmt { $$ = $1; }
    ;

assign_stmt: TK_ID '=' expr_stmt { $$ = new AsigStatement($1,$3, yylineno);};

method_invoc: TK_ID '(' arg_list ')' { $$ = new MethodInvocationStmt($1, *$3, yylineno); }

arg_list: arg_list ',' term {$$ = $1; $$->push_back($3);}
            | term { $$ = new ParameterList; $$->push_back($1); }
            ;

variable_decl: LET TK_ID '=' expr_stmt { $$ = new Declarator($2,$4,yylineno); }

expr_stmt:  expr_stmt ADD factor { $$ = new BinaryExpr( '+', $1, $3, yylineno); }
    | expr_stmt SUB factor { $$ = new BinaryExpr( '-', $1, $3, yylineno); }
    | factor { $$ = $1; }
    ;

factor: factor MUL term { $$ = new BinaryExpr( '*', $1, $3, yylineno); }
    | factor DIV term { $$ = new BinaryExpr( '/', $1, $3, yylineno); }
    | term { $$ = $1; }
    ;

term: TK_LIT_FLOAT {$$ = new FloatExpr($1, yylineno); }
    | TK_ID { $$ = new IdExpr($1, yylineno); }
    ;

while_stmt: WHILE '(' rel_expr ')' DO statements DONE { $$ = new WhileStatement( $3, *$6, yylineno); }

rel_expr: expr_stmt '>' expr_stmt { $$ = new BinaryExpr( '>', $1, $3, yylineno); }
        | expr_stmt '<' expr_stmt { $$ = new BinaryExpr( '<', $1, $3, yylineno); }
        ;

%%