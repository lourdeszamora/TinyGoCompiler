%option yylineo

%{

#include <string.h>
#include <stdlib.h>

%}

DIGIT [0-9]
LETTER [A-Za-z] | "_"
ID {LETTER}+ {DIGIT}* | {LETTER}*
SPACES \n | \r | \t
%%
{SPACES} {/* ignore */}
"break"  { cout << "break_keyword" << endl; }
"func"   { cout << "func_keyword" << endl; }
"else"   { cout << "else_keyword" << endl; }
"if"     { cout << "if_keyword" << endl; }
"continue"   { cout << "continue_keyword" << endl; }
"for"    { cout << "for_keyword" << endl; }
"import" { cout << "import_keyword"<< endl; }
"return" { cout << "return_keyword" << endl; }
"var"    { cout << "var_keyword" << endl; }
"true"   { cout << "true_keyword" << endl; }
"false"  { cout << "false_keyword" << endl; }
"int"  { cout << "int_keyword" << endl; }
"float32"  { cout << "float32" << endl; }
"false"  { cout << "false_keyword" << endl; }
"bool" { cout << "bool" << endl; }
"string"  { cout << "string_keyword" << endl; }
{ID} { cout << "ID" << endl; }
{DIGIT}+ { cout << "int_value" << endl;  }
{DIGIT}+"."{DIGIT}+ { cout << "float_value" << endl; }
"!" { cout << "!" << endl; }
";" { cout << ";" << endl; }
"," { cout << "," << endl; }
"+" { cout << "+" << endl; }
"-" { cout << "-" << endl; }
"/" { cout << "/" << endl; }
"*" { cout << "*" << endl; }
"%" { cout << "%" << endl; }
"=" { cout << "=" << endl; }
"[" {  cout << "[" << endl; }
"]" {  cout << "]" << endl; }
"{" {  cout << "{" << endl; }
"}" {  cout << "}" << endl; }
"+=" {  cout << "TK_PLUS_EQUAL" << endl; }
"-=" {  cout << "TK_MINUS_EQUAL" << endl; }
"++" {  cout << "TK_PLUS_PLUS" << endl; }
"--" {  cout << "TK_MINUS_MINUS" << endl; }
"||" {  cout << "TK_OR" << endl; }
"&&" {  cout << "TK_AND" << endl; }
"==" {  cout << "TK_EQUAL" << endl; }
"!=" {  cout << "TK_NOT_EQUAL" << endl; }
">" {  cout << ">" << endl; }
"<" {  cout << "<" << endl; }
">=" {  cout << "TK_GREATER_OR_EQUAL" << endl; }
"<=" {  cout << "TK_LESS_OR_EQUAL" << endl; }
"(" {  cout << "(" << endl" << endl; }
. { printf("caracter '%c' invalido en la linea %d\n", yytext[0], yylineno); }
%%