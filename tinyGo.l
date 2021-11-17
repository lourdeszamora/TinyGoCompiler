%option yylineno
%option noyywrap

%{

#include <string.h>
#include <stdlib.h>
#include <iostream>
#include "main.cpp"

using namespace std;

%}

DIGIT [0-9]
LETTER [A-Za-z] | "_"
%%
[\t \r \n] {/* ignore */}
"break"  { cout << "break_keyword" << endl; }
"package"  { cout << "package_keyword" << endl; }
"func"  { cout << "func_keyword" << endl; }
"main"   { cout << "main_keyword" << endl; }
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
"float32"  { cout << "float32_keyword" << endl; }
"fmt.Println" { cout << "fmt.Println" << endl; }
"bool" { cout << "bool_keyword" << endl; }
"string"  { cout << "string_keyword" << endl; }
([a-zA-Z]|_)({DIGIT}|[a-zA-Z_0-9]|_)* { cout << "ID "<< yytext << endl; }
"//".*\n {cout << "Coment_Line" << endl;}
"/*"[^/]"*/" {cout << "Coment_BLOCK" << endl;}
\"([^\\\"]|\\.)*\" {cout << "Strig_Value "<< yytext << endl;}
{DIGIT}+ { cout << "int_value " << yytext << endl;  }
{DIGIT}+"."{DIGIT}+ { cout << "float_value "<< yytext << endl; }
"." { cout << "." << endl; }
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
":=" { cout << ":=" << endl; }
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
"(" {  cout << "(" << endl; }
")" {  cout << ")" << endl; }
. { printf("caracter '%c' invalido en la linea %d\n", yytext[0], yylineno);}
%%