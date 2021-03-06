%option yylineno
%option noyywrap

%{


#include "tokens.h"


%}
%x BLOCK_COMMENT
DIGIT [0-9]
%%
[\t \r] {/* ignore */}
\n { return EOL; }
"break"    { return BREAK; }
"package"  { return PACKAGE; }
"func"     { return FUNC; }
"main"     { return MAIN; }
"else"     { return ELSE; }
"if"       { return IF; }
"continue" { return CONTINUE; }
"for"      { return FOR; }
"import"   { return IMPORT; } 
"return"   { return RETN; }
"var"      { return VAR; }
"true"     { return TRUE; }
"false"    { return FALSE; }
"int"      { return INT; }
"float32"  { return FLOAT; }
"fmt.Println" { return PRINTLN; }
"bool"        { return BOOL; }
"string"      { return STRING; }
([a-zA-Z]|_)({DIGIT}|[a-zA-Z_0-9]|_)* { return TK_ID; }
"//".*\n {/* ignore */}
"/*" { BEGIN(BLOCK_COMMENT); }
\"([^\\\"]|\\.)*\" { return STRING_LIT; }
{DIGIT}+ { return TK_LIT_INT ; }
{DIGIT}+"."{DIGIT}+ { return TK_LIT_FLOAT ; }
"!" { return '!'; }
";" {  return ';';}
"," {  return ','; }
"+" {  return '+';}
"-" {  return '-'; }
"/" {  return '/';}
"*" {  return '*'; }
"%" {  return '%';}
"=" {  return '='; }
"[" {  return '['; }
"]" {  return ']';}
"{" {   return '{'; }
"}" {   return '}'; }
":=" {  return DOSPUNTOSIGUAL; }
"+=" {  return TK_PLUS_EQUAL ; }
"-=" {  return TK_MINUS_EQUAL ; }
"++" {  return TK_PLUS_PLUS ; }
"--" {  return TK_MINUS_MINUS ; }
"||" {  return TK_OR ; }
"&&" {  return TK_AND; }
"==" {  return TK_EQUAL ; }
"!=" {  return TK_NOT_EQUAL ; }
">" {   return '>' ; }
"<" {   return '<' ;}
">=" {  return TK_GREATER_OR_EQUAL ; }
"<=" {  return TK_LESS_OR_EQUAL ; }
"("  {  return '(' ; }
")" {  return ')'  ; }
<BLOCK_COMMENT>"*/" { BEGIN(INITIAL); }
<BLOCK_COMMENT>\n   { }
<BLOCK_COMMENT>.    { }
. { printf("caracter '%c' invalido en la linea %d\n", yytext[0], yylineno); }
%%