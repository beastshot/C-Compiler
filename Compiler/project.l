%x C_COMMENT

%{
#include<stdio.h>
#include "y.tab.h"
%}

%%
var {return VAR;}
bool {return BOOL;}
char {return CHAR;}
int {return INT;}
real {return REAL;}
string {return STRING;}
int\* {return INTPTR;}
char\* {return CHARPTR;}
real\* {return REALPTR;}
if {return IF;}
else {return ELSE;}
while {return WHILE;}
func {return FUNC;}
proc {return PROC;}
null {return NULLNULL;}
Main {yylval.string= strdup(yytext);return MAIN;}
"&&" {return AND;}
"/" {return DIV;}
"=" {return ASSUME;}
"==" {return EQUAL;}
">" {return BIGGER;}
">=" {return BIGGEREQ;}
"<" {return LESS;}
"<=" {return LESSEQ;}
"-" {return MINUS;}
"!" {return NOT;}
"!=" {return NOTEQ;}
"||" {return OR;}
"+" {return PLUS;}
"*" {return MULTI;}
"&" {return ADDRESS;}
"^" {return DEREFRENCE;}
"|" {return PIPE;}
";" {return SEMICOLON;}
":" {return COLON;}
"," {return COMMA;}
"{" {return STARTBLOCK;}
"}" {return ENDBLOCK;}
"(" {return STARTPAREN;}
")" {return ENDPAREN;}
"[" {return FIRSTINDEX;}
"]" {return LASTINDEX;}
"/%"            { BEGIN(C_COMMENT); }
<C_COMMENT>"%/" { BEGIN(INITIAL); }
<C_COMMENT>\n   yylineno++;
<C_COMMENT>.   ;

return {return RETURN;}
true {yylval.string= strdup(yytext);return B_TRUE;}
false {yylval.string= strdup(yytext);return B_FALSE;}
[a-zA-Z][a-zA-Z0-9_]* { yylval.string= strdup(yytext);return ID;}
"\""[^"]*"\"" {yylval.string= strdup(yytext);return STRINGPAR;}
\'.\' {yylval.string= strdup(yytext);return CHARPAR;}
[+-]?[1-9][0-9]*|[0-9] {yylval.string= strdup(yytext);return DECIMALPAR;}
0(x|X)[0-9A-F]+ {yylval.string= strdup(yytext);return HEXPAR;}
[-+]?[0-9]*[\.]?[0-9]*[eE]?[-+]?[0-9]+ {yylval.string= strdup(yytext);return REALPAR;}
	
[\n] {yylineno++;}
[ \t]+ ;
. return yytext[0];
%%

