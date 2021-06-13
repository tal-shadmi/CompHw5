%{
/* Declaration Section*/
#include "AST.hpp"
#define YYSTYPE AST::Node*
#include "parser.tab.hpp"
#include <stdio.h>
#include "output.hpp"
extern int yylineno;
extern char* yytext;
extern int yyleng;
extern int yylex();
%}

%x ENDPROGRAM
%option noyywrap
%option yylineno
%option nounput

digit                         [0-9]
letter                        [a-zA-Z]
whitespace                    [\t\n\r ]
escape-sequence               \\[rnt"\\]

%%

void                                            return VOID;
int                                             return INT;
byte                                            return BYTE;
b                                               return B;
bool                                            return BOOL;
and                                             return AND;
or                                              return OR;
not                                             return NOT;
true                                            return TRUE;
false                                           return FALSE;
return                                          return RETURN;
if                                              return IF;
else                                            return ELSE;
while                                           return WHILE;
break                                           return BREAK;
continue                                        return CONTINUE;
switch                                          return SWITCH;
case                                            return CASE;
default                                         return DEFAULT;
":"                                             return COLON;
";"                                             return SC;
","                                             return COMMA;
"("                                             return LPAREN;
")"                                             return RPAREN;
"{"                                             return LBRACE;
"}"                                             return RBRACE;
"="                                             return ASSIGN;
"<"|">"|"<="|">="								{ yylval = new AST::IDNode(yytext);
                                                  return RELOP; }
"=="|"!="										{ yylval = new AST::IDNode(yytext);
                                                  return EQOP; }
"*"|"/"											{ yylval = new AST::IDNode(yytext);
                                                  return BINOP_MULTIPLICATIVE; }
"+"|"-"											{ yylval = new AST::IDNode(yytext);
                                                  return BINOP_ADDITIVE; }
\/\/[^\r\n]*(\r|\n|\r\n)?                         
{letter}({letter}|{digit})*                     { yylval = new AST::IDNode(yytext);
                                                  return ID; }
0|[1-9]{digit}*                                 { yylval = new AST::LiteralNode(string(yytext), AST::Type::INT);
                                                  return NUM; }
\"([^\n\r\"\\]|{escape-sequence})+\"            { yylval = new AST::LiteralNode(string(yytext), AST::Type::STRING);
                                                  return STRING; }
{whitespace}+
.                                               { output::errorLex(yylineno);
                                                  exit(1); }
<INITIAL><<EOF>>                                { BEGIN(ENDPROGRAM); return END; }
<ENDPROGRAM><<EOF>>                             { return 0; }

%%
