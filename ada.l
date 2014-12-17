%{ 
   /*
   Regan McCooey
   9/24/14
   Lexer for Ada
   CSCI 364
   Creates a lexer for the language Ada
  */

   #include "y.tab.h"
   int lineno;
   char linebuff[ 500 ];
%}

digits [0-9]
alpha [a-zA-Z]
alphaDig [a-zA-Z0-9]

%%
[ \t]
\n.*                              { lineno++; 
                                  strcpy( linebuff, yytext + 1);
                                  yyless(1);
                                }
"--"[^\n]*                            ;
"end if"                        { return ENDIF; }
"end record"                    { return ENDREC; }
"end loop"                      { return ENDLOOP; }
"exit when"                     { return EXITWHEN; }
"is"                            { return IS; }
"of"                            { return OF; }
"begin"                         { return BEG; }
"end"                           { return END; }
".."                            { return DOTDOT; }
"'"                             { return TICK; }
"procedure"                     { return PROCEDURE; } 
"type"                          { return TYPE; } 
"array"                         { return ARRAY; } 
"record"                        { return RECORD; }
"in"                            { return IN; } 
"out"                           { return OUT; } 
"range"                         { return RANGE; }
"constant"                      { return CONST; } 
":="                            { return ASSIGN; }
"exception"                     { return EXP; }
"null"                          { return NULLWORD; }
"loop"                          { return LOOP; }
"if"                            { return IF; }
"then"                          { return THEN; }
"elsif"                        { return ELSEIF; }
"else"                          { return ELSE; } 
"exit"                          { return EXIT; }
"when"                          { return WHEN; }
"and"                           { return AND; }
"or"                            { return OR; }
"not"                           { return NOT; }
"="                             { return EQ; }
"/="                            { return NEQ; }
">="                            { return GTE; }
"<="                            { return LTE; }
">"                             { return GT; }
"<"                             { return LT; }
"**"                            { return EXPONENT; }
"raise"                         { return RAISE; }
"others"                        { return OTHERS; }
"=>"                            { return ARROW; }
"case"                          { return CASE; }
{alpha}("_"*{alphaDig}*)*       { 
                                  yylval.var = malloc( sizeof( yytext ) );
                                  strcpy( yylval.var, yytext ); 
                                  return ID; 
                                }
{digits}+                       { 
                                  yylval.integer = atoi( yytext );
                                  return NUMBER; 
                                } 
.                               { return yytext[ 0 ]; }
%%

#include <stdio.h>
#include <string.h>
void yyerror( char *s )
{
   printf( "%d: %s at %s in this line: \n%s\n", 
           lineno, s, yytext, linebuff);
}

