%option reentrant noyywrap bison-bridge bison-locations
%option extra-type="struct Extra *"

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexer.h"
#include "parser.tab.h"

#define YY_USER_ACTION                                                         \
    {                                                                          \
        int i;                                                                 \
        struct Extra *extra = yyextra;                                         \
        if (!extra->continued) {                                               \
            yylloc->first_line = extra->cur_line;                              \
            yylloc->first_column = extra->cur_column;                          \
        }                                                                      \
        extra->continued = false;                                              \
        for (i = 0; i < yyleng; ++i) {                                         \
            if (yytext[i] == '\n') {                                           \
                extra->cur_line += 1;                                          \
                extra->cur_column = 1;                                         \
            } else {                                                           \
                extra->cur_column += 1;                                        \
            }                                                                  \
        }                                                                      \
        yylloc->last_line = extra->cur_line;                                   \
        yylloc->last_column = extra->cur_column;                               \
    }

void yyerror(YYLTYPE *loc, yyscan_t scanner, const char *message) {
    printf("Error (%d,%d): %s\n", loc->first_line, loc->first_column, message);
}
%}

DIGIT [0-9]
LETTER [a-zA-Z]
IDENT {LETTER}({LETTER}|{DIGIT}|[_-])*

%%

[\t \n\r]+ { /* Skip whitespace */ }

\%class   { return KW_CLASS; }
\%tokens  { return KW_TOKENS; }
\%types   { return KW_TYPES; }
\%methods { return KW_METHODS; }
\%grammar { return KW_GRAMMAR; }
\%axiom   { return KW_AXIOM; }
\%end     { return KW_END; }
\%rep     { return KW_REP; }

{IDENT}  { yylval->str = strdup(yytext); return ID; }

\,       { return COMMA; }
\:       { return COLON; }
\;       { return SEMICOLON; }
\(       { return LPAREN; }
\)       { return RPAREN; }
\[       { return LBRACKET; }
\]       { return RBRACKET; }
\=       { return ASSIGN; }
\|       { return PIPE; }
\/       { return SLASH; }

\$.*$    { printf("\n%s\n", yytext); }

.        { yyerror(yylloc, yyscanner, "Unknown character"); return -1; }

%%

void init_scanner(FILE *input, yyscan_t *scanner, struct Extra *extra) {
    extra->continued = false;
    extra->cur_line = 1;
    extra->cur_column = 1;

    yylex_init(scanner);
    yylex_init_extra(extra, scanner);
    yyset_in(input, *scanner);
}

void destroy_scanner(yyscan_t scanner) {
    yylex_destroy(scanner);
}
