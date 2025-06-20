%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define NUMBER 256
#define REG_STRING 257
#define LIT_STRING 259

struct Position {
    int line, pos, index;
};

struct Fragment {
    struct Position starting, following;
};

typedef struct Fragment YYLTYPE;

YYLTYPE yylloc;

union Token {
    char *ident;
    long num;
    char ch;
};

typedef union Token YYSTYPE;

YYSTYPE yylval;

int current_line = 1, current_column = 1, current_index = 0;

#define YY_USER_ACTION {\
    yylloc.starting.line = current_line;\
    yylloc.starting.pos = current_column;\
    yylloc.starting.index = current_index;\
    for(int i=0;yytext[i];i++){\
        current_index++;\
        if (yytext[i]=='\n') { current_line++; current_column=1; }\
        else current_column++;\
    }\
    yylloc.following.line = current_line;\
    yylloc.following.pos = current_column;\
    yylloc.following.index = current_index;\
}

void print_pos(struct Position*p){
    printf("(%d,%d)",p->line,p->pos);
}

void print_frag(struct Fragment*f){
    print_pos(&(f->starting));
    printf("-");
    print_pos(&(f->following));
}

typedef struct {
    int line;
    int column;
    char message[256];
} Error;

Error errors[100];
int error_count = 0;

void add_error(int line, int column, const char* message) {
    if (error_count < 100) {
        errors[error_count].line = line;
        errors[error_count].column = column;
        snprintf(errors[error_count].message, 256, "%s", message);
        error_count++;
    } else {
        fprintf(stderr, "Error list overflow. Increase error buffer size.\n");
    }
}

void print_errors() {
    for (int i = 0; i < error_count; i++) {
        printf("Error (%d,%d): %s\n", errors[i].line, errors[i].column, errors[i].message);
    }
}
%}

%option noyywrap
%option yylineno
%option header-file="lex.yy.h"

DIGIT [01]
LIT_STRING @\"([^\"]|\"\")*\"
REG_STRING \"([^\"\\]|\\["tn\\])*\"

%%

{REG_STRING} {
    char buf[256] = {0};
    int bi = 0;
    for (int i = 1; i < yyleng - 1 && bi < 255; i++) {
        if (yytext[i] == '\\') {
            switch (yytext[i + 1]) {
                case 'n': buf[bi++] = '\n'; i++; break;
                case 't': buf[bi++] = '\t'; i++; break;
                case '"': buf[bi++] = '"'; i++; break;
                case '\\': buf[bi++] = '\\'; i++; break;
                default: buf[bi++] = yytext[i]; break;
            }
        } else {
            buf[bi++] = yytext[i];
        }
    }
    buf[bi] = '\0';
    yylval.ident = strdup(buf);
    return REG_STRING;
}

{LIT_STRING} {
    char* content = strdup(yytext + 2);
    content[yyleng - 3] = '\0';

    char processed[256] = {0};
    int j = 0;
    for (int i = 0; content[i] && j < 255; i++) {
        if (content[i] == '"' && content[i + 1] == '"') {
            processed[j++] = '"';
            i++;
        } else {
            processed[j++] = content[i];
        }
    }
    processed[j] = '\0';

    yylval.ident = strdup(processed);
    free(content);
    return LIT_STRING;
}

{DIGIT}+ {
    uint64_t sum = 0;
    for (int i = 0; yytext[i]; i++)
        if (yytext[i] == '1') sum++;
    yylval.num = sum;
    return NUMBER;
}

[ \t\n]+ {/*skip spaces and newlines*/}

. {
    char error_message[256];
    snprintf(error_message, 256, "Invalid character '%s'", yytext);
    add_error(yylloc.starting.line, yylloc.starting.pos, error_message);
}

%%

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input-file>\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("fopen");
        return 1;
    }

    int token;
    while ((token = yylex())) {
        switch (token) {
            case NUMBER:
                printf("NUMBER "); print_frag(&yylloc);
                printf(": %ld\n", yylval.num);
                break;
            case REG_STRING:
                printf("REG_STRING "); print_frag(&yylloc);
                printf(": %s\n", yylval.ident);
                free(yylval.ident);
                break;
            case LIT_STRING:
                printf("LIT_STRING "); print_frag(&yylloc);
                printf(": %s\n", yylval.ident);
                free(yylval.ident);
                break;
            default:
                printf("UNKNOWN TOKEN "); print_frag(&yylloc); printf("\n");
        }
    }

    print_errors();

    fclose(yyin);
    return 0;
}