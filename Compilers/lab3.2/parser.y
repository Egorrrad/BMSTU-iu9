%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lexer.h"

#define INDENT_SIZE 2

void indent(const int level) {
    for (int i = 0; i < level; ++i) {
        for (int j = 0; j < INDENT_SIZE; ++j) {
            printf(" ");
        }
    }
}

%}

%define api.pure
%define locations

%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner}
%parse-param {int indent_level}

%union {
    char* str;
}

%token KW_CLASS KW_TOKENS KW_TYPES KW_METHODS KW_GRAMMAR KW_AXIOM KW_END KW_REP
%token <str> ID
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACKET RBRACKET ASSIGN PIPE SLASH

%{
int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, yyscan_t scanner);
void yyerror(YYLTYPE *loc, yyscan_t scanner, int indent_level, const char *message);
%}

%%

NSpec:
    NClassSection NTokensSection NTypesSection NMethodsSection NGrammarSection NAxiomSection KW_END {
        printf("%%end\n");
      }
    ;

NClassSection:
    KW_CLASS ID {
        printf("%%class\n");
        indent(indent_level + 1);
        printf("%s\n\n", $2);
      }
    ;

NTokensSection:
    KW_TOKENS {
        printf("%%tokens\n");
        indent(++indent_level);
      } 
      NTokensList {
      	printf("\n\n");
      	indent_level--;
      }
    ;

NTypesSection:
    KW_TYPES {
        printf("%%types\n\n");
      }
    | KW_TYPES {
        printf("%%types\n");
        indent(++indent_level);
      } 
      NTypesMapping {
      	printf("\n");
      	indent_level--;
      }
    ;

NMethodsSection:
    KW_METHODS {
        printf("%%methods\n");
      }
    | KW_METHODS {
        printf("%%methods\n");
		    indent(++indent_level);
      } 
      NMethodsList {
      	printf("\n");
      	indent_level--;
      }
    ;

NGrammarSection:
    KW_GRAMMAR {
        printf("%%grammar\n\n");
      }
    | KW_GRAMMAR {
        printf("%%grammar\n");
        indent(++indent_level);
      } 
      NGrammarRules {
      	printf("\n");
      	indent_level--;
      }
    ;

NAxiomSection:
    KW_AXIOM ID {
        printf("%%axiom\n");
        indent(++indent_level);
        printf("%s\n\n", $2);
        indent_level--;
      }
    ;

NTokensList:
    ID {
        printf("%s ", $1);
      }
    | ID {
        printf("%s ", $1);
      } 
      NTokensList
    ;

NTypesMapping:
    NTypeMapping
    | NTypesMapping NTypeMapping  
    ;

NTypeMapping:
    NIdList COLON {
        printf(" : "); 
      } 
      NTypeName SEMICOLON {
        printf(";\n");
        indent(indent_level);
      }
    ;

NTypeName:
    ID {
        printf("%s", $1);
      }
    | ID LBRACKET RBRACKET {
      	printf("%s[]", $1);
      }
    ;

NIdList:
    ID {
        printf("%s", $1);
      }
    | NIdList COMMA ID {
      	printf(", %s", $3);
      }  
    ;

NMethodsList:
    NMethod
    | NMethodsList NMethod 
    ;

NMethod:
    NTypeName ID LPAREN RPAREN SEMICOLON {
        printf(" %s()\n", $2);
        indent(indent_level);
      }
    | NTypeName ID LPAREN {
       	printf(" %s(", $2);
      } 
      NParamList RPAREN SEMICOLON {
  	    printf(");\n");
  	    indent(indent_level);
      }
    ;

NParamList:
    NTypeName 
    | NParamList COMMA {
        printf(", ");
      }
      NTypeName  
    ;

NGrammarRules:
    NGrammarRule
    | NGrammarRules {
    	indent(indent_level);
      } 
      NGrammarRule 
    ;

NGrammarRule:
    ID ASSIGN {
        printf("%s =\n", $1);
        indent_level += 2;
      }
      NAlternatives SEMICOLON {
        printf("\n");
        indent(--indent_level);
        printf(";\n\n");
        indent_level--;
      }
    ;

NAlternatives:
	  {
		indent(indent_level);
	  } 
      NRuleAlternative 
    | NAlternatives PIPE {
      	printf("\n");
      	indent(indent_level - 1);
      	printf("| ");
      }
      NRuleAlternative 
    ;

NRuleAlternative:
    NRuleElementList 
    | NRuleElementList SLASH ID {
        printf("/ %s", $3);
    }
    | SLASH ID {
        printf("/ %s", $2);
    }
    ;

NRuleElementList:
    NRuleElement
    | NRuleElementList NRuleElement 
    ;

NRuleElement:
    ID {
        printf("%s ", $1);
      }
    | LPAREN {
        printf("(\n");
        indent_level += 2;
      } 
      NAlternatives RPAREN {
        printf("\n");
        indent_level -= 2;
        indent(indent_level);
        printf(")\n");
        indent(indent_level);
      }
    | KW_REP {
        printf("\n");
        indent(indent_level);
        printf("%%rep "); 
      } 
      NRuleElement 
    ;

%%

int main(int argc, char *argv[]) {
    FILE *input = 0;
    yyscan_t scanner;
    struct Extra extra;

    if (argc > 1) {
        printf("Read file %s\n", argv[1]);
        input = fopen(argv[1], "r");
    } else {
        printf("No file in command line, use stdin\n");
        input = stdin;
    }

    init_scanner(input, &scanner, &extra);
    int indent_level = 0;
    yyparse(scanner, indent_level);
    destroy_scanner(scanner);

    if (input != stdin) {
        fclose(input);
    }

    return 0;
}
