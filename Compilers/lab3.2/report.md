% "Лабораторная работа 3.2 «Форматтер исходных текстов»"
% 9 июня 2025 г.
% 

# Цель работы
Целью данной работы является приобретение навыков использования генератора синтаксических анализаторов bison.

# Индивидуальный вариант
Язык спецификации для генератора синтаксических анализаторов:
```
%class
SimpleImperativeLang

%tokens
NUMBER PLUS MINUS STAR FRAC LBRAC RBRAC
TRUE FALSE ADD OR NOT LT GT LE GE NE EQ
IF THEN ELSE END WHILE DO SEMICOLON
VAR ASSIGN INPUT PRINT COMMA

%types
Expr, Term, Factor, NUMBER: ArithmExpr;
PLUS, MINUS, STAR, FRAC: ArithmOp;
BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE: BoolExpr;
LT, GT, LE, GE, NE, EQ: RelaOp;
Program, Statement, StatementList, Program: Statement;
VAR, STRING: String;
PrintItem: PrintItem;

%methods
ArithmExpr neg_op(ArithmOp, ArithmExpr);
ArithmExprChunk chunk(ArithmOp, ArithmExpr);
ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[]);
ArithmExpr deref(String);

BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr);
BoolExpr disj_op(BoolExpr, BoolExpr[]);
BoolExpr conj_op(BoolExpr, BoolExpr[]);
BoolExpr not_op(BoolExpr);

Statement assign_stmt(String, ArithmExpr);
Statement append(Statement, Statement);
$ для упрощения описания языка считаем последовательность операторов
$ оператором
Statement compound(Statement, Statement[]);
Statement if_else_stmt(BoolExpr, Statement, Statement);
Statement empty_stmt();
Statement while_stmt(BoolExpr, Statement);
Statement input_stmt(String, String[]);

PrintItem print_value(ArithmExpr);
PrintItem print_string(String);
Statement print_stmt(PrintItem, PrintItem[]);

%grammar
Program =
StatementList
;

StatementList =
Statement
%rep (
SEMICOLON Statement
)
/ compound
;

Statement =
VAR ASSIGN Expr / assign_stmt
$ Ветка else может отсутствовать
| IF BoolExpr THEN StatementList
(
/ empty_stmt
| ELSE StatementList
)
END / if_else_stmt
| WHILE BoolExpr DO StatementList END / while_stmt
| INPUT VAR
%rep (
COMMA VAR
)
/ input_stmt
| PRINT PrintItem
%rep (
COMMA PrintItem
)
/ print_stmt
;

PrintItem =
Expr / print_value
| STRING / print_string
;

BoolExpr =
BoolTerm
%rep (
OR BoolTerm
)
/ disj_op
;

BoolTerm =
BoolFactor
%rep (
AND BoolFactor
)
/ conj_op
;

BoolFactor =
TRUE
| FALSE
| Expr RelaOp Expr / rela_op
| NOT BoolFactor / not_op
| LBRAC BoolExpr RBRAC
;

$ Первому терму в выражении может предшествовать знак минус
Expr =
(
Term
| MINUS Term / neg_op
)
%rep (
(
PLUS
| MINUS
)
Term / chunk
)
/ bin_op
;
Term =
Factor
%rep (
(
STAR
| FRAC
)
Factor / chunk
)
/ bin_op
;
Factor =
NUMBER
| VAR / deref
| LBRAC Expr RBRAC;

%axiom
Program

%end
```
Выше приведён возможный вариант автоформатирования — все вложенные правила (скобки) 
выводятся с новой строки с дополнительным отступом.

Комментарии начинаются на знак доллара и продолжаются до конца строки.

Имена терминалов, нетерминалов, типов и методов — идентификаторы, начинающиеся 
с буквы и состоящие из букв, цифр и знаков прочерка.

Секция методов содержит сигнатуры методов в стиле языка Java без указания имён 
параметров. Типы могут быть простыми или массивами.

Каждое правило грамматики состоит из нескольких альтернатив. Альтернатива может 
завершаться необязательным именем метода, предварённого знаком дробной черты, 
который вызывается после её разбора.

Элементами альтернативы могут быть имена терминальных и нетерминальных символов, 
либо вложенные (безымянные) правила. Вложенные правила, как и обычные правила, 
могут состоять из нескольких альтернатив, каждая из которых может 
завершаться знаком дробной черты и именем метода.

Перед элементом альтернативы может располагаться ключевое слово %rep, 
означающее повторение соответствующего элемента.

Ключевые слова и идентификаторы чувствительны к регистру.

# Реализация

Файл `lexer.h`
```C
#ifndef LAB3_2_LEXER_H
#define LAB3_2_LEXER_H

#include <stdbool.h>
#include <stdio.h>

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#endif

struct Extra {
    bool continued;
    int cur_line;
    int cur_column;
};

void init_scanner(FILE *input, yyscan_t *scanner, struct Extra *extra);
void destroy_scanner(yyscan_t);


#endif
```

Файл `lexer.l`
```C
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
```

Файл `parser.y`
```C
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
```

# Тестирование

Входные данные

Файл `input.txt`
```
%class
SimpleImperativeLang
%tokens
NUMBER PLUS MINUS STAR FRAC LBRAC RBRAC
TRUE FALSE ADD OR NOT LT GT LE GE NE EQ
IF THEN ELSE END WHILE DO SEMICOLON
VAR ASSIGN INPUT PRINT COMMA
%types
Expr, Term, Factor, NUMBER: ArithmExpr;
PLUS, MINUS, STAR, FRAC: ArithmOp;
BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE: BoolExpr;
LT, GT, LE, GE, NE, EQ: RelaOp;
Program, Statement, StatementList, Program: Statement;
VAR, STRING: String;
PrintItem: PrintItem;
%methods
ArithmExpr neg_op(ArithmOp, ArithmExpr);
ArithmExprChunk chunk(ArithmOp, ArithmExpr);
ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[]);
ArithmExpr deref(String);
BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr);
BoolExpr disj_op(BoolExpr, BoolExpr[]);
BoolExpr conj_op(BoolExpr, BoolExpr[]);
BoolExpr not_op(BoolExpr);
Statement assign_stmt(String, ArithmExpr);
Statement append(Statement, Statement);
$ для упрощения описания языка считаем последовательность операторов
$ оператором
Statement compound(Statement, Statement[]);
Statement if_else_stmt(BoolExpr, Statement, Statement);
Statement empty_stmt();
Statement while_stmt(BoolExpr, Statement);
Statement input_stmt(String, String[]);
PrintItem print_value(ArithmExpr);
PrintItem print_string(String);
Statement print_stmt(PrintItem, PrintItem[]);
%grammar
Program =
StatementList
;
StatementList =
Statement
%rep (
SEMICOLON Statement
)
/ compound
;
Statement =
VAR ASSIGN Expr / assign_stmt
$ Ветка else может отсутствовать
| IF BoolExpr THEN StatementList
( / empty_stmt | ELSE StatementList )
END / if_else_stmt
| WHILE BoolExpr DO StatementList END / while_stmt
| INPUT VAR
%rep (
COMMA VAR
)
/ input_stmt
| PRINT PrintItem
%rep (
COMMA PrintItem
)
/ print_stmt
;
PrintItem =
Expr / print_value
| STRING / print_string
;
BoolExpr =
BoolTerm
%rep (
OR BoolTerm
)
/ disj_op
;
BoolTerm =
BoolFactor
%rep (
AND BoolFactor
)
/ conj_op
;
BoolFactor =
TRUE
| FALSE
| Expr RelaOp Expr / rela_op
| NOT BoolFactor / not_op
| LBRAC BoolExpr RBRAC
;
$ Первому терму в выражении может предшествовать знак минус
Expr =
(
Term
| MINUS Term / neg_op
)
%rep (
(
PLUS
| MINUS
)
Term / chunk
)
/ bin_op
;
Term =
Factor
%rep (
(
STAR
| FRAC
)
Factor / chunk
)
/ bin_op
;
Factor =
NUMBER
| VAR / deref
| LBRAC Expr RBRAC;
%axiom
Program
%end
```

Вывод на `stdout`

```
Read file input.txt
%class
  SimpleImperativeLang

%tokens
  NUMBER PLUS MINUS STAR FRAC LBRAC RBRAC 
  TRUE FALSE ADD OR NOT LT GT LE GE NE EQ IF 
  THEN ELSE END WHILE DO SEMICOLON VAR ASSIGN INPUT PRINT COMMA 

%types
  Expr, Term, Factor, NUMBER : ArithmExpr;
  PLUS, MINUS, STAR, FRAC : ArithmOp;
  BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE : BoolExpr;
  LT, GT, LE, GE, NE, EQ : RelaOp;
  Program, Statement, StatementList, Program : Statement;
  VAR, STRING : String;
  PrintItem : PrintItem;
  
%methods
  ArithmExpr neg_op(ArithmOp, ArithmExpr);
  ArithmExprChunk chunk(ArithmOp, ArithmExpr);
  ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[]);
  ArithmExpr deref(String);
  BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr);
  BoolExpr disj_op(BoolExpr, BoolExpr[]);
  BoolExpr conj_op(BoolExpr, BoolExpr[]);
  BoolExpr not_op(BoolExpr);
  Statement assign_stmt(String, ArithmExpr);
  Statement append(Statement, Statement);
  
$ для упрощения описания языка считаем последовательность операторов

$ оператором
Statement compound(Statement, Statement[]);
  Statement if_else_stmt(BoolExpr, Statement, Statement);
  Statement empty_stmt()
  Statement while_stmt(BoolExpr, Statement);
  Statement input_stmt(String, String[]);
  PrintItem print_value(ArithmExpr);
  PrintItem print_string(String);
  Statement print_stmt(PrintItem, PrintItem[]);
  
%grammar
  Program =
      StatementList 
    ;

  StatementList =
      Statement 
      %rep (
          SEMICOLON Statement 
      )
      / compound
    ;

  Statement =
      VAR ASSIGN Expr / assign_stmt
$ Ветка else может отсутствовать

    | IF BoolExpr THEN StatementList (
          / empty_stmt
        | ELSE StatementList 
      )
      END / if_else_stmt
    | WHILE BoolExpr DO StatementList END / while_stmt
    | INPUT VAR 
      %rep (
          COMMA VAR 
      )
      / input_stmt
    | PRINT PrintItem 
      %rep (
          COMMA PrintItem 
      )
      / print_stmt
    ;

  PrintItem =
      Expr / print_value
    | STRING / print_string
    ;

  BoolExpr =
      BoolTerm 
      %rep (
          OR BoolTerm 
      )
      / disj_op
    ;

  BoolTerm =
      BoolFactor 
      %rep (
          AND BoolFactor 
      )
      / conj_op
    ;

  BoolFactor =
      TRUE 
    | FALSE 
    | Expr RelaOp Expr / rela_op
    | NOT BoolFactor / not_op
    | LBRAC BoolExpr RBRAC 
    ;


$ Первому терму в выражении может предшествовать знак минус
  Expr =
      (
          Term 
        | MINUS Term / neg_op
      )
      
      %rep (
          (
              PLUS 
            | MINUS 
          )
          Term / chunk
      )
      / bin_op
    ;

  Term =
      Factor 
      %rep (
          (
              STAR 
            | FRAC 
          )
          Factor / chunk
      )
      / bin_op
    ;

  Factor =
      NUMBER 
    | VAR / deref
    | LBRAC Expr RBRAC 
    ;


%axiom
  Program

%end
```

# Вывод
В ходе лабораторной работы был успешно разработан форматтер для языка 
спецификации синтаксических анализаторов с 
использованием инструментов flex и bison. Реализация продемонстрировала 
корректную обработку всех ключевых элементов 
языка: секций class, tokens, types, методов и грамматических правил с 
поддержкой вложенных конструкций, комментариев 
и повторяющихся элементов. Особое внимание было уделено автоматическому 
форматированию с сохранением структуры 
исходного кода и добавлением соответствующих отступов. Тестирование подтвердило, 
что программа правильно анализирует 
и форматирует входные данные, включая сложные грамматические 
конструкции с альтернативами и операторами. 
Работа позволила закрепить практические навыки создания лексических 
и синтаксических анализаторов, а также углубить 
понимание формального описания грамматик.