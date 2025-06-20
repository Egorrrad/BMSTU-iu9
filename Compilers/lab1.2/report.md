% Лабораторная работа № 1.2. «Лексический анализатор
на основе регулярных выражений»
% 4 марта 2025 г.
% 

# Цель работы
Целью данной работы является приобретение навыка разработки простейших лексических анализаторов, 
работающих на основе поиска в тексте по образцу, заданному регулярным выражением.
# Индивидуальный вариант
* Числа: последовательности десятичных цифр.
* Идентификаторы: последовательности латинских букв и десятичных цифр, содержащих как минимум одну букву.
* Знаки операций: «+», «-», «\*», «/», «(», «)».
* Комментарии начинаются на «(\*», заканчиваются на «\*)», не могут быть вложенными.

# Реализaция

```go
package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
	"unicode/utf8"
)

type Tоken struct {
	Tag    string
	Value  string
	Line   int
	Column int
}

type Lexer struct {
	input    []byte
	pos      int
	line     int
	column   int
	patterns []Pattern
}

type Pattern struct {
	tag   string
	regex *regexp.Regexp
}

const (
	CommentTag  = "COMMENT"
	IdentTag    = "IDENT"
	NumberTag   = "NUMBER"
	OperatorTag = "OPERATOR"
)

func NewLexer(input []byte) *Lexer {
	patterns := []Pattern{
		{CommentTag, regexp.MustCompile(`^\(\*.*\*\)`)},
		{NumberTag, regexp.MustCompile(`^\d+`)},
		{IdentTag, regexp.MustCompile(`^[a-zA-Z0-9]*[a-zA-Z][a-zA-Z0-9]*`)},
		{OperatorTag, regexp.MustCompile(`^[+\-*/()]`)},
	}
	return &Lexer{
		input:    input,
		pos:      0,
		line:     1,
		column:   1,
		patterns: patterns,
	}
}

func (l *Lexer) nextToken() (*Token, error) {
	// Пропускаем пробельные символы
	l.skipWhitespace()

	// Если достигнут конец входных данных
	if l.pos >= len(l.input) {
		return nil, nil
	}

	for _, pattern := range l.patterns {
		if match := pattern.regex.Find(l.input[l.pos:]); match != nil {
			value := string(match)
			if pattern.tag == CommentTag {
				// проверяем вложенность
				comL := len(value)
				comment := value[2 : comL-2]
				if strings.Contains(comment, "*)") || strings.Contains(comment, "(*") {
					line := l.line
					column := l.column
					l.pos += len(match)
					l.column += utf8.RuneCountInString(value)
					return nil, fmt.Errorf("syntax error (%d,%d)", line, column)
				}
			}
			token := &Token{
				Tag:    pattern.tag,
				Value:  value,
				Line:   l.line,
				Column: l.column,
			}
			l.pos += len(match)
			l.column += utf8.RuneCountInString(value)
			return token, nil
		}
	}

	return nil, fmt.Errorf("syntax error (%d,%d)", l.line, l.column)
}

func (l *Lexer) skipWhitespace() {
	for l.pos < len(l.input) {
		r, size := utf8.DecodeRune(l.input[l.pos:])
		if !isWhitespace(r) {
			break
		}
		if r == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.pos += size
	}
}

func isWhitespace(r rune) bool {
	return r == ' ' || r == '\t' || r == '\n'
}

func (l *Lexer) skipError() {
	for l.pos < len(l.input) {
		r, size := utf8.DecodeRune(l.input[l.pos:])
		if isWhitespace(r) {
			if r == '\n' {
				l.line++
				l.column = 1
			} else {
				l.column++
			}
			l.pos += size
			break
		}
		l.pos += size
		l.column++
	}
}

func main() {
	input, err := os.ReadFile("lab1.2/input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	lexer := NewLexer(input)

	for {
		token, err := lexer.nextToken()
		if err != nil {
			fmt.Println(err)
			lexer.skipError()
		}
		if token != nil {
			fmt.Printf("%s (%d,%d): %s\n", token.Tag, token.Line, token.Column, token.Value)
		}
		if token == nil && err == nil {
			break
		}
	}
}
```

# Тестирование

Входные данные

```
count := 100 (* This is a comment *)
q12345
a + b * (c - d)
@invalid_symbol
abc123 (* Another comment with spaces *)
x := y + 42
(* Nested (* comments *) are not allowed *) *)
123456
123 + abc + 456
12345
```

Вывод на `stdout`

```
IDENT (1,1): count
syntax error (1,7)
NUMBER (1,10): 100
COMMENT (1,14): (* This is a comment *)
IDENT (2,1): q12345
IDENT (3,1): a
OPERATOR (3,3): +
IDENT (3,5): b
OPERATOR (3,7): *
OPERATOR (3,9): (
IDENT (3,10): c
OPERATOR (3,12): -
IDENT (3,14): d
OPERATOR (3,15): )
syntax error (4,1)
IDENT (5,1): abc123
COMMENT (5,8): (* Another comment with spaces *)
IDENT (6,1): x
syntax error (6,3)
IDENT (6,6): y
OPERATOR (6,8): +
NUMBER (6,10): 42
syntax error (7,1)
NUMBER (8,1): 123456
NUMBER (9,1): 123
OPERATOR (9,5): +
IDENT (9,7): abc
OPERATOR (9,11): +
NUMBER (9,13): 456
NUMBER (10,1): 12345
```

# Вывод
В ходе выполнения лабораторной работы был разработан лексический анализатор на основе регулярных выражений, 
который успешно идентифицирует числа, идентификаторы, знаки операций и комментарии в тексте. 
Реализация на языке Go показала свою эффективность при обработке входных данных, 
включая корректное распознавание лексем и обработку ошибок, 
таких как недопустимые символы и вложенные комментарии. 
В процессе тестирования были выявлены и обработаны синтаксические ошибки, 
что подтверждает корректность работы анализатора.