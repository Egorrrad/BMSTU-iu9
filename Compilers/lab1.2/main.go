package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
	"unicode/utf8"
)

type Token struct {
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
