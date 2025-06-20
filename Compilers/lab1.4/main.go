package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

type Position struct {
	Line, Column int
}

type Token struct {
	Tag      string
	StartPos Position
	EndPos   Position
	Value    string
}

type Lexer struct {
	reader        *bufio.Reader
	current       rune
	pos           Position
	symbolClasses map[rune]int
	transitions   [][]int
	finalStates   map[int]string
	keywordStates map[int]string
}

func NewLexer(reader io.Reader) *Lexer {
	l := &Lexer{
		reader: bufio.NewReader(reader),
		pos:    Position{Line: 1, Column: 0},
		symbolClasses: map[rune]int{
			' ': 0, '\t': 0, '\r': 0,
			':': 1, '{': 2, '}': 3, '!': 4,
			'\n': 8,
			's':  9, // для 'switch'
			'w':  10,
			'i':  11,
			't':  12,
			'c':  13, // для 'case'
			'h':  14,
			'a':  15,
			'e':  16,
		},
		transitions: [][]int{
			// ws  :   {   }   !  letter digit other \n  s   w   i   t   c   h   a   e
			{0, 2, 2, 2, 4, 1, 3, -1, 0, 5, 1, 1, 1, 6, 1, 1, 1, 15, 1, 1},                   // 0 начальное состояние
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},              // 1 идентификаторы
			{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, // 2 операторы
			{-1, -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},  // 3 числа
			{4, 4, 4, 4, 4, 4, 4, 4, -1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4},                    // 4 комментарий, до \n
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1},              // 5 's' (s1)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 8, 1, 1, 1, 1},              // 6 'c' (c1)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 9, 1, 1, 1, 1, 1, 1, 1, 1},              // 7 'sw' (s2)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},             // 8 'ca' (c2)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 11, 1, 1, 1, 1, 1, 1, 1},             // 9 'swi' (s3)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 12, 1, 1, 1},             // 10 'cas' (c3)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 13, 1, 1, 1, 1, 1, 1},             // 11 'swit' (s4)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},              // 12 'case' (keyword)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 14, 1, 1, 1, 1, 1},             // 13 'switc' (s5)
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},              // 14 'switch' (keyword)
		},
		finalStates: map[int]string{
			1:  "IDENTIFIER",
			2:  "OPERATOR",
			3:  "NUMBER",
			4:  "COMMENT",
			12: "KEYWORD",    // 'case'
			14: "KEYWORD",    // 'switch'
			5:  "IDENTIFIER", // s1
			6:  "IDENTIFIER", // c1
			7:  "IDENTIFIER", // s2
			8:  "IDENTIFIER", // c2
			9:  "IDENTIFIER", // s3
			10: "IDENTIFIER", // c3
			11: "IDENTIFIER", // s4
			13: "IDENTIFIER", // s5
			15: "IDENTIFIER", // e1
			16: "IDENTIFIER", // e2
		},
		keywordStates: map[int]string{},
	}

	// заполняем буквы и цифры
	for ch := 'a'; ch <= 'z'; ch++ {
		if _, exists := l.symbolClasses[ch]; !exists {
			l.symbolClasses[ch] = 5 // обычные буквы
		}
	}
	for ch := 'A'; ch <= 'Z'; ch++ {
		if _, exists := l.symbolClasses[ch]; !exists {
			l.symbolClasses[ch] = 5 // обычные буквы заглавные
		}
	}
	for ch := '0'; ch <= '9'; ch++ {
		l.symbolClasses[ch] = 6
	}

	l.readRune()
	return l
}

func (l *Lexer) readRune() {
	r, _, err := l.reader.ReadRune()
	if err != nil {
		l.current = 0
		return
	}
	l.current = r
	if r == '\n' {
		l.pos.Line++
		l.pos.Column = 0
	} else {
		l.pos.Column++
	}
}

func (l *Lexer) NextToken() (*Token, error) {
	// Если достигнут конец файла
	if l.current == 0 {
		return nil, io.EOF
	}

	state := 0
	startPos := l.pos
	var tokenValue strings.Builder

	lastFinalState := -1
	lastFinalPos := l.pos
	lastFinalValueLen := 0

	for {
		class, exists := l.symbolClasses[l.current]
		if !exists {
			class = 7
		}

		nextState := -1
		if state >= 0 && state < len(l.transitions) && class >= 0 && class < len(l.transitions[state]) {
			nextState = l.transitions[state][class]
		}

		if nextState == -1 {
			if lastFinalState != -1 {
				tag := l.finalStates[lastFinalState]
				tokenStr := tokenValue.String()[:lastFinalValueLen]

				if state == 0 && lastFinalState == 0 {
					state = 0
					startPos = l.pos
					tokenValue.Reset()
					lastFinalState = -1
					continue
				}

				return &Token{
					Tag:      tag,
					StartPos: startPos,
					EndPos:   lastFinalPos,
					Value:    tokenStr,
				}, nil
			}

			if tokenValue.Len() == 0 {
				errChar := l.current
				errPos := l.pos
				l.readRune()
				return nil, fmt.Errorf("unknown character at (%d,%d): %q",
					errPos.Line, errPos.Column, string(errChar))
			} else {
				return nil, fmt.Errorf("unrecognized token at (%d,%d): %s",
					startPos.Line, startPos.Column, tokenValue.String())
			}
		}

		tokenValue.WriteRune(l.current)
		state = nextState

		if _, isFinal := l.finalStates[state]; isFinal {
			lastFinalState = state
			lastFinalPos = l.pos
			lastFinalValueLen = tokenValue.Len()
		}

		l.readRune()

		if l.current == 0 {
			if tag, isFinal := l.finalStates[state]; isFinal {
				return &Token{
					Tag:      tag,
					StartPos: startPos,
					EndPos:   l.pos,
					Value:    tokenValue.String(),
				}, nil
			}

			if lastFinalState != -1 {
				tag := l.finalStates[lastFinalState]
				return &Token{
					Tag:      tag,
					StartPos: startPos,
					EndPos:   lastFinalPos,
					Value:    tokenValue.String()[:lastFinalValueLen],
				}, nil
			}
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: lexer <inputfile>")
		return
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer file.Close()

	lexer := NewLexer(file)

	for {
		token, err := lexer.NextToken()
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Println("Error:", err)
			continue
		}

		displayValue := strings.ReplaceAll(token.Value, "\n", "")
		if token.Tag == "UNKNOWN" {
			fmt.Printf("Unknown character at (%d,%d): %q\n", token.StartPos.Line, token.StartPos.Column, displayValue)
		} else {
			fmt.Printf("%s (%d,%d)-(%d,%d): %s\n",
				token.Tag,
				token.StartPos.Line, token.StartPos.Column,
				token.EndPos.Line, token.EndPos.Column,
				displayValue)
		}
		if token.Tag == "KEYWORD" && displayValue == "EOF" {
			break
		}
	}
}
