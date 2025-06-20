package main

import (
	"io"
	"unicode"
)

type Scanner struct {
	compiler *Compiler
	position *Position
}

func NewScanner(reader io.Reader, compiler *Compiler) *Scanner {
	return &Scanner{
		compiler: compiler,
		position: NewPosition(reader),
	}
}

func (s *Scanner) NextToken() Token {
	for {
		start := *s.position

		switch {
		case s.position.Cp() == -1:
			return Token{EndOfProgram, Fragment{start, *s.position}, nil}

		case unicode.IsSpace(s.position.Cp()):
			s.position.Next()
			continue

		case s.position.Cp() == '*':
			return s.processStar(start)

		case s.position.Cp() == '+', s.position.Cp() == '-':
			return s.processNumber(start)

		case unicode.IsUpper(s.position.Cp()):
			return s.processIdentOrKeyword(start)

		default:
			s.compiler.AddMessage(true, s.position, "Unexpected character")
			s.position.Next()
		}
	}
}

func (s *Scanner) processStar(start Position) Token {
	s.position.Next()
	if s.position.Cp() == '*' {
		s.position.Next()
		return Token{KeywordDoublestar, Fragment{start, *s.position}, nil}
	}
	return Token{NUMBER, Fragment{start, *s.position}, 0}
}

func (s *Scanner) processNumber(start Position) Token {
	sign := s.position.Cp()
	s.position.Next()

	count := 1
	for s.position.Cp() == sign {
		count++
		s.position.Next()
	}

	value := count
	if sign == '-' {
		value = -value
	}

	return Token{NUMBER, Fragment{start, *s.position}, value}
}

func (s *Scanner) processIdentOrKeyword(start Position) Token {
	var buf []rune
	for unicode.IsUpper(s.position.Cp()) {
		buf = append(buf, s.position.Cp())
		s.position.Next()
	}

	for s.position.Cp() == '+' || s.position.Cp() == '-' || s.position.Cp() == '*' {
		buf = append(buf, s.position.Cp())
		s.position.Next()
	}

	str := string(buf)
	switch str {
	case "ON":
		return Token{KeywordOn, Fragment{start, *s.position}, nil}
	case "OFF":
		return Token{KeywordOff, Fragment{start, *s.position}, nil}
	default:
		code := s.compiler.AddName(str)
		return Token{IDENT, Fragment{start, *s.position}, code}
	}
}
