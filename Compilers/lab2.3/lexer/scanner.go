package lexer

import (
	"fmt"
	"io"
	"strings"
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
	start := *s.position
	var tag DomainTag
	var value interface{}

	for {
		switch ch := s.position.Cp(); {
		case ch == -1:
			tag = EOFTag
		case ch == '`':
			tag, value = s.scanKeyword()
		case unicode.IsUpper(ch):
			tag, value = s.scanNeterm()
		case ch == '"':
			tag, value = s.scanString()
		case unicode.IsSpace(ch):
			s.skipWhitespace()
			continue
		default:
			s.compiler.AddMessage(true, s.position,
				fmt.Sprintf("Unexpected character: %c", ch))
			s.position.Next()
		}
		break
	}

	end := *s.position
	return Token{
		Tag:    tag,
		Coords: Fragment{start, end},
		Value:  value,
	}
}

func (s *Scanner) scanKeyword() (DomainTag, interface{}) {
	var buf strings.Builder
	start := s.position

	for {
		ch := s.position.Cp()
		if ch == -1 || unicode.IsSpace(ch) {
			break
		}
		buf.WriteRune(ch)
		s.position.Next()
	}

	keyword := buf.String()
	if isValidKeyword(keyword) {
		return KeywordTag, keyword
	}

	s.compiler.AddMessage(true, start, "Invalid keyword: "+keyword)
	return ErrorTag, keyword
}

func (s *Scanner) scanNeterm() (DomainTag, interface{}) {
	var buf strings.Builder

	for {
		ch := s.position.Cp()
		if !unicode.IsUpper(ch) && !unicode.IsDigit(ch) {
			break
		}
		buf.WriteRune(ch)
		s.position.Next()
	}

	ident := buf.String()
	code := s.compiler.AddName(ident)
	return NeterminalTag, code
}

func (s *Scanner) scanString() (DomainTag, interface{}) {
	var buf strings.Builder
	start := s.position
	s.position.Next() // пропускаем "

	for {
		ch := s.position.Cp()
		if ch == -1 {
			s.compiler.AddMessage(true, start, "Unterminated string")
			return ErrorTag, buf.String()
		}
		if ch == '"' {
			s.position.Next()
			break
		}
		buf.WriteRune(ch)
		s.position.Next()
	}

	return StringTag, buf.String()
}

func (s *Scanner) skipWhitespace() {
	for unicode.IsSpace(s.position.Cp()) {
		s.position.Next()
	}
}

func isValidKeyword(s string) bool {
	keywords := []string{
		"`axiom", "`is", "`or", "`end", "`epsilon",
	}
	for _, kw := range keywords {
		if s == kw {
			return true
		}
	}
	return false
}
