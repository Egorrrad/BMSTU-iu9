package lexer

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"unicode"
)

type Scanner struct {
	reader    *bufio.Reader
	compiler  *Compiler
	current   rune
	line      int
	pos       int
	startLine int
	startPos  int
	eof       bool
}

func NewScanner(reader io.Reader, compiler *Compiler) *Scanner {
	s := &Scanner{
		reader:   bufio.NewReader(reader),
		compiler: compiler,
		line:     1,
		pos:      1,
	}
	// читаем первую руну
	if err := s.advance(); err != nil {
		s.current = -1
		s.eof = true
	}
	return s
}

// перемещаемся к следующему символу
func (s *Scanner) advance() error {
	if s.eof {
		return nil
	}
	r, _, err := s.reader.ReadRune()
	if err == io.EOF {
		s.eof = true
		s.current = -1
		return nil
	}
	if err != nil {
		return err
	}
	s.current = r
	if s.current == '\n' {
		s.line++
		s.pos = 1
	} else {
		s.pos++
	}
	return nil
}

// смотрим на текущий символ без перемещения
func (s *Scanner) peek() (rune, error) {
	if s.eof {
		return -1, nil
	}
	r, _, err := s.reader.ReadRune()
	if err == io.EOF {
		return -1, nil
	}
	if err != nil {
		return 0, err
	}
	s.reader.UnreadRune()
	return r, nil
}

func (s *Scanner) startToken() {
	s.startLine = s.line
	s.startPos = s.pos
}

func (s *Scanner) makeCoords() Fragment {
	start := Position{line: s.startLine, pos: s.startPos}
	finish := Position{line: s.line, pos: s.pos}
	return Fragment{
		Start:  start,
		Finish: finish,
	}
}

func (s *Scanner) error(msg string) Token {
	start := Position{line: s.startLine, pos: s.startPos}
	s.compiler.AddMessage(true, &start, msg)
	return Token{Tag: ErrorTag, Coords: s.makeCoords(), Value: nil}
}

// пропускаем пробелы
func (s *Scanner) skipWhitespace() error {
	for !s.eof && unicode.IsSpace(s.current) {
		if err := s.advance(); err != nil {
			return err
		}
	}
	return nil
}

// пропускаем комменты начинающиеся с $
func (s *Scanner) skipComment() error {
	for !s.eof && s.current != '\n' {
		if err := s.advance(); err != nil {
			return err
		}
	}
	return nil
}

var keywordTags = map[string]DomainTag{
	"%class":   KeywordTag,
	"%tokens":  KeywordTag,
	"%types":   KeywordTag,
	"%methods": KeywordTag,
	"%grammar": KeywordTag,
	"%axiom":   KeywordTag,
	"%end":     KeywordTag,
	"%rep":     KeywordTag,
}

func (s *Scanner) readIdentifier() (Token, error) {
	var sb strings.Builder
	if !unicode.IsLetter(s.current) && s.current != '_' && s.current != '%' {
		return s.error(fmt.Sprintf("Invalid character for identifier: %c", s.current)), nil
	}
	sb.WriteRune(s.current)

	for {
		next, err := s.peek()
		if err != nil {
			return Token{}, err
		}
		if !unicode.IsLetter(next) && !unicode.IsDigit(next) && next != '_' {
			break
		}
		if err := s.advance(); err != nil {
			return Token{}, err
		}
		sb.WriteRune(s.current)
	}

	if err := s.advance(); err != nil {
		return Token{}, err
	}

	// проверяем ключевое ли это слово
	ident := sb.String()
	if tag, ok := keywordTags[ident]; ok {
		return Token{Tag: tag, Coords: s.makeCoords(), Value: ident}, nil
	}

	nameCode := s.compiler.AddName(ident)
	return Token{Tag: IdentifierTag, Coords: s.makeCoords(), Value: nameCode}, nil
}

// читаем следующий токен
func (s *Scanner) NextToken() Token {
	for {
		if err := s.skipWhitespace(); err != nil {
			start := Position{line: s.line, pos: s.pos}
			s.compiler.AddMessage(true, &start, err.Error())
			return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
		}

		// проверяем конец ли файла
		if s.eof {
			return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
		}
		s.startToken()

		if s.current == '$' {
			if err := s.skipComment(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			continue
		}

		switch s.current {
		case ';':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: SemicolonTag, Coords: s.makeCoords(), Value: nil}
		case ',':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: CommaTag, Coords: s.makeCoords(), Value: nil}
		case ':':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: ColonTag, Coords: s.makeCoords(), Value: nil}
		case '=':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: EqualsTag, Coords: s.makeCoords(), Value: nil}
		case '/':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: SlashTag, Coords: s.makeCoords(), Value: nil}
		case '|':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: BarTag, Coords: s.makeCoords(), Value: nil}
		case '(':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: LParTag, Coords: s.makeCoords(), Value: nil}
		case ')':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: RParTag, Coords: s.makeCoords(), Value: nil}
		case '[':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: LBracketTag, Coords: s.makeCoords(), Value: nil}
		case ']':
			if err := s.advance(); err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return Token{Tag: RBracketTag, Coords: s.makeCoords(), Value: nil}
		}

		if unicode.IsLetter(s.current) || s.current == '_' || s.current == '%' {
			token, err := s.readIdentifier()
			if err != nil {
				start := Position{line: s.line, pos: s.pos}
				s.compiler.AddMessage(true, &start, err.Error())
				return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
			}
			return token
		}

		token := s.error(fmt.Sprintf("Unexpected character: %c", s.current))
		if err := s.advance(); err != nil {
			start := Position{line: s.line, pos: s.pos}
			s.compiler.AddMessage(true, &start, err.Error())
			return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
		}
		return token
	}
}
