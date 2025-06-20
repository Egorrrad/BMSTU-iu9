package lexer

import (
	"bufio"
	"io"
	"unicode/utf8"
)

type Position struct {
	reader  *bufio.Reader
	line    int
	pos     int
	current rune
}

func NewPosition(reader io.Reader) *Position {
	p := &Position{
		reader:  bufio.NewReader(reader),
		line:    1,
		pos:     1,
		current: -1,
	}
	p.Next()
	return p
}

func (p *Position) Cp() rune  { return p.current }
func (p *Position) Line() int { return p.line }
func (p *Position) Pos() int  { return p.pos }

func (p *Position) Next() {
	r, size, err := p.reader.ReadRune()
	if err != nil {
		p.current = -1
		return
	}

	if p.current == '\n' {
		p.line++
		p.pos = 1
	} else {
		p.pos++
	}

	p.current = r

	if r == utf8.RuneError && size == 1 {
		p.reader.UnreadRune()
	}
}
