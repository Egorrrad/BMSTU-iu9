package lexer

import "fmt"

type Token struct {
	Tag    DomainTag
	Coords Fragment
	Value  interface{}
}

type Fragment struct {
	Start  Position
	Finish Position
}

func (f Fragment) String() string {
	return fmt.Sprintf("(%d,%d)-(%d,%d)",
		f.Start.Line(), f.Start.Pos(),
		f.Finish.Line(), f.Finish.Pos())
}
