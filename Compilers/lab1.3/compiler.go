package main

import (
	"container/list"
	"fmt"
)

type Compiler struct {
	messages  *list.List
	nameTable map[string]int
	names     []string
}

func NewCompiler() *Compiler {
	return &Compiler{
		messages:  list.New(),
		nameTable: make(map[string]int),
		names:     make([]string, 0),
	}
}

func (c *Compiler) AddMessage(isError bool, pos *Position, text string) {
	c.messages.PushBack(fmt.Sprintf("%s (%d,%d): %s",
		map[bool]string{true: "Error", false: "Warning"}[isError],
		pos.Line(), pos.Pos(), text))
}

func (c *Compiler) AddName(name string) int {
	if code, exists := c.nameTable[name]; exists {
		return code
	}
	code := len(c.names)
	c.names = append(c.names, name)
	c.nameTable[name] = code
	return code
}
