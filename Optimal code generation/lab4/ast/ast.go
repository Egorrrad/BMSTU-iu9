package ast

import (
	"fmt"
	"strings"
)

// Node — базовый интерфейс для узлов AST.
type Node interface {
	String() string
}

// Expr — интерфейс для выражений AST.
type Expr interface {
	Node
	isExpr()
}

// Stmt — интерфейс для операторов AST.
type Stmt interface {
	Node
	isStmt()
}

// Number представляет числовой литерал.
type Number struct {
	Value int
}

func (n Number) String() string { return fmt.Sprintf("%d", n.Value) }
func (n Number) isExpr()        {}

// Variable представляет переменную.
type Variable struct {
	Name string
}

func (v Variable) String() string { return v.Name }
func (v Variable) isExpr()        {}

// BinaryOp представляет бинарную операцию
type BinaryOp struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b BinaryOp) String() string {
	return fmt.Sprintf("(%s %s %s)", b.Left, b.Op, b.Right)
}
func (b BinaryOp) isExpr() {}

// Assignment представляет присваивание
type Assignment struct {
	Name string
	Expr Expr
}

func (a Assignment) String() string {
	return fmt.Sprintf("%s = %s", a.Name, a.Expr)
}
func (a Assignment) isStmt() {}

// IfStmt представляет условный оператор if.
type IfStmt struct {
	Cond Expr
	Then *Block
	Else *Block
}

func (i IfStmt) String() string {
	elsePart := ""
	if i.Else != nil {
		elsePart = fmt.Sprintf(" else %s", i.Else)
	}
	return fmt.Sprintf("if %s %s%s", i.Cond, i.Then, elsePart)
}
func (i IfStmt) isStmt() {}

// WhileStmt представляет цикл while.
type WhileStmt struct {
	Cond Expr
	Body *Block
}

func (w WhileStmt) String() string {
	return fmt.Sprintf("while %s %s", w.Cond, w.Body)
}
func (w WhileStmt) isStmt() {}

// Block представляет блок операторов.
type Block struct {
	Statements []Stmt
}

func (b Block) String() string {
	var stmts []string
	for _, s := range b.Statements {
		stmts = append(stmts, s.String())
	}
	return fmt.Sprintf("{\n%s\n}", strings.Join(stmts, "\n"))
}
