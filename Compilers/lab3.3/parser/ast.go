package parser

import (
	"Compilers/lab3.3/lexer"
)

// Specification представляет всю спецификацию
type Specification struct {
	ClassName string
	Tokens    []string
	Types     []TypeMapping
	Methods   []Method
	Grammar   []Rule
	Axiom     string
	Pos       lexer.Fragment
}

// TypeMapping представляет сопоставление символов с типом
type TypeMapping struct {
	Symbols  []string
	TypeName string
	IsArray  bool
	Pos      lexer.Fragment
}

// Method представляет объявление метода
type Method struct {
	Name       string
	ReturnType string
	IsArray    bool
	Params     []Parameter
	Pos        lexer.Fragment
}

// Parameter представляет параметр метода
type Parameter struct {
	TypeName string
	IsArray  bool
	Pos      lexer.Fragment
}

// Rule представляет правило грамматики
type Rule struct {
	Name       string
	Production Expression
	Action     string
	Type       string
	Pos        lexer.Fragment
}

// Expression представляет правую часть правила
type Expression interface {
	isExpression()
	GetPos() lexer.Fragment
}

// Sequence представляет упорядоченный список выражений
type Sequence struct {
	Items  []Expression
	Action string
	Pos    lexer.Fragment
}

func (s Sequence) isExpression()          {}
func (s Sequence) GetPos() lexer.Fragment { return s.Pos }

// Alternative представляет выбор между выражениями
type Alternative struct {
	Options []Expression
	Action  string
	Pos     lexer.Fragment
}

func (a Alternative) isExpression()          {}
func (a Alternative) GetPos() lexer.Fragment { return a.Pos }

// Repetition представляет ноль или более вхождений
type Repetition struct {
	Item   Expression
	Action string
	Pos    lexer.Fragment
}

func (r Repetition) isExpression()          {}
func (r Repetition) GetPos() lexer.Fragment { return r.Pos }

// Terminal представляет конкретный токен
type Terminal struct {
	Value string
	Pos   lexer.Fragment
}

func (t Terminal) isExpression()          {}
func (t Terminal) GetPos() lexer.Fragment { return t.Pos }

// NonTerminal нетерминал
type NonTerminal struct {
	Name string
	Pos  lexer.Fragment
}

func (nt NonTerminal) isExpression()          {}
func (nt NonTerminal) GetPos() lexer.Fragment { return nt.Pos }

// Grouped представляет заключенное в скобки подвыражение
type Grouped struct {
	Expression Expression
	Action     string
	Pos        lexer.Fragment
}

func (g Grouped) isExpression()          {}
func (g Grouped) GetPos() lexer.Fragment { return g.Pos }

// MethodAction представляет действие метода
type MethodAction struct {
	MethodName string
	Expression Expression
	Pos        lexer.Fragment
}

func (m MethodAction) isExpression()          {}
func (m MethodAction) GetPos() lexer.Fragment { return m.Pos }

// UnaryOp представляет унарный оператор
type UnaryOp struct {
	Op     string
	Right  Expression
	Action string
	Pos    lexer.Fragment
}

func (u UnaryOp) isExpression()          {}
func (u UnaryOp) GetPos() lexer.Fragment { return u.Pos }

// BinaryOp представляет бинарный оператор
type BinaryOp struct {
	Left   Expression
	Op     string
	Right  Expression
	Action string
	Pos    lexer.Fragment
}

func (b BinaryOp) isExpression()          {}
func (b BinaryOp) GetPos() lexer.Fragment { return b.Pos }

// MethodCall представляет вызов метода
type MethodCall struct {
	MethodName string
	Args       []Expression
	Pos        lexer.Fragment
}

func (m MethodCall) isExpression()          {}
func (m MethodCall) GetPos() lexer.Fragment { return m.Pos }
