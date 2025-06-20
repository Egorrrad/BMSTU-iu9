package parser

// Specification представляет всю спецификацию
type Specification struct {
	ClassName string
	Tokens    []string
	Types     []TypeMapping
	Methods   []Method
	Grammar   []Rule
	Axiom     string
}

// TypeMapping представляет сопоставление символов с типом
type TypeMapping struct {
	Symbols  []string
	TypeName string
	IsArray  bool // Для типов массивов (с [] в конце)
}

// Method представляет объявление метода
type Method struct {
	Name       string
	ReturnType string
	IsArray    bool
	Params     []Parameter
}

// Parameter представляет параметр метода
type Parameter struct {
	TypeName string
	IsArray  bool
}

// Rule представляет грамматическое правило
type Rule struct {
	Name       string
	Production Expression
	Action     string
}

// Expression представляет правую часть правила
type Expression interface {
	isExpression()
}

// Sequence представляет упорядоченный список выражений
type Sequence struct {
	Items []Expression
}

func (s Sequence) isExpression() {}

// Alternative представляет выбор между выражениями
type Alternative struct {
	Options []Expression
}

func (a Alternative) isExpression() {}

// Repetition представляет ноль или более вхождений
type Repetition struct {
	Item   Expression
	Action string
}

func (r Repetition) isExpression() {}

// Terminal представляет конкретный токен
type Terminal struct {
	Value string
}

func (t Terminal) isExpression() {}

// NonTerminal представляет ссылку на нетерминал
type NonTerminal struct {
	Name string
}

func (nt NonTerminal) isExpression() {}

// Grouped представляет заключенное в скобки подвыражение
type Grouped struct {
	Expression Expression
}

func (g Grouped) isExpression() {}
