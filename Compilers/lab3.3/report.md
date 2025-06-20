% Лабораторная работа № 3.3 «Семантический анализ»
% 4 июня 2025 г.
% 

# Цель работы
Целью данной работы является получение навыков выполнения семантического анализа.

# Индивидуальный вариант
Язык спецификации для генератора синтаксических анализаторов:
```
%class
SimpleImperativeLang

%tokens
NUMBER PLUS MINUS STAR FRAC LBRAC RBRAC
TRUE FALSE ADD OR NOT LT GT LE GE NE EQ
IF THEN ELSE END WHILE DO SEMICOLON
VAR ASSIGN INPUT PRINT COMMA

%types
Expr, Term, Factor, NUMBER: ArithmExpr;
PLUS, MINUS, STAR, FRAC: ArithmOp;
BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE: BoolExpr;
LT, GT, LE, GE, NE, EQ: RelaOp;
Program, Statement, StatementList, Program: Statement;
VAR, STRING: String;
PrintItem: PrintItem;

%methods
ArithmExpr neg_op(ArithmOp, ArithmExpr);
ArithmExprChunk chunk(ArithmOp, ArithmExpr);
ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[]);
ArithmExpr deref(String);

BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr);
BoolExpr disj_op(BoolExpr, BoolExpr[]);
BoolExpr conj_op(BoolExpr, BoolExpr[]);
BoolExpr not_op(BoolExpr);

Statement assign_stmt(String, ArithmExpr);
Statement append(Statement, Statement);
$ для упрощения описания языка считаем последовательность операторов
$ оператором
Statement compound(Statement, Statement[]);
Statement if_else_stmt(BoolExpr, Statement, Statement);
Statement empty_stmt();
Statement while_stmt(BoolExpr, Statement);
Statement input_stmt(String, String[]);

PrintItem print_value(ArithmExpr);
PrintItem print_string(String);
Statement print_stmt(PrintItem, PrintItem[]);

%grammar
Program = StatementList;

StatementList = Statement %rep (SEMICOLON Statement) / compound;

Statement =
VAR ASSIGN Expr / assign_stmt
$ Ветка else может отсутствовать
| IF BoolExpr THEN StatementList (/ empty_stmt | ELSE StatementList) END
/ if_else_stmt
| WHILE BoolExpr DO StatementList END / while_stmt
| INPUT VAR %rep (COMMA VAR) / input_stmt
| PRINT PrintItem (COMMA PrintItem) / print_stmt
;

PrintItem = Expr / print_value | STRING / print_string;

BoolExpr = BoolTerm %rep (OR BoolTerm) / disj_op;
BoolTerm = BoolFactor %rep (AND BoolFactor) / conj_op;
BoolFactor =
TRUE | FALSE
| Expr RelaOp Expr / rela_op
| NOT BoolFactor / not_op
| LBRAC BoolExpr RBRAC
;

$ Первому терму в выражении может предшествовать знак минус
Expr = (Term | MINUS Term / neg_op) %rep ((PLUS | MINUS) Term / chunk)
/ bin_op;
Term = Factor %rep ((STAR | FRAC) Factor / chunk) / bin_op;
Factor = NUMBER | VAR / deref | LBRAC Expr RBRAC;

%axiom
Program

%end
```
Семантический анализ
Для символов грамматики, определённых в секции %tokens, не должно быть 
правил в секции %grammar, т.к. они терминальные символы.
Для остальных (т.е. нетерминальных) символов должно быть ровно одно 
правило в секции %grammar.
Символ в секции %types может встречаться не более одного раза, если 
символ встречается, то будем называть его типизированным, иначе — нетипизированным.
Если альтернатива заканчивается на дробь и имя метода, то типы типизированных 
элементов альтернативы должны совпадать с типами аргументов метода, 
тип альтернативы будет совпадать с типом возвращаемого значения метода 
(если void — нетипизированная).
Если альтернатива не заканчивается на дробь и имя метода, то в ней должно 
быть не более одного типизированного элемента:
если таковой один, то его тип будет типом альтернативы,
если таковых нет, то альтернатива будет нетипизированной.
Все альтернативы в правиле должны иметь одинаковый тип, либо все быть нетипизированными.
Тип элемента альтернативы, предварённый префиксом %rep, является массивом.
В секции %methods имена методов не могут повторяться.
Все методы, вызываемые из правил, должны быть определены в секции %methods, 
все определённые методы должны хотя бы один раз вызываться в правилах.
Примечание. В образце есть семантические ошибки!

# Реализация

Файл `scanner.go`
```go
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

	if err := s.advance(); err != nil {
		s.current = -1
		s.eof = true
	}
	return s
}


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


func (s *Scanner) skipWhitespace() error {
	for !s.eof && unicode.IsSpace(s.current) {
		if err := s.advance(); err != nil {
			return err
		}
	}
	return nil
}


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


	ident := sb.String()
	if tag, ok := keywordTags[ident]; ok {
		return Token{Tag: tag, Coords: s.makeCoords(), Value: ident}, nil
	}

	nameCode := s.compiler.AddName(ident)
	return Token{Tag: IdentifierTag, Coords: s.makeCoords(), Value: nameCode}, nil
}

func (s *Scanner) NextToken() Token {
	for {
		if err := s.skipWhitespace(); err != nil {
			start := Position{line: s.line, pos: s.pos}
			s.compiler.AddMessage(true, &start, err.Error())
			return Token{Tag: EOFTag, Coords: s.makeCoords(), Value: nil}
		}

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

```

Файл `ast.go`
```go
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
```

Файл `parser.go`
```go
package parser

import (
	"Compilers/lab3.3/lexer"
	"fmt"
)

type Parser struct {
	tokens   []lexer.Token
	compiler *lexer.Compiler
	Current  lexer.Token
	pos      int
}

func NewParser(tokens []lexer.Token, compiler *lexer.Compiler) *Parser {
	return &Parser{
		tokens:   tokens,
		compiler: compiler,
		pos:      0,
		Current:  lexer.Token{Tag: lexer.EOFTag},
	}
}

func (p *Parser) NextToken() {
	p.pos++
	if p.pos < len(p.tokens) {
		p.Current = p.tokens[p.pos]
	} else {
		p.Current = lexer.Token{Tag: lexer.EOFTag}
	}
}

func (p *Parser) error(msg string) error {
	start := p.Current.Coords.Start
	errMsg := fmt.Sprintf("Parse error at %d:%d: %s", start.Line(), start.Pos(), msg)
	p.compiler.AddMessage(true, &start, msg)
	return fmt.Errorf(errMsg)
}

func (p *Parser) getIdentifierName() string {
	if p.Current.Tag == lexer.IdentifierTag {
		if nameCode, ok := p.Current.Value.(int); ok {
			return p.compiler.Names()[nameCode]
		}
	}
	return ""
}

func (p *Parser) Parse() (*Specification, error) {
	spec := &Specification{}
	if len(p.tokens) > 0 {
		p.Current = p.tokens[0]
		spec.Pos = p.Current.Coords
	}

	sections := []func(*Specification) error{
		p.parseClassDecl,
		p.parseTokensDecl,
		p.parseTypesDecl,
		p.parseMethodsDecl,
		p.parseGrammarDecl,
		p.parseAxiomDecl,
	}

	for _, parseSection := range sections {
		if err := parseSection(spec); err != nil {
			return spec, err
		}
	}

	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%end" {
		return spec, p.error("Expected %end")
	}
	p.NextToken()

	return spec, nil
}

func (p *Parser) parseClassDecl(spec *Specification) error {
	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%class" {
		return p.error("Expected %class")
	}
	p.NextToken()

	if p.Current.Tag != lexer.IdentifierTag {
		return p.error("Expected IDENTIFIER after %class")
	}
	spec.ClassName = p.getIdentifierName()
	p.NextToken()

	return nil
}

func (p *Parser) parseTokensDecl(spec *Specification) error {
	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%tokens" {
		return p.error("Expected %tokens")
	}
	p.NextToken()

	for p.Current.Tag == lexer.IdentifierTag {
		spec.Tokens = append(spec.Tokens, p.getIdentifierName())
		p.NextToken()
	}

	return nil
}

func (p *Parser) parseTypesDecl(spec *Specification) error {
	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%types" {
		return p.error("Expected %types")
	}
	p.NextToken()

	for p.Current.Tag == lexer.IdentifierTag {
		typeDecl, err := p.parseTypeDecl()
		if err != nil {
			return err
		}
		spec.Types = append(spec.Types, typeDecl)

		if p.Current.Tag == lexer.SemicolonTag {
			p.NextToken()
		} else {
			break
		}
	}

	return nil
}

func (p *Parser) parseTypeDecl() (TypeMapping, error) {
	var mapping TypeMapping
	var symbols []string
	mapping.Pos = p.Current.Coords

	if p.Current.Tag != lexer.IdentifierTag {
		return mapping, p.error("Expected IDENTIFIER in TypeDecl")
	}
	symbols = append(symbols, p.getIdentifierName())
	p.NextToken()

	for p.Current.Tag == lexer.CommaTag {
		p.NextToken()
		if p.Current.Tag != lexer.IdentifierTag {
			return mapping, p.error("Expected IDENTIFIER after comma")
		}
		symbols = append(symbols, p.getIdentifierName())
		p.NextToken()
	}

	if p.Current.Tag != lexer.ColonTag {
		return mapping, p.error("Expected ':' in TypeDecl")
	}
	p.NextToken()

	typeName, isArray, err := p.parseType()
	if err != nil {
		return mapping, err
	}

	mapping.Symbols = symbols
	mapping.TypeName = typeName
	mapping.IsArray = isArray
	return mapping, nil
}

func (p *Parser) parseType() (string, bool, error) {
	if p.Current.Tag != lexer.IdentifierTag {
		return "", false, p.error("Expected IDENTIFIER in Type")
	}
	typeName := p.getIdentifierName()
	p.NextToken()

	isArray := false
	if p.Current.Tag == lexer.LBracketTag {
		p.NextToken()
		if p.Current.Tag != lexer.RBracketTag {
			return "", false, p.error("Expected ']' after '['")
		}
		isArray = true
		p.NextToken()
	}

	return typeName, isArray, nil
}

func (p *Parser) parseMethodsDecl(spec *Specification) error {
	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%methods" {
		return p.error("Expected %methods")
	}
	p.NextToken()

	for p.Current.Tag == lexer.IdentifierTag {
		method, err := p.parseMethodDecl()
		if err != nil {
			return err
		}
		spec.Methods = append(spec.Methods, method)

		if p.Current.Tag == lexer.SemicolonTag {
			p.NextToken()
		} else {
			break
		}
	}

	return nil
}

func (p *Parser) parseMethodDecl() (Method, error) {
	var method Method
	method.Pos = p.Current.Coords

	returnType, isArray, err := p.parseType()
	if err != nil {
		return method, err
	}
	method.ReturnType = returnType
	method.IsArray = isArray

	if p.Current.Tag != lexer.IdentifierTag {
		return method, p.error("Expected method name")
	}
	method.Name = p.getIdentifierName()
	p.NextToken()

	if p.Current.Tag != lexer.LParTag {
		return method, p.error("Expected '(' in method declaration")
	}
	p.NextToken()

	if p.Current.Tag == lexer.IdentifierTag {
		params, err := p.parseParamList()
		if err != nil {
			return method, err
		}
		method.Params = params
	}

	if p.Current.Tag != lexer.RParTag {
		return method, p.error("Expected ')' in method declaration")
	}
	p.NextToken()

	return method, nil
}

func (p *Parser) parseParamList() ([]Parameter, error) {
	var params []Parameter

	typeName, isArray, err := p.parseType()
	if err != nil {
		return params, err
	}
	params = append(params, Parameter{TypeName: typeName, IsArray: isArray, Pos: p.Current.Coords})

	for p.Current.Tag == lexer.CommaTag {
		p.NextToken()
		typeName, isArray, err := p.parseType()
		if err != nil {
			return params, err
		}
		params = append(params, Parameter{TypeName: typeName, 
			IsArray: isArray, Pos: p.Current.Coords})
	}

	return params, nil
}

func (p *Parser) parseGrammarDecl(spec *Specification) error {
	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%grammar" {
		return p.error("Expected %grammar")
	}
	p.NextToken()

	for p.Current.Tag == lexer.IdentifierTag {
		rule, err := p.parseRule()
		if err != nil {
			return err
		}
		spec.Grammar = append(spec.Grammar, rule)

		if p.Current.Tag == lexer.SemicolonTag {
			p.NextToken()
		} else {
			break
		}
	}

	return nil
}

func (p *Parser) parseRule() (Rule, error) {
	var rule Rule
	rule.Pos = p.Current.Coords

	if p.Current.Tag != lexer.IdentifierTag {
		return rule, p.error("Expected rule name")
	}
	rule.Name = p.getIdentifierName()
	p.NextToken()

	if p.Current.Tag != lexer.EqualsTag {
		return rule, p.error("Expected '=' in rule definition")
	}
	p.NextToken()

	expr, action, err := p.parseProduction()
	if err != nil {
		return rule, err
	}

	rule.Production = expr
	rule.Action = action
	return rule, nil
}

func (p *Parser) parseProduction() (Expression, string, error) {
	expr, action, err := p.parseAlternative()
	if err != nil {
		return nil, "", err
	}

	if p.Current.Tag == lexer.BarTag {
		options := []Expression{}
		if action != "" {
			options = append(options, 
				MethodAction{MethodName: action, Expression: expr, Pos: p.Current.Coords})
		} else {
			options = append(options, expr)
		}

		for p.Current.Tag == lexer.BarTag {
			p.NextToken()
			altExpr, altAction, err := p.parseAlternative()
			if err != nil {
				return nil, "", err
			}
			if altAction != "" {
				options = append(options, 
					MethodAction{MethodName: altAction, 
						Expression: altExpr, Pos: p.Current.Coords})
			} else {
				options = append(options, altExpr)
			}
		}

		return Alternative{Options: options, Pos: p.Current.Coords}, "", nil
	}

	if action != "" {
		return MethodAction{MethodName: action, 
			Expression: expr, Pos: p.Current.Coords}, "", nil
	}

	return expr, "", nil
}

func (p *Parser) parseAlternative() (Expression, string, error) {
	var items []Expression
	coords := p.Current.Coords

	if p.Current.Tag == lexer.SlashTag {
		p.NextToken()
		if p.Current.Tag != lexer.IdentifierTag {
			return nil, "", p.error("Expected method name after '/'")
		}
		action := p.getIdentifierName()
		p.NextToken()

		return MethodAction{
			MethodName: action,
			Expression: Sequence{Items: []Expression{}, Pos: coords},
			Pos:        coords,
		}, "", nil
	}

	for p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%rep" ||
		p.Current.Tag == lexer.IdentifierTag ||
		p.Current.Tag == lexer.LParTag {

		expr, err := p.parseAltElement()
		if err != nil {
			return nil, "", err
		}
		items = append(items, expr)

		if p.Current.Tag == lexer.SlashTag {
			p.NextToken()
			if p.Current.Tag != lexer.IdentifierTag {
				return nil, "", p.error("Expected method name after '/'")
			}
			action := p.getIdentifierName()
			p.NextToken()

			var expr Expression
			if len(items) > 0 {
				expr = Sequence{Items: items, Pos: coords}
			} else {
				expr = Sequence{Items: []Expression{}, Pos: coords}
			}

			return MethodAction{
				MethodName: action,
				Expression: expr,
				Pos:        coords,
			}, "", nil
		}
	}

	if len(items) == 1 {
		return items[0], "", nil
	} else if len(items) > 0 {
		return Sequence{Items: items, Pos: coords}, "", nil
	}

	return Sequence{Items: []Expression{}, Pos: coords}, "", nil
}

func (p *Parser) parseAltElement() (Expression, error) {
	hasRep := p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%rep"
	coords := p.Current.Coords
	if hasRep {
		p.NextToken()
	}

	if p.Current.Tag == lexer.IdentifierTag {
		name := p.getIdentifierName()
		p.NextToken()

		var expr Expression
		if hasRep {
			expr = Repetition{Item: Terminal{Value: name, Pos: coords}, Pos: coords}
		} else {
			expr = Terminal{Value: name, Pos: coords}
		}
		return expr, nil
	}

	if p.Current.Tag == lexer.LParTag {
		p.NextToken()
		expr, _, err := p.parseProduction()
		if err != nil {
			return nil, err
		}

		if p.Current.Tag != lexer.RParTag {
			return nil, p.error("Expected ')'")
		}
		p.NextToken()

		if seq, ok := expr.(Sequence); ok && len(seq.Items) >= 2 && 
			seq.Items[0].(Terminal).Value == "COMMA" {
			if hasRep {
				return Repetition{Item: 
					Grouped{Expression: expr, Pos: coords}, Pos: coords}, nil
			}
			return Repetition{Item: 
				Grouped{Expression: expr, Pos: coords}, Pos: coords}, nil
		}

		if hasRep {
			return Repetition{Item: 
				Grouped{Expression: expr, Pos: coords}, Pos: coords}, nil
		}
		return Grouped{Expression: expr, Pos: coords}, nil
	}

	return nil, p.error("Expected IDENTIFIER or '('")
}

func (p *Parser) parseAxiomDecl(spec *Specification) error {
	if p.Current.Tag != lexer.KeywordTag || p.Current.Value != "%axiom" {
		return p.error("Expected %axiom")
	}
	p.NextToken()

	if p.Current.Tag != lexer.IdentifierTag {
		return p.error("Expected axiom name")
	}
	spec.Axiom = p.getIdentifierName()
	p.NextToken()

	return nil
}
```

Файл `semantic.go`
```go
package semantic

import (
	"Compilers/lab3.3/lexer"
	"Compilers/lab3.3/parser"
	"fmt"
)

type SemanticError struct {
	Msg string
	Pos lexer.Fragment
}

func (e SemanticError) Error() string {
	return fmt.Sprintf("semantic error at %s: %s", e.Pos.String(), e.Msg)
}

type Parameter struct {
	TypeName string
	IsArray  bool
}

type Symbol struct {
	Name      string
	Category  SymbolCategory
	Type      string
	IsArray   bool
	DefinedAt int
	Params    []Parameter
}

type SymbolCategory int

const (
	CatTerminal SymbolCategory = iota
	CatNonTerminal
	CatType
	CatMethod
)

type SymbolTable struct {
	Parent     *SymbolTable
	Symbols    map[string]map[SymbolCategory]Symbol
	OpenScopes []*SymbolTable
}

func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		Parent:     parent,
		Symbols:    make(map[string]map[SymbolCategory]Symbol),
		OpenScopes: []*SymbolTable{},
	}
}

func (st *SymbolTable) AddSymbol(sym Symbol) error {
	if _, exists := st.Symbols[sym.Name]; !exists {
		st.Symbols[sym.Name] = make(map[SymbolCategory]Symbol)
	}
	if _, exists := st.Symbols[sym.Name][sym.Category]; exists {
		return fmt.Errorf("symbol %s with category %v already defined", sym.Name, sym.Category)
	}
	st.Symbols[sym.Name][sym.Category] = sym
	return nil
}

func (st *SymbolTable) FindSymbol(name string, category SymbolCategory) (Symbol, bool) {
	if catSymbols, exists := st.Symbols[name]; exists {
		if sym, exists := catSymbols[category]; exists {
			return sym, true
		}
	}
	for _, openScope := range st.OpenScopes {
		if sym, exists := openScope.FindSymbol(name, category); exists {
			return sym, true
		}
	}
	if st.Parent != nil {
		return st.Parent.FindSymbol(name, category)
	}
	return Symbol{}, false
}

type Analyzer struct {
	spec        *parser.Specification
	global      *SymbolTable
	usedMethods map[string]bool
	ruleTables  map[string]*SymbolTable
}

func NewAnalyzer(spec *parser.Specification) *Analyzer {
	return &Analyzer{
		spec:        spec,
		global:      NewSymbolTable(nil),
		usedMethods: make(map[string]bool),
		ruleTables:  make(map[string]*SymbolTable),
	}
}

func (a *Analyzer) Analyze() (string, error) {
	if err := a.buildSymbolTables(); err != nil {
		return "", err
	}
	if err := a.markUsedMethods(); err != nil {
		return "", err
	}
	if err := a.checkTerminals(); err != nil {
		return "", err
	}
	if err := a.checkNonTerminals(); err != nil {
		return "", err
	}
	if err := a.checkTypes(); err != nil {
		return "", err
	}
	if err := a.checkGrammarRules(); err != nil {
		return "", err
	}
	if err := a.checkMethods(); err != nil {
		return "", err
	}
	return "Программа корректна", nil
}

func (a *Analyzer) buildSymbolTables() error {
	for i, token := range a.spec.Tokens {
		if err := a.global.AddSymbol(Symbol{
			Name:      token,
			Category:  CatTerminal,
			DefinedAt: i,
		}); err != nil {
			return SemanticError{Msg: err.Error(), Pos: a.spec.Pos}
		}
	}

	for _, typeMapping := range a.spec.Types {
		for _, symbol := range typeMapping.Symbols {
			if err := a.global.AddSymbol(Symbol{
				Name:     symbol,
				Category: CatType,
				Type:     typeMapping.TypeName,
				IsArray:  typeMapping.IsArray,
			}); err != nil {
				return SemanticError{Msg: err.Error(), Pos: typeMapping.Pos}
			}
		}
	}

	for _, method := range a.spec.Methods {
		params := make([]Parameter, len(method.Params))
		for i, param := range method.Params {
			params[i] = Parameter{TypeName: param.TypeName, IsArray: param.IsArray}
		}
		if err := a.global.AddSymbol(Symbol{
			Name:     method.Name,
			Category: CatMethod,
			Type:     method.ReturnType,
			IsArray:  method.IsArray,
			Params:   params,
		}); err != nil {
			return SemanticError{Msg: err.Error(), Pos: method.Pos}
		}
	}

	for _, rule := range a.spec.Grammar {
		if err := a.global.AddSymbol(Symbol{
			Name:      rule.Name,
			Category:  CatNonTerminal,
			DefinedAt: len(a.spec.Tokens) + len(a.spec.Types) + 
				len(a.spec.Methods) + len(a.spec.Grammar),
		}); err != nil {
			return SemanticError{Msg: err.Error(), Pos: rule.Pos}
		}
	}
	return nil
}

func (a *Analyzer) markUsedMethods() error {
	for _, rule := range a.spec.Grammar {
		if rule.Action != "" {
			a.usedMethods[rule.Action] = true
		}
		if err := a.markUsedMethodsInExpr(rule.Production); err != nil {
			return err
		}
	}
	return nil
}

func (a *Analyzer) markUsedMethodsInExpr(expr parser.Expression) error {
	switch e := expr.(type) {
	case parser.Sequence:
		for _, item := range e.Items {
			if err := a.markUsedMethodsInExpr(item); err != nil {
				return err
			}
		}
	case parser.Alternative:
		for _, opt := range e.Options {
			if err := a.markUsedMethodsInExpr(opt); err != nil {
				return err
			}
		}
	case parser.Repetition:
		if err := a.markUsedMethodsInExpr(e.Item); err != nil {
			return err
		}
	case parser.Grouped:
		if err := a.markUsedMethodsInExpr(e.Expression); err != nil {
			return err
		}
	case parser.MethodAction:
		a.usedMethods[e.MethodName] = true
		if err := a.markUsedMethodsInExpr(e.Expression); err != nil {
			return err
		}
	case parser.UnaryOp:
		if err := a.markUsedMethodsInExpr(e.Right); err != nil {
			return err
		}
	case parser.BinaryOp:
		if err := a.markUsedMethodsInExpr(e.Left); err != nil {
			return err
		}
		if err := a.markUsedMethodsInExpr(e.Right); err != nil {
			return err
		}
	case parser.MethodCall:
		a.usedMethods[e.MethodName] = true
		for _, arg := range e.Args {
			if err := a.markUsedMethodsInExpr(arg); err != nil {
				return err
			}
		}
	}
	return nil
}

func (a *Analyzer) checkTerminals() error {
	terminalRules := make(map[string]bool)
	for _, rule := range a.spec.Grammar {
		for _, token := range a.spec.Tokens {
			if rule.Name == token {
				terminalRules[token] = true
			}
		}
	}
	for terminal := range terminalRules {
		return SemanticError{
			Msg: fmt.Sprintf("terminal %s should not " +
				"have grammar rules", terminal),
			Pos: a.spec.Grammar[0].Pos,
		}
	}
	return nil
}

func (a *Analyzer) checkNonTerminals() error {
	ruleCount := make(map[string]int)
	nonTerminals := make(map[string]bool)

	for _, rule := range a.spec.Grammar {
		ruleCount[rule.Name]++
		nonTerminals[rule.Name] = true
	}

	for name, count := range ruleCount {
		if count != 1 {
			for _, rule := range a.spec.Grammar {
				if rule.Name == name {
					return SemanticError{
						Msg: fmt.Sprintf("non-terminal %s has " +
							"%d rules (should be exactly 1)", name, count),
						Pos: rule.Pos,
					}
				}
			}
		}
	}

	for _, rule := range a.spec.Grammar {
		if err := a.checkNonTerminalReferences(rule.Production, 
			nonTerminals); err != nil {
			return err
		}
	}

	return nil
}

func (a *Analyzer) checkNonTerminalReferences(expr parser.Expression,
	nonTerminals map[string]bool) error {
	switch e := expr.(type) {
	case parser.NonTerminal:
		if !nonTerminals[e.Name] {
			return SemanticError{
				Msg: fmt.Sprintf("non-terminal %s is referenced but has no rule", e.Name),
				Pos: e.Pos,
			}
		}
	case parser.Sequence:
		for _, item := range e.Items {
			if err := a.checkNonTerminalReferences(item, nonTerminals); err != nil {
				return err
			}
		}
	case parser.Alternative:
		for _, opt := range e.Options {
			if err := a.checkNonTerminalReferences(opt, nonTerminals); err != nil {
				return err
			}
		}
	case parser.Repetition:
		return a.checkNonTerminalReferences(e.Item, nonTerminals)
	case parser.Grouped:
		return a.checkNonTerminalReferences(e.Expression, nonTerminals)
	case parser.MethodAction:
		return a.checkNonTerminalReferences(e.Expression, nonTerminals)
	case parser.UnaryOp:
		return a.checkNonTerminalReferences(e.Right, nonTerminals)
	case parser.BinaryOp:
		if err := a.checkNonTerminalReferences(e.Left, nonTerminals); err != nil {
			return err
		}
		return a.checkNonTerminalReferences(e.Right, nonTerminals)
	case parser.MethodCall:
		for _, arg := range e.Args {
			if err := a.checkNonTerminalReferences(arg, nonTerminals); err != nil {
				return err
			}
		}
	}
	return nil
}

func (a *Analyzer) checkTypes() error {
	typeSymbols := make(map[string]int)
	for _, typeMapping := range a.spec.Types {
		for _, symbol := range typeMapping.Symbols {
			typeSymbols[symbol]++
		}
	}
	for symbol, count := range typeSymbols {
		if count > 1 {
			for _, typeMapping := range a.spec.Types {
				for _, sym := range typeMapping.Symbols {
					if sym == symbol {
						return SemanticError{
							Msg: fmt.Sprintf("symbol %s appears in %%types %d " +
								"times (should be at most 1)", symbol, count),
							Pos: typeMapping.Pos,
						}
					}
				}
			}
		}
	}
	return nil
}

func (a *Analyzer) checkMethods() error {
	methodNames := make(map[string]bool)
	for _, method := range a.spec.Methods {
		if methodNames[method.Name] {
			return SemanticError{
				Msg: fmt.Sprintf("method %s is defined multiple times", method.Name),
				Pos: method.Pos,
			}
		}
		methodNames[method.Name] = true
	}

	for _, method := range a.spec.Methods {
		if !a.usedMethods[method.Name] {
			return SemanticError{
				Msg: fmt.Sprintf("method %s is defined but never used", method.Name),
				Pos: method.Pos,
			}
		}
	}
	return nil
}

func (a *Analyzer) checkGrammarRules() error {
	for _, rule := range a.spec.Grammar {
		localTable := NewSymbolTable(a.global)
		if err := a.checkRule(rule, localTable); err != nil {
			return err
		}
		a.ruleTables[rule.Name] = localTable
	}
	return nil
}

func (a *Analyzer) checkRule(rule parser.Rule, table *SymbolTable) error {
	isMethodAction := rule.Action != ""
	typ, isArray, err := a.checkSemantics(rule.Production, table)
	if err != nil {
		return err
	}
	if isMethodAction {
		err = a.checkMethodCompatibility(rule.Name, typ, isArray, 
			rule.Action, rule.Production, table, rule.Pos)
		if err != nil {
			return err
		}
	}
	return nil
}

func (a *Analyzer) checkMethodCompatibility(ruleName string, ruleType string, 
	ruleIsArray bool, methodName string, expr parser.Expression, 
	table *SymbolTable, pos lexer.Fragment) error {
	methodSym, exists := table.FindSymbol(methodName, CatMethod)
	if !exists {
		return SemanticError{
			Msg: fmt.Sprintf("undefined method %s in rule %s", methodName, ruleName),
			Pos: pos,
		}
	}

	var typedElements []Parameter
	switch e := expr.(type) {
	case parser.MethodAction:
		if e.MethodName == methodName {
			typedElements = a.collectTypedElements(e.Expression, table)
		}
	case parser.Alternative:
		for _, opt := range e.Options {
			if seq, ok := opt.(parser.Sequence); ok {
				for _, item := range seq.Items {
					if ma, ok := item.(parser.MethodAction); 
					ok && ma.MethodName == methodName {
						for _, seqItem := range seq.Items {
							if seqItem == item {
								typedElements = append(typedElements, 
									a.collectTypedElements(ma.Expression, 
										table)...)
								break
							}
							typedElements = append(typedElements, 
								a.collectTypedElements(seqItem, table)...)
						}
						break
					}
				}
				if len(typedElements) > 0 {
					break
				}
			}
		}
	}
	if len(typedElements) == 0 {
		typedElements = a.collectTypedElements(expr, table)
	}

	if len(typedElements) != len(methodSym.Params) {
		return SemanticError{
			Msg: fmt.Sprintf("method %s in rule %s expects %d parameters, got %d",
				methodName, ruleName, len(methodSym.Params), len(typedElements)),
			Pos: pos,
		}
	}

	for i, param := range methodSym.Params {
		if i < len(typedElements) && !a.typesMatch(typedElements[i].TypeName, 
			typedElements[i].IsArray, param.TypeName, param.IsArray) {
			return SemanticError{
				Msg: fmt.Sprintf("parameter %d type mismatch in " +
					"method %s in rule %s: expected %s%s, got %s%s",
					i+1, methodName, ruleName, param.TypeName, 
					map[bool]string{true: "[]", false: ""}[param.IsArray],
					typedElements[i].TypeName, 
					map[bool]string{true: "[]", false: ""}[typedElements[i].IsArray]),
				Pos: pos,
			}
		}
	}

	if !a.typesMatch(ruleType, ruleIsArray, methodSym.Type, methodSym.IsArray) {
		return SemanticError{
			Msg: fmt.Sprintf("type mismatch in rule %s: expected %s%s, got %s%s",
				ruleName, methodSym.Type, 
				map[bool]string{true: "[]", false: ""}[methodSym.IsArray],
				ruleType, map[bool]string{true: "[]", false: ""}[ruleIsArray]),
			Pos: pos,
		}
	}
	return nil
}

func (a *Analyzer) getMethodReturnType(methodName string, table *SymbolTable) (string, bool) {
	methodSym, exists := table.FindSymbol(methodName, CatMethod)
	if !exists {
		return "", false
	}
	return methodSym.Type, methodSym.IsArray
}

func (a *Analyzer) collectTypedElements(expr parser.Expression, 
	table *SymbolTable) []Parameter {
	var typed []Parameter
	switch e := expr.(type) {
	case parser.Terminal:
		if typ, isArr := a.getSymbolType(e.Value, table); typ != "" {
			typed = append(typed, Parameter{TypeName: typ, IsArray: isArr})
		} else {
			for _, typeMapping := range a.spec.Types {
				for _, symbol := range typeMapping.Symbols {
					if symbol == e.Value {
						typed = append(typed, 
							Parameter{TypeName: typeMapping.TypeName, 
								IsArray: typeMapping.IsArray})
						break
					}
				}
			}
			for _, typeMapping := range a.spec.Types {
				if typeMapping.TypeName == e.Value {
					typed = append(typed, Parameter{TypeName: e.Value, IsArray: false})
					break
				}
			}
		}
	case parser.NonTerminal:
		if typ, isArr := a.getSymbolType(e.Name, table); typ != "" {
			typed = append(typed, Parameter{TypeName: typ, IsArray: isArr})
		}
	case parser.Sequence:
		for _, item := range e.Items {
			itemTyped := a.collectTypedElements(item, table)
			if len(itemTyped) > 0 {
				typed = append(typed, itemTyped...)
			}
		}
	case parser.Alternative:
		for i, opt := range e.Options {
			optTyped := a.collectTypedElements(opt, table)
			if len(optTyped) > 0 {
				if i == 0 {
					typed = append(typed, optTyped[0])
					break
				}
			}
		}
	case parser.Repetition:
		itemTyped := a.collectTypedElements(e.Item, table)
		for _, t := range itemTyped {
			typed = append(typed, Parameter{TypeName: t.TypeName, IsArray: true})
		}
	case parser.Grouped:
		typed = append(typed, a.collectTypedElements(e.Expression, table)...)
	case parser.UnaryOp:
		typed = append(typed, a.collectTypedElements(e.Right, table)...)
	case parser.BinaryOp:
		typed = append(typed, a.collectTypedElements(e.Left, table)...)
		typed = append(typed, a.collectTypedElements(e.Right, table)...)
	case parser.MethodCall:
		for _, arg := range e.Args {
			typed = append(typed, a.collectTypedElements(arg, table)...)
		}
	case parser.MethodAction:
		if typ, isArr := a.getMethodReturnType(e.MethodName, table); typ != "" {
			typed = append(typed, Parameter{TypeName: typ, IsArray: isArr})
		}
	}
	return typed
}

func (a *Analyzer) checkSemantics(expr parser.Expression, 
	table *SymbolTable) (string, bool, error) {
	switch e := expr.(type) {
	case parser.Terminal:
		if _, exists := table.FindSymbol(e.Value, CatNonTerminal); exists {
			return a.checkNonTerminalSemantics(
				parser.NonTerminal{Name: e.Value, Pos: e.Pos}, table)
		}
		return a.checkTerminalSemantics(e, table)
	case parser.NonTerminal:
		return a.checkNonTerminalSemantics(e, table)
	case parser.Sequence:
		return a.checkSequenceSemantics(e, table, false)
	case parser.Alternative:
		return a.checkAlternativeSemantics(e, table)
	case parser.Repetition:
		return a.checkRepetitionSemantics(e, table)
	case parser.Grouped:
		return a.checkGroupedSemantics(e, table)
	case parser.MethodAction:
		return a.checkMethodActionSemantics(e, table)
	case parser.UnaryOp:
		return a.checkUnaryOpSemantics(e, table)
	case parser.BinaryOp:
		return a.checkBinaryOpSemantics(e, table)
	case parser.MethodCall:
		return a.checkMethodCallSemantics(e, table)
	default:
		return "", false, SemanticError{
			Msg: "unknown expression type",
			Pos: expr.GetPos(),
		}
	}
}

func (a *Analyzer) checkTerminalSemantics(t parser.Terminal, 
	table *SymbolTable) (string, bool, error) {
	_, exists := table.FindSymbol(t.Value, CatTerminal)
	if !exists {
		return "", false, SemanticError{
			Msg: fmt.Sprintf("undefined terminal %s", t.Value),
			Pos: t.Pos,
		}
	}
	typ, isArray := a.getSymbolType(t.Value, table)
	return typ, isArray, nil
}

func (a *Analyzer) checkNonTerminalSemantics(nt parser.NonTerminal, 
	table *SymbolTable) (string, bool, error) {
	_, exists := table.FindSymbol(nt.Name, CatNonTerminal)
	if !exists {
		return "", false, SemanticError{
			Msg: fmt.Sprintf("undefined non-terminal %s", nt.Name),
			Pos: nt.Pos,
		}
	}
	typ, isArray := a.getSymbolType(nt.Name, table)
	return typ, isArray, nil
}

func (a *Analyzer) checkSequenceSemantics(s parser.Sequence, 
	table *SymbolTable, isMethodAction bool) (string, bool, error) {
	var lastType string
	var lastIsArray bool
	typedCount := 0
	for _, item := range s.Items {
		typ, isArr, err := a.checkSemantics(item, table)
		if err != nil {
			return "", false, err
		}
		if typ != "" {
			lastType = typ
			lastIsArray = isArr
			typedCount++
		}
	}
	if !isMethodAction && typedCount > 1 {
		return "", false, SemanticError{
			Msg: "sequence has more than one typed element",
			Pos: s.Pos,
		}
	}
	return lastType, lastIsArray, nil
}

func (a *Analyzer) checkAlternativeSemantics(alt parser.Alternative,
	table *SymbolTable) (string, bool, error) {
	if len(alt.Options) == 0 {
		return "", false, SemanticError{
			Msg: "empty alternative",
			Pos: alt.Pos,
		}
	}
	var firstType string
	var firstIsArray bool
	for i, opt := range alt.Options {
		typ, isArr, err := a.checkSemantics(opt, table)
		if err != nil {
			return "", false, err
		}
		if _, isMethodAction := opt.(parser.MethodAction); !isMethodAction {
			typedElements := a.collectTypedElements(opt, table)
			if len(typedElements) > 1 {
				return "", false, SemanticError{
					Msg: "alternative option has more than one typed element",
					Pos: alt.Pos,
				}
			}
		}
		if i == 0 {
			firstType = typ
			firstIsArray = isArr
		} else if !a.typesMatch(firstType, firstIsArray, typ, isArr) {
			return "", false, SemanticError{
				Msg: "all alternatives must have the same type",
				Pos: alt.Pos,
			}
		}
	}
	return firstType, firstIsArray, nil
}

func (a *Analyzer) checkRepetitionSemantics(p parser.Repetition, table *SymbolTable) (string, bool, error) {
	typ, _, err := a.checkSemantics(p.Item, table)
	if err != nil {
		return "", false, err
	}
	return typ, true, nil
}

func (a *Analyzer) checkGroupedSemantics(g parser.Grouped, table *SymbolTable) (string, bool, error) {
	return a.checkSemantics(g.Expression, table)
}

func (a *Analyzer) checkMethodActionSemantics(m parser.MethodAction, 
	table *SymbolTable) (string, bool, error) {
	methodSym, exists := table.FindSymbol(m.MethodName, CatMethod)
	if !exists {
		return "", false, SemanticError{
			Msg: fmt.Sprintf("undefined method %s", m.MethodName),
			Pos: m.Pos,
		}
	}
	typedElements := a.collectTypedElements(m.Expression, table)
	if len(typedElements) != len(methodSym.Params) {
		return "", false, SemanticError{
			Msg: fmt.Sprintf("method %s expects %d parameters, got %d", 
				m.MethodName, len(methodSym.Params), len(typedElements)),
			Pos: m.Pos,
		}
	}
	for i, param := range methodSym.Params {
		if i < len(typedElements) && !a.typesMatch(typedElements[i].TypeName, 
			typedElements[i].IsArray, param.TypeName, param.IsArray) {
			return "", false, SemanticError{
				Msg: fmt.Sprintf("parameter %d type mismatch in " +
					"method %s: expected %s%s, got %s%s",
					i+1, m.MethodName, param.TypeName, 
					map[bool]string{true: "[]", false: ""}[param.IsArray],
					typedElements[i].TypeName, 
					map[bool]string{true: "[]", false: ""}[typedElements[i].IsArray]),
				Pos: m.Pos,
			}
		}
	}
	return methodSym.Type, methodSym.IsArray, nil
}

func (a *Analyzer) checkUnaryOpSemantics(u parser.UnaryOp, 
	table *SymbolTable) (string, bool, error) {
	typ, isArr, err := a.checkSemantics(u.Right, table)
	if err != nil {
		return "", false, err
	}
	return typ, isArr, nil
}

func (a *Analyzer) checkBinaryOpSemantics(b parser.BinaryOp, table *SymbolTable) (string, bool, error) {
	leftType, leftIsArray, err := a.checkSemantics(b.Left, table)
	if err != nil {
		return "", false, err
	}
	rightType, rightIsArray, err := a.checkSemantics(b.Right, table)
	if err != nil {
		return "", false, err
	}
	if !a.typesMatch(leftType, leftIsArray, rightType, rightIsArray) {
		return "", false, SemanticError{
			Msg: "type mismatch in binary operation",
			Pos: b.Pos,
		}
	}
	return leftType, leftIsArray, nil
}

func (a *Analyzer) checkMethodCallSemantics(m parser.MethodCall, table *SymbolTable) (string, bool, error) {
	methodSym, exists := table.FindSymbol(m.MethodName, CatMethod)
	if !exists {
		return "", false, SemanticError{
			Msg: fmt.Sprintf("undefined method %s", m.MethodName),
			Pos: m.Pos,
		}
	}
	typedElements := make([]Parameter, 0, len(m.Args))
	for _, arg := range m.Args {
		typ, isArr, err := a.checkSemantics(arg, table)
		if err != nil {
			return "", false, err
		}
		if typ != "" {
			typedElements = append(typedElements, Parameter{TypeName: typ, IsArray: isArr})
		}
	}
	if len(typedElements) != len(methodSym.Params) {
		return "", false, SemanticError{
			Msg: fmt.Sprintf("method %s expects %d parameters, " +
				"got %d", m.MethodName, len(methodSym.Params), len(typedElements)),
			Pos: m.Pos,
		}
	}
	for i, param := range methodSym.Params {
		if i < len(typedElements) && !a.typesMatch(typedElements[i].TypeName, 
			typedElements[i].IsArray, param.TypeName, param.IsArray) {
			return "", false, SemanticError{
				Msg: fmt.Sprintf("parameter %d type mismatch in " +
					"method %s: expected %s%s, got %s%s",
					i+1, m.MethodName, param.TypeName, 
					map[bool]string{true: "[]", false: ""}[param.IsArray],
					typedElements[i].TypeName, 
					map[bool]string{true: "[]", false: ""}[typedElements[i].IsArray]),
				Pos: m.Pos,
			}
		}
	}
	return methodSym.Type, methodSym.IsArray, nil
}

func (a *Analyzer) getSymbolType(name string, table *SymbolTable) (string, bool) {
	sym, exists := table.FindSymbol(name, CatType)
	if !exists {
		return "", false
	}
	return sym.Type, sym.IsArray
}

func (a *Analyzer) isTerminal(symbol string) bool {
	_, exists := a.global.FindSymbol(symbol, CatTerminal)
	return exists
}

func (a *Analyzer) isNonTerminal(symbol string) bool {
	_, exists := a.global.FindSymbol(symbol, CatNonTerminal)
	return exists
}

func (a *Analyzer) typesMatch(type1 string, isArray1 bool, type2 string, isArray2 bool) bool {
	if type1 == "" || type2 == "" {
		return true
	}
	return type1 == type2 && isArray1 == isArray2
}
```

Файл `main.go`
```go
package main

import (
	"Compilers/lab3.3/lexer"
	"Compilers/lab3.3/parser"
	"Compilers/lab3.3/semantic"
	"fmt"
	"os"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Printf("Error opening file: %v\n", err)
		return
	}
	defer file.Close()

	comp := lexer.NewCompiler()
	scan := lexer.NewScanner(file, comp)

	var tokens []lexer.Token
	for {
		token := scan.NextToken()
		/*
			fmt.Printf("%s %s", lexer.TagToString(token.Tag), token.Coords)
			if token.Tag == lexer.IdentifierTag {
				nameCode := token.Value.(int)
				name := comp.Names()[nameCode]
				fmt.Printf(": %s", name)
			} else if token.Value != nil {
				fmt.Printf(": %v", token.Value)
			}
			fmt.Println()

		*/
		tokens = append(tokens, token)
		if token.Tag == lexer.EOFTag {
			break
		}
	}

	for e := comp.Messages.Front(); e != nil; e = e.Next() {
		fmt.Println(e.Value)
	}

	p := parser.NewParser(tokens, comp)
	spec, err := p.Parse()
	if err != nil {
		fmt.Println("Parse error:", err)
		fmt.Println(p.Current)
		for e := comp.Messages.Front(); e != nil; e = e.Next() {
			fmt.Println(e.Value)
		}
		os.Exit(1)
	}

	fmt.Println("\nParsed Specification:")
	parser.PrintSpecification(spec)

	fmt.Println("\nRunning semantic analysis...")
	analyzer := semantic.NewAnalyzer(spec)
	message, err := analyzer.Analyze()
	if err != nil {
		fmt.Println("\nSemantic error found:")
		fmt.Println("-", err)
		os.Exit(1)
	} else {
		fmt.Println(message)
		fmt.Println("No semantic errors found!")
	}

}
```

# Тестирование

## Входные данные

Файл `input.txt`
```
%class
  SimpleImperativeLang

%tokens
  NUMBER PLUS MINUS STAR FRAC LBRAC RBRAC
  TRUE FALSE ADD OR NOT LT GT LE GE NE EQ
  IF THEN ELSE END WHILE DO SEMICOLON
  VAR ASSIGN INPUT PRINT COMMA

%types
  Expr, Term, Factor, NUMBER: ArithmExpr;
  PLUS, MINUS, STAR, FRAC: ArithmOp;
  BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE: BoolExpr;
  LT, GT, LE, GE, NE, EQ: RelaOp;
  Program, Statement, StatementList, Program: Statement;
  VAR, STRING: String;
  PrintItem: PrintItem;

%methods
  ArithmExpr neg_op(ArithmOp, ArithmExpr);
  ArithmExprChunk chunk(ArithmOp, ArithmExpr);
  ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[]);
  ArithmExpr deref(String);

  BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr);
  BoolExpr disj_op(BoolExpr, BoolExpr[]);
  BoolExpr conj_op(BoolExpr, BoolExpr[]);
  BoolExpr not_op(BoolExpr);

  Statement assign_stmt(String, ArithmExpr);
  Statement append(Statement, Statement);
  $ для упрощения описания языка считаем последовательность операторов
  $ оператором
  Statement compound(Statement, Statement[]);
  Statement if_else_stmt(BoolExpr, Statement, Statement);
  Statement empty_stmt();
  Statement while_stmt(BoolExpr, Statement);
  Statement input_stmt(String, String[]);

  PrintItem print_value(ArithmExpr);
  PrintItem print_string(String);
  Statement print_stmt(PrintItem, PrintItem[]);

%grammar
  Program = StatementList;

  StatementList = Statement %rep (SEMICOLON Statement) / compound;

  Statement =
      VAR ASSIGN Expr / assign_stmt
      $ Ветка else может отсутствовать
    | IF BoolExpr THEN StatementList (/ empty_stmt | ELSE StatementList) END
      / if_else_stmt
    | WHILE BoolExpr DO StatementList END / while_stmt
    | INPUT VAR %rep (COMMA VAR) / input_stmt
    | PRINT PrintItem (COMMA PrintItem) / print_stmt
    ;

  PrintItem = Expr / print_value | STRING / print_string;

  BoolExpr = BoolTerm %rep (OR BoolTerm) / disj_op;
  BoolTerm = BoolFactor %rep (AND BoolFactor) / conj_op;
  BoolFactor =
      TRUE | FALSE
    | Expr RelaOp Expr / rela_op
    | NOT BoolFactor / not_op
    | LBRAC BoolExpr RBRAC
    ;

  $ Первому терму в выражении может предшествовать знак минус
  Expr = (Term | MINUS Term / neg_op) %rep ((PLUS | MINUS) Term / chunk)
      / bin_op;
  Term = Factor %rep ((STAR | FRAC) Factor / chunk) / bin_op;
  Factor = NUMBER | VAR / deref | LBRAC Expr RBRAC;

%axiom
  Program

%end
```

## Вывод на `stdout`

```
Parsed Specification:
ClassName: SimpleImperativeLang (at (1,2)-(2,1))
Tokens: NUMBER, PLUS, MINUS, STAR, FRAC, LBRAC, RBRAC, TRUE, FALSE, ADD, OR, 
NOT, LT, GT, LE, GE, NE, EQ, IF, THEN, ELSE, END, WHILE, DO, SEMICOLON, VAR, ASSIGN, INPUT, PRINT, COMMA
Types:
  Symbols: Expr, Term, Factor, NUMBER, TypeName: ArithmExpr (at (11,4)-(11,8))
  Symbols: PLUS, MINUS, STAR, FRAC, TypeName: ArithmOp (at (12,4)-(12,8))
  Symbols: BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE, TypeName: BoolExpr (at (13,4)-(13,12))
  Symbols: LT, GT, LE, GE, NE, EQ, TypeName: RelaOp (at (14,4)-(14,6))
  Symbols: Program, Statement, StatementList, Program, TypeName: Statement (at (15,4)-(15,11))
  Symbols: VAR, STRING, TypeName: String (at (16,4)-(16,7))
  Symbols: PrintItem, TypeName: PrintItem (at (17,4)-(17,13))
Methods:
  ArithmExpr neg_op(ArithmOp, ArithmExpr) (at (20,4)-(20,14))
  ArithmExprChunk chunk(ArithmOp, ArithmExpr) (at (21,4)-(21,19))
  ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[]) (at (22,4)-(22,14))
  ArithmExpr deref(String) (at (23,4)-(23,14))
  BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr) (at (25,4)-(25,12))
  BoolExpr disj_op(BoolExpr, BoolExpr[]) (at (26,4)-(26,12))
  BoolExpr conj_op(BoolExpr, BoolExpr[]) (at (27,4)-(27,12))
  BoolExpr not_op(BoolExpr) (at (28,4)-(28,12))
  Statement assign_stmt(String, ArithmExpr) (at (30,4)-(30,13))
  Statement append(Statement, Statement) (at (31,4)-(31,13))
  Statement compound(Statement, Statement[]) (at (34,4)-(34,13))
  Statement if_else_stmt(BoolExpr, Statement, Statement) (at (35,4)-(35,13))
  Statement empty_stmt() (at (36,4)-(36,13))
  Statement while_stmt(BoolExpr, Statement) (at (37,4)-(37,13))
  Statement input_stmt(String, String[]) (at (38,4)-(38,13))
  PrintItem print_value(ArithmExpr) (at (40,4)-(40,13))
  PrintItem print_string(String) (at (41,4)-(41,13))
  Statement print_stmt(PrintItem, PrintItem[]) (at (42,4)-(42,13))
Grammar:
  Program = Terminal{StatementList} (at (45,14)-(45,27)) (at (45,4)-(45,11))
  StatementList = MethodAction{
      Method: compound,
      Expr: Sequence{
      Terminal{Statement} (at (47,20)-(47,29)),
      Repetition{
        Grouped(
          Sequence{
            Terminal{SEMICOLON} (at (47,36)-(47,45)),
            Terminal{Statement} (at (47,46)-(47,55))
          } (at (47,36)-(47,45))
        ) (at (47,30)-(47,34))
      } (at (47,30)-(47,34))
    } (at (47,20)-(47,29))
  } (at (47,20)-(47,29)) (at (47,4)-(47,17))
  Statement = Alternative{
        MethodAction{
        Method: assign_stmt,
        Expr: Sequence{
        Terminal{VAR} (at (50,8)-(50,11)),
        Terminal{ASSIGN} (at (50,12)-(50,18)),
        Terminal{Expr} (at (50,19)-(50,23))
      } (at (50,8)-(50,11))
    } (at (50,8)-(50,11))
    | MethodAction{
        Method: if_else_stmt,
        Expr: Sequence{
        Terminal{IF} (at (52,8)-(52,10)),
        Terminal{BoolExpr} (at (52,11)-(52,19)),
        Terminal{THEN} (at (52,20)-(52,24)),
        Terminal{StatementList} (at (52,25)-(52,38)),
        Grouped(
          Alternative{
                        MethodAction{
                Method: empty_stmt,
                Expr: Sequence{
                
              } (at (52,40)-(52,41))
            } (at (52,40)-(52,41))
            | Sequence{
              Terminal{ELSE} (at (52,55)-(52,59)),
              Terminal{StatementList} (at (52,60)-(52,73))
            } (at (52,55)-(52,59))
          } (at (52,73)-(52,74))
        ) (at (52,39)-(52,40)),
        Terminal{END} (at (52,75)-(53,1))
      } (at (52,8)-(52,10))
    } (at (52,8)-(52,10))
    | MethodAction{
        Method: while_stmt,
        Expr: Sequence{
        Terminal{WHILE} (at (54,8)-(54,13)),
        Terminal{BoolExpr} (at (54,14)-(54,22)),
        Terminal{DO} (at (54,23)-(54,25)),
        Terminal{StatementList} (at (54,26)-(54,39)),
        Terminal{END} (at (54,40)-(54,43))
      } (at (54,8)-(54,13))
    } (at (54,8)-(54,13))
    | MethodAction{
        Method: input_stmt,
        Expr: Sequence{
        Terminal{INPUT} (at (55,8)-(55,13)),
        Terminal{VAR} (at (55,14)-(55,17)),
        Repetition{
          Grouped(
            Sequence{
              Terminal{COMMA} (at (55,24)-(55,29)),
              Terminal{VAR} (at (55,30)-(55,33))
            } (at (55,24)-(55,29))
          ) (at (55,18)-(55,22))
        } (at (55,18)-(55,22))
      } (at (55,8)-(55,13))
    } (at (55,8)-(55,13))
    | MethodAction{
        Method: print_stmt,
        Expr: Sequence{
        Terminal{PRINT} (at (56,8)-(56,13)),
        Terminal{PrintItem} (at (56,14)-(56,23)),
        Repetition{
          Grouped(
            Sequence{
              Terminal{COMMA} (at (56,25)-(56,30)),
              Terminal{PrintItem} (at (56,31)-(56,40))
            } (at (56,25)-(56,30))
          ) (at (56,24)-(56,25))
        } (at (56,24)-(56,25))
      } (at (56,8)-(56,13))
    } (at (56,8)-(56,13))
  } (at (57,6)-(58,1)) (at (49,4)-(49,13))
  PrintItem = Alternative{
        MethodAction{
        Method: print_value,
        Expr: Sequence{
        Terminal{Expr} (at (59,16)-(59,20))
      } (at (59,16)-(59,20))
    } (at (59,16)-(59,20))
    | MethodAction{
        Method: print_string,
        Expr: Sequence{
        Terminal{STRING} (at (59,37)-(59,43))
      } (at (59,37)-(59,43))
    } (at (59,37)-(59,43))
  } (at (59,58)-(60,1)) (at (59,4)-(59,13))
  BoolExpr = MethodAction{
      Method: disj_op,
      Expr: Sequence{
      Terminal{BoolTerm} (at (61,15)-(61,23)),
      Repetition{
        Grouped(
          Sequence{
            Terminal{OR} (at (61,30)-(61,32)),
            Terminal{BoolTerm} (at (61,33)-(61,41))
          } (at (61,30)-(61,32))
        ) (at (61,24)-(61,28))
      } (at (61,24)-(61,28))
    } (at (61,15)-(61,23))
  } (at (61,15)-(61,23)) (at (61,4)-(61,12))
  BoolTerm = MethodAction{
      Method: conj_op,
      Expr: Sequence{
      Terminal{BoolFactor} (at (62,15)-(62,25)),
      Repetition{
        Grouped(
          Sequence{
            Terminal{AND} (at (62,32)-(62,35)),
            Terminal{BoolFactor} (at (62,36)-(62,46))
          } (at (62,32)-(62,35))
        ) (at (62,26)-(62,30))
      } (at (62,26)-(62,30))
    } (at (62,15)-(62,25))
  } (at (62,15)-(62,25)) (at (62,4)-(62,12))
  BoolFactor = Alternative{
        Terminal{TRUE} (at (64,8)-(64,12))
    | Terminal{FALSE} (at (64,15)-(65,1))
    | MethodAction{
        Method: rela_op,
        Expr: Sequence{
        Terminal{Expr} (at (65,8)-(65,12)),
        Terminal{RelaOp} (at (65,13)-(65,19)),
        Terminal{Expr} (at (65,20)-(65,24))
      } (at (65,8)-(65,12))
    } (at (65,8)-(65,12))
    | MethodAction{
        Method: not_op,
        Expr: Sequence{
        Terminal{NOT} (at (66,8)-(66,11)),
        Terminal{BoolFactor} (at (66,12)-(66,22))
      } (at (66,8)-(66,11))
    } (at (66,8)-(66,11))
    | Sequence{
      Terminal{LBRAC} (at (67,8)-(67,13)),
      Terminal{BoolExpr} (at (67,14)-(67,22)),
      Terminal{RBRAC} (at (67,23)-(68,1))
    } (at (67,8)-(67,13))
  } (at (68,6)-(69,1)) (at (63,4)-(63,14))
  Expr = MethodAction{
      Method: bin_op,
      Expr: Sequence{
      Grouped(
        Alternative{
                    Terminal{Term} (at (71,12)-(71,16))
          | MethodAction{
              Method: neg_op,
              Expr: Sequence{
              Terminal{MINUS} (at (71,19)-(71,24)),
              Terminal{Term} (at (71,25)-(71,29))
            } (at (71,19)-(71,24))
          } (at (71,19)-(71,24))
        } (at (71,38)-(71,39))
      ) (at (71,11)-(71,12)),
      Repetition{
        Grouped(
          MethodAction{
              Method: chunk,
              Expr: Sequence{
              Grouped(
                Alternative{
                                    Terminal{PLUS} (at (71,47)-(71,51))
                  | Terminal{MINUS} (at (71,54)-(71,59))
                } (at (71,59)-(71,60))
              ) (at (71,46)-(71,47)),
              Terminal{Term} (at (71,61)-(71,65))
            } (at (71,46)-(71,47))
          } (at (71,46)-(71,47))
        ) (at (71,40)-(71,44))
      } (at (71,40)-(71,44))
    } (at (71,11)-(71,12))
  } (at (71,11)-(71,12)) (at (71,4)-(71,8))
  Term = MethodAction{
      Method: bin_op,
      Expr: Sequence{
      Terminal{Factor} (at (73,11)-(73,17)),
      Repetition{
        Grouped(
          MethodAction{
              Method: chunk,
              Expr: Sequence{
              Grouped(
                Alternative{
                                    Terminal{STAR} (at (73,25)-(73,29))
                  | Terminal{FRAC} (at (73,32)-(73,36))
                } (at (73,36)-(73,37))
              ) (at (73,24)-(73,25)),
              Terminal{Factor} (at (73,38)-(73,44))
            } (at (73,24)-(73,25))
          } (at (73,24)-(73,25))
        ) (at (73,18)-(73,22))
      } (at (73,18)-(73,22))
    } (at (73,11)-(73,17))
  } (at (73,11)-(73,17)) (at (73,4)-(73,8))
  Factor = Alternative{
        Terminal{NUMBER} (at (74,13)-(74,19))
    | MethodAction{
        Method: deref,
        Expr: Sequence{
        Terminal{VAR} (at (74,22)-(74,25))
      } (at (74,22)-(74,25))
    } (at (74,22)-(74,25))
    | Sequence{
      Terminal{LBRAC} (at (74,36)-(74,41)),
      Terminal{Expr} (at (74,42)-(74,46)),
      Terminal{RBRAC} (at (74,47)-(74,52))
    } (at (74,36)-(74,41))
  } (at (74,52)-(75,1)) (at (74,4)-(74,10))
Axiom: Program (at (1,2)-(2,1))

Running semantic analysis...

Semantic error found:
- semantic error at (15,4)-(15,11): symbol Program with category 2 already defined
exit status 1
```

# Вывод
В ходе выполнения лабораторной работы был успешно реализован семантический 
анализатор для языка спецификации генератора синтаксических анализаторов. 
Проведённый анализ включал проверку корректности типов, правил грамматики, 
методов и их совместимости, а также выявление неиспользуемых методов. 
В результате анализа входных данных все семантические 
ошибки были обнаружены. Это подтверждает 
корректность реализации анализатора. Работа позволила получить практические навыки 
семантического анализа.