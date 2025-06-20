% Лабораторная работа № 2.4 «Рекурсивный спуск»
% 6 мая 2025 г.
% 

# Цель работы
Целью данной работы является изучение алгоритмов построения парсеров методом рекурсивного спуска.

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
Комментарии начинаются на знак доллара и продолжаются до конца строки.

Имена терминалов, нетерминалов, типов и методов — идентификаторы, начинающиеся с буквы и состоящие из букв, 
цифр и знаков прочерка.

Секция методов содержит сигнатуры методов в стиле языка Java без указания имён параметров. Типы могут быть 
простыми или массивами.

Каждое правило грамматики состоит из нескольких альтернатив. Альтернатива может завершаться необязательным 
именем метода, предварённого знаком дробной черты, который вызывается после её разбора.

Элементами альтернативы могут быть имена терминальных и нетерминальных символов, либо вложенные (безымянные) 
правила. Вложенные правила, как и обычные правила, могут 
состоять из нескольких альтернатив, каждая из которых 
может завершаться знаком дробной черты и именем метода.

Перед элементом альтернативы может располагаться ключевое слово %rep, 
означающее повторение соответствующего элемента.

Ключевые слова и идентификаторы чувствительны к регистру.

# Реализация

## Лексическая структура
```
IDENT       = r"[a-zA-Z][a-zA-Z0-9\-]*"
KEYWORD     = r"%class|%tokens|%types|%methods|%grammar|%axiom|%end|%rep"
COMMENT     = r"\$[^\n]*"  
TYPE        = r"[a-zA-Z][a-zA-Z0-9\-]*(\[\])?"
LPAREN      = r"\("
RPAREN      = r"\)"
COMMA       = r","
SEMICOLON   = r";"
COLON       = r":"
EQUAL       = r"="
BAR        = r"\|"
SLASH       = r"/"
WHITESPACE  = r"[ \t]+"
NEWLINE     = r"\r?\n"
```

## Грамматика языка
```
SpecFile        ::= SectionList "%end"
SectionList     ::= Section SectionList | ε
Section         ::= ClassDecl
| TokensDecl
| TypesDecl
| MethodsDecl
| GrammarDecl
| AxiomDecl
ClassDecl       ::= "%class" IDENT
TokensDecl      ::= "%tokens" TokenList
TokenList       ::= IDENT TokenList | ε
TypesDecl       ::= "%types" TypeList
TypeList        ::= TypeMapping SEMICOLON TypeList | ε
TypeMapping     ::= IdentList ":" TYPE
IdentList       ::= IDENT IdentRest
IdentRest       ::= COMMA IDENT IdentRest | ε
MethodsDecl     ::= "%methods" MethodList
MethodList      ::= MethodDecl SEMICOLON MethodList | ε
MethodDecl      ::= TYPE IDENT "(" ParamList? ")"
ParamList       ::= Param ParamRest
ParamRest       ::= COMMA Param ParamRest | ε
Param           ::= TYPE
GrammarDecl     ::= "%grammar" RuleList
RuleList        ::= Rule RuleList | ε
Rule            ::= IDENT "=" Production SEMICOLON
Production      ::= AltProduction Action?
AltProduction   ::= Sequence AltRest
AltRest         ::= BAR Sequence AltRest | ε
Sequence        ::= Item Sequence | ε
Item            ::= Terminal | Group | Repetition
Group           ::= LPAREN Production RPAREN
Repetition      ::= "%rep" Group Action?
Terminal        ::= IDENT
Action          ::= SLASH IDENT
AxiomDecl       ::= "%axiom" IDENT
Comment         ::= "$" .*? NEWLINE
```

## Программная реализация

Файл `compiler.go`
```go
package lexer

import (
	"container/list"
	"fmt"
)

type Compiler struct {
	Messages  *list.List
	nameTable map[string]int
	names     []string
}

func NewCompiler() *Compiler {
	return &Compiler{
		Messages:  list.New(),
		nameTable: make(map[string]int),
		names:     make([]string, 0),
	}
}

func (c *Compiler) AddMessage(isError bool, pos *Position, text string) {
	c.Messages.PushBack(fmt.Sprintf("%s (%d,%d): %s",
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

func (c *Compiler) Names() []string {
	return c.names
}
```

Файл `domain.go`
```go
package lexer

type DomainTag int

const (
	KeywordTag DomainTag = iota
	IdentifierTag
	SemicolonTag
	CommaTag
	ColonTag
	EqualsTag
	SlashTag
	BarTag
	LParTag
	RParTag
	LBracketTag
	RBracketTag
	EOFTag
	ErrorTag
)

func TagToString(tag DomainTag) string {
	return [...]string{
		"Keyword",
		"Identifier",
		"Semicolon",
		"Comma",
		"Colon",
		"Equals",
		"Slash",
		"Bar",
		"LPar",
		"RPar",
		"LBracket",
		"RBracket",
		"EOF",
		"Error",
	}[tag]
}
```

Файл `position.go`
```go
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
```

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
```

Файл `token.go`
```go
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
```

Файл `ast.go`
```go
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
```

Файл `parser.go`
```go
package parser

import (
	"Compilers/lab2.4/lexer"
	"fmt"
)

type Parser struct {
	tokens   []lexer.Token
	compiler *lexer.Compiler
	Current  lexer.Token // Текущий токен
	pos      int         // Позиция в списке токенов
}

func NewParser(tokens []lexer.Token, compiler *lexer.Compiler) *Parser {
	p := &Parser{
		tokens:   tokens,
		compiler: compiler,
		pos:      0,
	}
	if len(tokens) > 0 {
		p.Current = tokens[0]
	}
	return p
}

// NextToken переходит к следующему токену и обновляет Current
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

// getIdentifierName получает имя идентификатора из текущего токена
func (p *Parser) getIdentifierName() string {
	if p.Current.Tag == lexer.IdentifierTag {
		nameCode, ok := p.Current.Value.(int)
		if ok {
			return p.compiler.Names()[nameCode]
		}
	}
	return ""
}

// Parse является точкой входа для разбора всей программы
func (p *Parser) Parse() (*Specification, error) {
	spec := &Specification{}
	hasErrors := false

	// Разбираем каждую секцию согласно грамматике
	if err := p.parseClassDecl(spec); err != nil {
		hasErrors = true
	}
	if err := p.parseTokensDecl(spec); err != nil {
		hasErrors = true
	}
	if err := p.parseTypesDecl(spec); err != nil {
		hasErrors = true
	}
	if err := p.parseMethodsDecl(spec); err != nil {
		hasErrors = true
	}
	if err := p.parseGrammarDecl(spec); err != nil {
		hasErrors = true
	}
	if err := p.parseAxiomDecl(spec); err != nil {
		hasErrors = true
	}

	// Разбираем %end
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%end" {
		p.NextToken()
	} else {
		hasErrors = true
		p.error("Expected %end")
	}

	if hasErrors {
		return spec, fmt.Errorf("parsing completed with errors, check compiler messages")
	}
	return spec, nil
}

// parseClassDecl разбирает %class IDENTIFIER
func (p *Parser) parseClassDecl(spec *Specification) error {
	// %class
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%class" {
		p.NextToken()
	} else {
		return p.error("Expected %class")
	}

	// IDENTIFIER
	if p.Current.Tag == lexer.IdentifierTag {
		spec.ClassName = p.getIdentifierName()
		p.NextToken()
	} else {
		return p.error("Expected IDENTIFIER after %class")
	}

	return nil
}

// parseTokensDecl разбирает %tokens IDENTIFIER (IDENTIFIER)*
func (p *Parser) parseTokensDecl(spec *Specification) error {
	// %tokens
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%tokens" {
		p.NextToken()
	} else {
		return p.error("Expected %tokens")
	}

	// IDENTIFIER (IDENTIFIER)*
	for p.Current.Tag == lexer.IdentifierTag {
		spec.Tokens = append(spec.Tokens, p.getIdentifierName())
		p.NextToken()
	}

	return nil
}

// parseTypesDecl разбирает %types TypeDecl (";" TypeDecl)*
func (p *Parser) parseTypesDecl(spec *Specification) error {
	// %types
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%types" {
		p.NextToken()
	} else {
		return p.error("Expected %types")
	}

	// TypeDecl (";" TypeDecl)*
	for {
		if p.Current.Tag != lexer.IdentifierTag {
			break
		}

		typeMapping, err := p.parseTypeDecl()
		if err != nil {
			return err
		}
		spec.Types = append(spec.Types, typeMapping)

		if p.Current.Tag == lexer.SemicolonTag {
			p.NextToken()
		} else {
			break
		}
	}

	return nil
}

// parseTypeDecl разбирает IDENTIFIER ("," IDENTIFIER)* ":" Type
func (p *Parser) parseTypeDecl() (TypeMapping, error) {
	typeMapping := TypeMapping{}
	symbols := []string{}

	// IDENTIFIER ("," IDENTIFIER)*
	if p.Current.Tag == lexer.IdentifierTag {
		symbols = append(symbols, p.getIdentifierName())
		p.NextToken()
	} else {
		return typeMapping, p.error("Expected IDENTIFIER in TypeDecl")
	}

	for p.Current.Tag == lexer.CommaTag {
		p.NextToken()
		if p.Current.Tag == lexer.IdentifierTag {
			symbols = append(symbols, p.getIdentifierName())
			p.NextToken()
		} else {
			return typeMapping, p.error("Expected IDENTIFIER after comma in TypeDecl")
		}
	}

	// ":"
	if p.Current.Tag == lexer.ColonTag {
		p.NextToken()
	} else {
		return typeMapping, p.error("Expected ':' in TypeDecl")
	}

	// Type
	typeName, isArray, err := p.parseType()
	if err != nil {
		return typeMapping, err
	}

	typeMapping.Symbols = symbols
	typeMapping.TypeName = typeName
	typeMapping.IsArray = isArray

	return typeMapping, nil
}

// parseType разбирает IDENTIFIER ("[" "]")?
func (p *Parser) parseType() (string, bool, error) {
	var typeName string
	var isArray bool

	// IDENTIFIER
	if p.Current.Tag == lexer.IdentifierTag {
		typeName = p.getIdentifierName()
		p.NextToken()
	} else {
		return "", false, p.error("Expected IDENTIFIER in Type")
	}

	// ("[" "]")?
	if p.Current.Tag == lexer.LBracketTag {
		p.NextToken()
		isArray = true
		if p.Current.Tag == lexer.RBracketTag {
			p.NextToken()
		} else {
			return "", false, p.error("Expected ']' after '[' in Type")
		}
	}

	return typeName, isArray, nil
}

// parseMethodsDecl разбирает %methods MethodDecl (";" MethodDecl)*
func (p *Parser) parseMethodsDecl(spec *Specification) error {
	// %methods
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%methods" {
		p.NextToken()
	} else {
		return p.error("Expected %methods")
	}

	// MethodDecl (";" MethodDecl)*
	for {
		if p.Current.Tag != lexer.IdentifierTag {
			break
		}

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

// parseMethodDecl разбирает Type IDENTIFIER "(" ParamList ")"
func (p *Parser) parseMethodDecl() (Method, error) {
	method := Method{}

	// Type
	returnType, isArray, err := p.parseType()
	if err != nil {
		return method, err
	}
	method.ReturnType = returnType
	method.IsArray = isArray

	// IDENTIFIER
	if p.Current.Tag == lexer.IdentifierTag {
		method.Name = p.getIdentifierName()
		p.NextToken()
	} else {
		return method, p.error("Expected IDENTIFIER in MethodDecl")
	}

	// "("
	if p.Current.Tag == lexer.LParTag {
		p.NextToken()
	} else {
		return method, p.error("Expected '(' in MethodDecl")
	}

	// ParamList
	if p.Current.Tag == lexer.IdentifierTag {
		params, err := p.parseParamList()
		if err != nil {
			return method, err
		}
		method.Params = params
	}

	// ")"
	if p.Current.Tag == lexer.RParTag {
		p.NextToken()
	} else {
		return method, p.error("Expected ')' in MethodDecl")
	}

	return method, nil
}

// parseParamList разбирает (Type ("," Type)*)?
func (p *Parser) parseParamList() ([]Parameter, error) {
	var params []Parameter

	typeName, isArray, err := p.parseType()
	if err != nil {
		return params, err
	}
	params = append(params, Parameter{TypeName: typeName, IsArray: isArray})

	for p.Current.Tag == lexer.CommaTag {
		p.NextToken()
		typeName, isArray, err := p.parseType()
		if err != nil {
			return params, err
		}
		params = append(params, Parameter{TypeName: typeName, IsArray: isArray})
	}

	return params, nil
}

// parseGrammarDecl разбирает %grammar Rule (";" Rule)*
func (p *Parser) parseGrammarDecl(spec *Specification) error {
	// %grammar
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%grammar" {
		p.NextToken()
	} else {
		return p.error("Expected %grammar")
	}

	// Rule (";" Rule)*
	for {
		if p.Current.Tag != lexer.IdentifierTag {
			break
		}

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

// parseRule разбирает IDENTIFIER "=" Alternative ("|" Alternative)*
func (p *Parser) parseRule() (Rule, error) {
	rule := Rule{}

	// IDENTIFIER
	if p.Current.Tag == lexer.IdentifierTag {
		rule.Name = p.getIdentifierName()
		p.NextToken()
	} else {
		return rule, p.error("Expected IDENTIFIER in Rule")
	}

	// "="
	if p.Current.Tag == lexer.EqualsTag {
		p.NextToken()
	} else {
		return rule, p.error("Expected '=' in Rule")
	}

	expr, action, err := p.parseProduction()
	if err != nil {
		return rule, err
	}

	rule.Production = expr
	rule.Action = action

	return rule, nil
}

// parseProduction разбирает Alternative ("|" Alternative)*
func (p *Parser) parseProduction() (Expression, string, error) {
	// Получаем первую альтернативу
	expr, action, err := p.parseAlternative()
	if err != nil {
		return nil, "", err
	}

	// Если есть операторы "|", создаем объект Alternative
	if p.Current.Tag == lexer.BarTag {
		options := []Expression{expr}

		for p.Current.Tag == lexer.BarTag {
			p.NextToken()
			altExpr, altAction, err := p.parseAlternative()
			if err != nil {
				return nil, "", err
			}
			options = append(options, altExpr)

			if action == "" {
				action = altAction
			}
		}
		return Alternative{Options: options}, action, nil
	}
	return expr, action, nil
}

// parseAlternative разбирает AltElement (AltElement)* ("/" IDENTIFIER)?
func (p *Parser) parseAlternative() (Expression, string, error) {
	var items []Expression
	var action string

	// AltElement (AltElement)*
	for p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%rep" ||
		p.Current.Tag == lexer.IdentifierTag ||
		p.Current.Tag == lexer.LParTag {

		expr, err := p.parseAltElement()
		if err != nil {
			return nil, "", err
		}
		items = append(items, expr)
	}

	// ("/" IDENTIFIER)?
	if p.Current.Tag == lexer.SlashTag {
		p.NextToken()
		if p.Current.Tag == lexer.IdentifierTag {
			action = p.getIdentifierName()
			p.NextToken()
		} else {
			return nil, "", p.error("Expected IDENTIFIER after '/' in Alternative")
		}
	}

	if len(items) == 1 && action == "" {
		return items[0], "", nil
	}

	return Sequence{Items: items}, action, nil
}

// parseAltElement разбирает %rep? (IDENTIFIER | "(" Alternative ("|" Alternative)* ")")
func (p *Parser) parseAltElement() (Expression, error) {
	hasRep := false

	// %rep?
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%rep" {
		hasRep = true
		p.NextToken()
	}

	// IDENTIFIER | "(" Alternative ("|" Alternative)* ")"
	if p.Current.Tag == lexer.IdentifierTag {
		name := p.getIdentifierName()
		p.NextToken()

		if hasRep {
			return Repetition{Item: Terminal{Value: name}}, nil
		}
		return Terminal{Value: name}, nil

	} else if p.Current.Tag == lexer.LParTag {
		p.NextToken()

		// Alternative ("|" Alternative)*
		expr, actionName, err := p.parseProduction()
		if err != nil {
			return nil, err
		}

		// ")"
		if p.Current.Tag == lexer.RParTag {
			p.NextToken()
		} else {
			return nil, p.error("Expected ')' in AltElement")
		}

		if hasRep {
			return Repetition{Item: Grouped{Expression: expr}, Action: actionName}, nil
		}
		return Grouped{Expression: expr}, nil
	} else {
		return nil, p.error("Expected IDENTIFIER or '(' in AltElement")
	}
}

// parseAxiomDecl разбирает %axiom IDENTIFIER
func (p *Parser) parseAxiomDecl(spec *Specification) error {
	// %axiom
	if p.Current.Tag == lexer.KeywordTag && p.Current.Value == "%axiom" {
		p.NextToken()
	} else {
		return p.error("Expected %axiom")
	}

	// IDENTIFIER
	if p.Current.Tag == lexer.IdentifierTag {
		spec.Axiom = p.getIdentifierName()
		p.NextToken()
	} else {
		return p.error("Expected IDENTIFIER in AxiomDecl")
	}

	return nil
}
```

Файл `utils.go`
```go
package parser

import (
	"fmt"
	"strings"
)

func PrintSpecification(spec *Specification) {
	fmt.Println("ClassName:", spec.ClassName)
	fmt.Println("Tokens:", strings.Join(spec.Tokens, ", "))
	fmt.Println("Types:")
	for _, t := range spec.Types {
		fmt.Printf("  Symbols: %s, TypeName: %s, IsArray: %t\n", 
			strings.Join(t.Symbols, ", "), t.TypeName, t.IsArray)
	}
	fmt.Println("Methods:")
	for _, m := range spec.Methods {
		fmt.Printf("  %s %s(", m.ReturnType, m.Name)
		for i, p := range m.Params {
			if i > 0 {
				fmt.Print(", ")
			}
			fmt.Printf("%s", p.TypeName)
			if p.IsArray {
				fmt.Print("[]")
			}
		}
		fmt.Println(")")
	}
	fmt.Println("Grammar:")
	for _, r := range spec.Grammar {
		fmt.Printf("  %s = ", r.Name)
		PrintExpression(r.Production, 0)
		if r.Action != "" {
			fmt.Printf(" / %s", r.Action)
		}
		fmt.Println()
	}
	fmt.Println("Axiom:", spec.Axiom)
}

func PrintExpression(expr Expression, level int) {
	indent := strings.Repeat("  ", level)
	switch e := expr.(type) {
	case Sequence:
		fmt.Printf("%sSequence{\n", indent)
		for _, item := range e.Items {
			PrintExpression(item, level+1)
			fmt.Println()
		}
		fmt.Printf("%s}", indent)
	case Alternative:
		fmt.Printf("%sAlternative{\n", indent)
		for i, opt := range e.Options {
			if i > 0 {
				fmt.Printf("%s| ", indent)
			}
			PrintExpression(opt, level+1)
			fmt.Println()
		}
		fmt.Printf("%s}", indent)
	case Repetition:
		fmt.Printf("%sRepetition{\n", indent)
		PrintExpression(e.Item, level+1)
		fmt.Println()
		if e.Action != "" {
			fmt.Printf("%s/ %s\n", indent, e.Action)
		}
		fmt.Printf("%s}", indent)
	case Terminal:
		fmt.Printf("%sTerminal{%s}", indent, e.Value)
	case NonTerminal:
		fmt.Printf("%sNonTerminal{%s}", indent, e.Name)
	case Grouped:
		fmt.Printf("%sGrouped(\n", indent)
		PrintExpression(e.Expression, level+1)
		fmt.Println()
		fmt.Printf("%s)", indent)
	default:
		fmt.Printf("%sUnknown Expression", indent)
	}
}
```

Файл `main.go`
```go
package main

import (
	"Compilers/lab2.4/lexer"
	"Compilers/lab2.4/parser"
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
		fmt.Printf("%s %s", lexer.TagToString(token.Tag), token.Coords)
		if token.Tag == lexer.IdentifierTag {
			nameCode := token.Value.(int)
			name := comp.Names()[nameCode]
			fmt.Printf(": %s", name)
		} else if token.Value != nil {
			fmt.Printf(": %v", token.Value)
		}
		fmt.Println()
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
}
```


# Тестирование

Входные данные

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

Вывод на `stdout`

```
Parsed Specification:
ClassName: SimpleImperativeLang
Tokens: NUMBER, PLUS, MINUS, STAR, FRAC, LBRAC, RBRAC, TRUE, FALSE, ADD, OR, NOT, 
LT, GT, LE, GE, NE, EQ, IF, THEN, ELSE, END, WHILE, DO, SEMICOLON, VAR, ASSIGN, INPUT, PRINT, COMMA
Types:
  Symbols: Expr, Term, Factor, NUMBER, TypeName: ArithmExpr, IsArray: false
  Symbols: PLUS, MINUS, STAR, FRAC, TypeName: ArithmOp, IsArray: false
  Symbols: BoolExpr, BoolTerm, BoolFactor, TRUE, FALSE, TypeName: BoolExpr, IsArray: false
  Symbols: LT, GT, LE, GE, NE, EQ, TypeName: RelaOp, IsArray: false
  Symbols: Program, Statement, StatementList, Program, TypeName: Statement, IsArray: false
  Symbols: VAR, STRING, TypeName: String, IsArray: false
  Symbols: PrintItem, TypeName: PrintItem, IsArray: false
Methods:
  ArithmExpr neg_op(ArithmOp, ArithmExpr)
  ArithmExprChunk chunk(ArithmOp, ArithmExpr)
  ArithmExpr bin_op(ArithmExpr, ArithmExprChunk[])
  ArithmExpr deref(String)
  BoolExpr rela_op(ArithmExpr, RelaOp, ArithmExpr)
  BoolExpr disj_op(BoolExpr, BoolExpr[])
  BoolExpr conj_op(BoolExpr, BoolExpr[])
  BoolExpr not_op(BoolExpr)
  Statement assign_stmt(String, ArithmExpr)
  Statement append(Statement, Statement)
  Statement compound(Statement, Statement[])
  Statement if_else_stmt(BoolExpr, Statement, Statement)
  Statement empty_stmt()
  Statement while_stmt(BoolExpr, Statement)
  Statement input_stmt(String, String[])
  PrintItem print_value(ArithmExpr)
  PrintItem print_string(String)
  Statement print_stmt(PrintItem, PrintItem[])
Grammar:
  Program = Terminal{StatementList}
  StatementList = Sequence{
  Terminal{Statement}
  Repetition{
    Grouped(
      Sequence{
        Terminal{SEMICOLON}
        Terminal{Statement}
      }
    )
  }
} / compound
  Statement = Alternative{
  Sequence{
    Terminal{VAR}
    Terminal{ASSIGN}
    Terminal{Expr}
  }
|   Sequence{
    Terminal{IF}
    Terminal{BoolExpr}
    Terminal{THEN}
    Terminal{StatementList}
    Grouped(
      Alternative{
        Sequence{
        }
      |         Sequence{
          Terminal{ELSE}
          Terminal{StatementList}
        }
      }
    )
    Terminal{END}
  }
|   Sequence{
    Terminal{WHILE}
    Terminal{BoolExpr}
    Terminal{DO}
    Terminal{StatementList}
    Terminal{END}
  }
|   Sequence{
    Terminal{INPUT}
    Terminal{VAR}
    Repetition{
      Grouped(
        Sequence{
          Terminal{COMMA}
          Terminal{VAR}
        }
      )
    }
  }
|   Sequence{
    Terminal{PRINT}
    Terminal{PrintItem}
    Grouped(
      Sequence{
        Terminal{COMMA}
        Terminal{PrintItem}
      }
    )
  }
} / assign_stmt
  PrintItem = Alternative{
  Sequence{
    Terminal{Expr}
  }
|   Sequence{
    Terminal{STRING}
  }
} / print_value
  BoolExpr = Sequence{
  Terminal{BoolTerm}
  Repetition{
    Grouped(
      Sequence{
        Terminal{OR}
        Terminal{BoolTerm}
      }
    )
  }
} / disj_op
  BoolTerm = Sequence{
  Terminal{BoolFactor}
  Repetition{
    Grouped(
      Sequence{
        Terminal{AND}
        Terminal{BoolFactor}
      }
    )
  }
} / conj_op
  BoolFactor = Alternative{
  Terminal{TRUE}
|   Terminal{FALSE}
|   Sequence{
    Terminal{Expr}
    Terminal{RelaOp}
    Terminal{Expr}
  }
|   Sequence{
    Terminal{NOT}
    Terminal{BoolFactor}
  }
|   Sequence{
    Terminal{LBRAC}
    Terminal{BoolExpr}
    Terminal{RBRAC}
  }
} / rela_op
  Expr = Sequence{
  Grouped(
    Alternative{
      Terminal{Term}
    |       Sequence{
        Terminal{MINUS}
        Terminal{Term}
      }
    }
  )
  Repetition{
    Grouped(
      Sequence{
        Grouped(
          Alternative{
            Terminal{PLUS}
          |             Terminal{MINUS}
          }
        )
        Terminal{Term}
      }
    )
  / chunk
  }
} / bin_op
  Term = Sequence{
  Terminal{Factor}
  Repetition{
    Grouped(
      Sequence{
        Grouped(
          Alternative{
            Terminal{STAR}
          |             Terminal{FRAC}
          }
        )
        Terminal{Factor}
      }
    )
  / chunk
  }
} / bin_op
  Factor = Alternative{
  Terminal{NUMBER}
|   Sequence{
    Terminal{VAR}
  }
|   Sequence{
    Terminal{LBRAC}
    Terminal{Expr}
    Terminal{RBRAC}
  }
} / deref
Axiom: Program
```

# Вывод
В процессе выполнения я освоил разработку лексического и синтаксического анализаторов 
для языка SimpleImperativeLang, 
поддерживающего сложные конструкции, такие как арифметические и булевы выражения, условные операторы, циклы и 
повторяющиеся элементы. Я научился применять метод рекурсивного спуска для разбора грамматик, 
строить абстрактное синтаксическое дерево и обрабатывать токены с учетом их типов и позиций в тексте. 
Также я приобрел навыки работы с ошибками компиляции, управления таблицей имен и 
вывода структурированных данных. 
Тестирование подтвердило корректность реализации, что укрепило понимание принципов работы компиляторов.