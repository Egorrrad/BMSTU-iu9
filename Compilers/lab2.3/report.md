% Лабораторная работа № 2.3 «Синтаксический анализатор на основе
  предсказывающего анализа»
% 29 апреля 2025 г.
% 

# Цель работы
Целью данной работы является изучение алгоритма построения 
таблиц предсказывающего анализатора.

# Индивидуальный вариант
```
$ ключевые слова
$ начинаются с обратной кавычки

F  `is "n" `or "(" E ")" `end
T  `is F T1 `end
T1 `is "*" F T1 `or `epsilon `end
`axiom E  `is T E1 `end
E1 `is "+" T E1 `or `epsilon `end
```

# Реализация

## Неформальное описание синтаксиса входного языка
Входной язык представляет собой набор правил грамматики, каждое из которых состоит из левой части, стрелки 
(представлена как ключевое слово 'is),
правой части и завершается ключевым словом 'end.

**Общая структура:**  
Правила состоят из набора правил. Каждое правило имеет вид:  
`Левая_часть 'is Правая_часть 'end`, где:
- `'is` заменяет стрелку (→)
- `'end` завершает правило

**Правила грамматики:**
1. **Набор правил (S):**  
   Может быть пустым (ε) или содержать последовательность правил: сначала одно правило K, затем остальные S.

2. **Отдельное правило (K):**  
   Формат: `L 'is R end`, где:
    - **L (Левая часть):**
        - `'axiom n` (если это аксиома)
        - `n` (простой нетерминал)
    - **R (Правая часть):**  
      Состоит из трёх компонентов:
        - **F** — начальный элемент (терминал, нетерминал или `'epsilon`)
        - **A** — дополнительные элементы (повторения `'epsilon`, терминалов, нетерминалов)
        - **T** — альтернативные ветки через `'or`

3. **Альтернативы (T):**
    - Могут отсутствовать.
    - Если есть, начинаются с `'or`, за которым следует новая правая часть (F A T).


## Лексическая структура
```
axiom	= `axiom
epsilon =	`epsilon
end =	`end
or =	`or
is =	`is
n =	[A-Z][A-Z0-9]*
string =	^\"[^\"]\"
```

## Грамматика языка
```
S   → K S | ε
K   → L is R end
L   → axiom n | n
R   → F A T
A   → epsilon A | n A | string A | ε
T   → or F A T | ε
F   → epsilon | n | string
```

## Программная реализация

Файл `main.go`
```go
package main

import (
	"Compilers/lab2.3/graphviz"
	"Compilers/lab2.3/lexer"
	"Compilers/lab2.3/parser"
	"fmt"
	"log"
	"os"
	"os/exec"
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
		if token.Value != nil {
			fmt.Printf(": %v", token.Value)
		}
		fmt.Println()
		tokens = append(tokens, token)
		if token.Tag == lexer.EOFTag {
			break
		}
	}
	
	p := parser.NewParser(tokens, comp)
	tree, err := p.Parse()
	if err != nil {
		fmt.Println("Parse error:", err)
		os.Exit(1)
	}
	
	filename := "parse_tree.dot"
	err = graphviz.ExportToDot(tree, filename)
	if err != nil {
		fmt.Println("Error exporting to DOT:", err)
		os.Exit(1)
	}

	fmt.Println("Parse tree exported to parse_tree.dot")

	outputFile := "graph.png"
	cmd := exec.Command("dot", "-Tpng", filename, "-o", outputFile)

	if err := cmd.Run(); err != nil {
		log.Fatalf("Error running Graphviz: %v\n" +
			"Make sure Graphviz is installed and available in PATH", err)
	}
	fmt.Printf("Graph saved in %s\n", outputFile)
}
```

Файл `compiler.go`
```go
package lexer

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
	NeterminalTag
	StringTag
	EOFTag
	ErrorTag
)

func TagToString(tag DomainTag) string {
	return [...]string{
		"Keyword",
		"Neterminal",
		"String",
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

import (
	"fmt"
	"strings"
)

type Node struct {
	Name     string
	Children []*Node
}

func PrintNode(node *Node, depth int) {
	indent := strings.Repeat("  ", depth)
	fmt.Printf("%sNode{\n", indent)
	fmt.Printf("%s  Name: \"%s\",\n", indent, node.Name)
	fmt.Printf("%s  Children: [\n", indent)

	for _, child := range node.Children {
		PrintNode(child, depth+2)
	}

	fmt.Printf("%s  ],\n", indent)
	fmt.Printf("%s}\n", indent)
}
```

Файл `grammar.go`
```go
package parser

import (
	"Compilers/lab2.3/lexer"
	"fmt"
)

type Production []string

var ParseTable = map[string]map[string]Production{
	"S": {
		"`axiom":     {"K", "S"},
		"Neterminal": {"K", "S"},
		"EOF":        {}, // ε
	},
	"K": {
		"`axiom":     {"L", "`is", "R", "`end"},
		"Neterminal": {"L", "`is", "R", "`end"},
	},
	"L": {
		"`axiom":     {"`axiom", "Neterminal"},
		"Neterminal": {"Neterminal"},
	},
	"R": {
		"`epsilon":   {"F", "A", "T"},
		"Neterminal": {"F", "A", "T"},
		"String":     {"F", "A", "T"},
	},
	"A": {
		"`epsilon":   {"`epsilon", "A"},
		"Neterminal": {"Neterminal", "A"},
		"String":     {"String", "A"},
		"`or":        {}, // ε
		"`end":       {}, // ε
	},
	"T": {
		"`or":  {"`or", "F", "A", "T"},
		"`end": {}, // ε
	},
	"F": {
		"`epsilon":   {"`epsilon"},
		"Neterminal": {"Neterminal"},
		"String":     {"String"},
	},
}

func isTerminal(symbol string) bool {
	return symbol != "S" && ParseTable[symbol] == nil
}

func terminalOf(token lexer.Token) string {
	switch token.Tag {
	case lexer.KeywordTag:
		return fmt.Sprintf("%v", token.Value)
	case lexer.NeterminalTag:
		return "Neterminal"
	case lexer.StringTag:
		return "String"
	case lexer.EOFTag:
		return "EOF"
	default:
		return ""
	}
}

func matchTerminal(expected string, token lexer.Token) bool {
	switch expected {
	case "Neterminal":
		return token.Tag == lexer.NeterminalTag
	case "String":
		return token.Tag == lexer.StringTag
	case "EOF":
		return token.Tag == lexer.EOFTag
	default:
		return token.Tag == lexer.KeywordTag && fmt.Sprintf("%v", token.Value) == expected
	}
}
```

Файл `parser.go`
```go
package parser

import (
	"Compilers/lab2.3/lexer"
	"fmt"
)

type Parser struct {
	tokens   []lexer.Token
	pos      int
	current  lexer.Token
	compiler *lexer.Compiler
}

func NewParser(tokens []lexer.Token, compiler *lexer.Compiler) *Parser {
	return &Parser{
		tokens:   tokens,
		pos:      0,
		current:  tokens[0],
		compiler: compiler,
	}
}

const Epsilon = "ε"

func (p *Parser) Parse() (*Node, error) {
	// Создаем копию токенов и добавляем маркер конца
	tokens := make([]lexer.Token, len(p.tokens))
	copy(tokens, p.tokens)
	tokens = append(tokens, lexer.Token{Tag: lexer.EOFTag, Value: "EOF"})

	root := &Node{Name: "S"}
	stack := []*Node{{Name: "EOF"}, root}

	current := tokens[0]
	tokens = tokens[1:]

	for {
		// Проверка на пустой стек
		if len(stack) == 0 {
			return nil, fmt.Errorf("parser error: stack empty but " +
				"input not fully consumed (current: %v)", current.Value)
		}

		currentNode := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		// достигнут конец входных данных
		if currentNode.Name == "EOF" && current.Tag == lexer.EOFTag {
			break
		}

		// Обработка терминалов
		if isTerminal(currentNode.Name) {
			if matchTerminal(currentNode.Name, current) {
				if current.Tag == lexer.NeterminalTag {
					currentNode.Name = fmt.Sprintf("'%s'", 
						p.compiler.Names()[current.Value.(int)])
				} else {
					currentNode.Name = fmt.Sprintf("'%s'", current.Value)
				}
				// переход к следующему токену
				if len(tokens) > 0 {
					current = tokens[0]
					tokens = tokens[1:]
				}
			} else {
				return nil, fmt.Errorf("syntax error: expected " +
					"%s, found %v", currentNode.Name, current.Value)
			}
		} else {
			// Обработка нетерминалов
			production, ok := ParseTable[currentNode.Name][terminalOf(current)]
			if !ok {
				return nil, fmt.Errorf("no rule for non-terminal %s " +
					"with terminal %v", currentNode.Name, current.Value)
			}

			// Обработка пустого правила (ε)
			if len(production) == 0 {
				currentNode.Children = append(currentNode.Children, &Node{Name: Epsilon})
			} else {
				// Создаем узлы
				newNodes := make([]*Node, len(production))
				for i, sym := range production {
					newNodes[i] = &Node{Name: sym}
				}

				// Добавляем новые узлы как детей текущего узла
				currentNode.Children = append(currentNode.Children, newNodes...)

				// Помещаем символы продукции в стек в обратном порядке
				for i := len(newNodes) - 1; i >= 0; i-- {
					stack = append(stack, newNodes[i])
				}
			}
		}
	}
	return root, nil
}
```

Файл `export.go`
```go
package graphviz

import (
	"Compilers/lab2.3/parser"
	"fmt"
	"os"
)

func ExportToDot(root *parser.Node, filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	fmt.Fprintln(file, "digraph ParseTree {")
	id := 0
	exportNode(file, root, &id)
	fmt.Fprintln(file, "}")

	return nil
}

func exportNode(file *os.File, node *parser.Node, id *int) int {
	currentID := *id
	fmt.Fprintf(file, "  node%d [label=\"%s\"];\n", currentID, node.Name)
	*id++

	for _, child := range node.Children {
		childID := exportNode(file, child, id)
		fmt.Fprintf(file, "  node%d -> node%d;\n", currentID, childID)
	}

	return currentID
}
```

# Тестирование

Входные данные

Файл `input.txt`
```
F  `is "n" `or "(" E ")" `end
T  `is F T1 `end
T1 `is "*" F T1 `or `epsilon `end
`axiom E  `is T E1 `end
E1 `is "+" T E1 `or `epsilon `end
```

Вывод на `stdout`

```
Neterminal (1,2)-(1,3): 0
Keyword (1,3)-(1,8): `is
String (1,8)-(1,12): n
Keyword (1,12)-(1,16): `or
String (1,16)-(1,20): (
Neterminal (1,20)-(1,22): 1
String (1,22)-(1,26): )
Keyword (1,26)-(1,31): `end
Neterminal (1,31)-(2,2): 2
Keyword (2,2)-(2,7): `is
Neterminal (2,7)-(2,9): 0
Neterminal (2,9)-(2,12): 3
Keyword (2,12)-(2,17): `end
Neterminal (2,17)-(3,3): 3
Keyword (3,3)-(3,7): `is
String (3,7)-(3,11): *
Neterminal (3,11)-(3,13): 0
Neterminal (3,13)-(3,16): 3
Keyword (3,16)-(3,20): `or
Keyword (3,20)-(3,29): `epsilon
Keyword (3,29)-(3,34): `end
Keyword (3,34)-(4,7): `axiom
Neterminal (4,7)-(4,9): 1
Keyword (4,9)-(4,14): `is
Neterminal (4,14)-(4,16): 2
Neterminal (4,16)-(4,19): 4
Keyword (4,19)-(4,24): `end
Neterminal (4,24)-(5,3): 4
Keyword (5,3)-(5,7): `is
String (5,7)-(5,11): +
Neterminal (5,11)-(5,13): 2
Neterminal (5,13)-(5,16): 4
Keyword (5,16)-(5,20): `or
Keyword (5,20)-(5,29): `epsilon
Keyword (5,29)-(5,33): `end
EOF (5,33)-(5,33)
Parse tree exported to parse_tree.dot
Graph saved in graph.png
```

![Дерево вывода](pics/graph.png)

# Вывод
В ходе выполнения лабораторной работы была разработан синтаксический 
анализатор на основе предсказывающего анализа. 
Также был реализован лексический анализатор, построено синтаксическое 
дерево, обеспечен его экспорт в графический формат с использованием Graphviz. 
В процессе работы была успешно применена 
таблица предсказывающего анализа для разбора входного языка. В результате выполнения лабораторной работы я 
изучил принципы построения таблиц предсказывающего разбора, 
научился разрабатывать лексический анализатор, обрабатывающий ключевые слова, 
идентификаторы и строки, реализовал парсер и убедился в его корректной работе, 
научился обрабатывать ошибки разбора и визуализировать синтаксическое дерево, 
получил практический опыт работы с 
языками описания грамматик и синтаксическим анализом, а также укрепил навыки программирования на языке Go.
