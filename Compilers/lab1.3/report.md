% Лабораторная работа № 1.3 «Объектно-ориентированный
лексический анализатор»
% 4 марта 2025 г.
% 

# Цель работы
Целью данной работы является приобретение навыка реализации 
лексического анализатора на объектно-ориентированном 
языке без применения каких-либо средств автоматизации решения задачи лексического анализа.
# Индивидуальный вариант
* Идентификаторы: последовательности заглавных латинских букв, 
за которыми могут располагаться последовательности 
знаков «+», «-» и «\*».
* Целочисленные литералы — числа в унарной системе счисления: 
знак «\*» (представляющий 0) или последовательности, 
состоящие целиком либо из знаков «+», либо из знаков «-».
* Ключевые слова: «ON», «OFF», «**».
# Реализация

Файл `main.go`
```go
package main

import (
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

	compiler := NewCompiler()
	lexer := NewScanner(file, compiler)

	for {
		token := lexer.NextToken()
		fmt.Printf("%s %s", tagToString(token.Tag), token.Coords)
		if token.Value != nil {
			fmt.Printf(": %v", token.Value)
		}
		fmt.Println()

		if token.Tag == EndOfProgram {
			break
		}
	}

	fmt.Println("\nСообщения об ошибках:")
	for msg := compiler.messages.Front(); msg != nil; msg = msg.Next() {
		fmt.Println(msg.Value)
	}

	fmt.Println("\nТаблица идентификаторов:")
	for code, name := range compiler.names {
		fmt.Printf("%d: %s\n", code, name)
	}
}
```

Файл `position.go`
```go
package main

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

Файл `token.go`
```go
package main

type Token struct {
	Tag    DomainTag
	Coords Fragment
	Value  interface{}
}
```

Файл `scanner.go`
```go
package main

import (
	"io"
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
	for {
		start := *s.position

		switch {
		case s.position.Cp() == -1:
			return Token{EndOfProgram, Fragment{start, *s.position}, nil}

		case unicode.IsSpace(s.position.Cp()):
			s.position.Next()
			continue

		case s.position.Cp() == '*':
			return s.processStar(start)

		case s.position.Cp() == '+', s.position.Cp() == '-':
			return s.processNumber(start)

		case unicode.IsUpper(s.position.Cp()):
			return s.processIdentOrKeyword(start)

		default:
			s.compiler.AddMessage(true, s.position, "Unexpected character")
			s.position.Next()
		}
	}
}

func (s *Scanner) processStar(start Position) Token {
	s.position.Next()
	if s.position.Cp() == '*' {
		s.position.Next()
		return Token{KeywordDoublestar, Fragment{start, *s.position}, nil}
	}
	return Token{NUMBER, Fragment{start, *s.position}, 0}
}

func (s *Scanner) processNumber(start Position) Token {
	sign := s.position.Cp()
	s.position.Next()

	count := 1
	for s.position.Cp() == sign {
		count++
		s.position.Next()
	}

	value := count
	if sign == '-' {
		value = -value
	}

	return Token{NUMBER, Fragment{start, *s.position}, value}
}

func (s *Scanner) processIdentOrKeyword(start Position) Token {
	var buf []rune
	for unicode.IsUpper(s.position.Cp()) {
		buf = append(buf, s.position.Cp())
		s.position.Next()
	}

	for s.position.Cp() == '+' || s.position.Cp() == '-' || s.position.Cp() == '*' {
		buf = append(buf, s.position.Cp())
		s.position.Next()
	}

	str := string(buf)
	switch str {
	case "ON":
		return Token{KeywordOn, Fragment{start, *s.position}, nil}
	case "OFF":
		return Token{KeywordOff, Fragment{start, *s.position}, nil}
	default:
		code := s.compiler.AddName(str)
		return Token{IDENT, Fragment{start, *s.position}, code}
	}
}
```

Файл `fragment.go`
```go
package main

import "fmt"

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

Файл `domain.go`
```go
package main


type DomainTag int

const (
	IDENT DomainTag = iota
	NUMBER
	KeywordOn
	KeywordOff
	KeywordDoublestar
	EndOfProgram
)


func tagToString(tag DomainTag) string {
	return [...]string{
		"IDENT", "NUMBER",
		"KEYWORD_ON", "KEYWORD_OFF", "KEYWORD_DOUBLESTAR",
		"END_OF_PROGRAM",
	}[tag]
}
```

Файл `compiler.go`
```go
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

```


# Тестирование

Входные данные

```
++ * * A+* B ON
OFF A+* +++ --- **
??
```

Вывод на `stdout`

```
KEYWORD_ON (1,2)-(1,4)
NUMBER (1,5)-(1,7): 2
NUMBER (1,8)-(1,9): 0
NUMBER (1,10)-(1,11): 0
IDENT (1,12)-(1,15): 0
IDENT (1,16)-(1,17): 1
KEYWORD_OFF (2,1)-(2,4)
IDENT (2,5)-(2,8): 0
NUMBER (2,9)-(2,12): 3
NUMBER (2,13)-(2,16): -3
KEYWORD_DOUBLESTAR (2,17)-(2,19)
END_OF_PROGRAM (3,2)-(3,2)

Сообщения об ошибках:
Error (3,1): Unexpected character
Error (3,2): Unexpected character

Таблица идентификаторов:
0: A+*
1: B
```

# Вывод
В ходе выполнения лабораторной работы был разработан объектно-ориентированный 
лексический анализатор на языке Go, 
который успешно справляется с обработкой идентификаторов, 
целочисленных литералов в унарной системе счисления и 
ключевых слов. Реализация включает в себя основные компоненты:
сканер, компилятор и структуры для работы с токенами и позициями.
Тестирование показало, что анализатор корректно распознает лексемы, 
обрабатывает ошибки и формирует таблицу 
идентификаторов.

