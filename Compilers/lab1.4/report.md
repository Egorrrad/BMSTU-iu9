% Лабораторная работа № 1.4 «Лексический распознаватель»
% 18 марта 2025 г.
% 

# Цель работы
Целью данной работы является изучение использования детерминированных конечных автоматов с размеченными 
заключительными состояниями (лексических распознавателей) для решения задачи лексического анализа.

# Индивидуальный вариант
switch, case, :, {, }, комментарии начинаются со знака ! и продолжаются до конца строки.

# Реализация

Лексическая структура языка — регулярные выражения для доменов:

* Whitespace = `"[ \t\n\r]+"`
* Identifier = `"[a-zA-Z][a-zA-Z0-9]*"`
* Number     = `"[0-9]+"`
* Keyword    = `"switch|case"`
* Operator   = `"[:{}]"`
* Comment    = `"!.*\n"`


Граф детерминированного распознавателя:

![Граф детерминированного распознавателя](pics/automata.png)

Реализация распознавателя:

Файл `Position.go`:
```go
package main

type Position struct {
    Line, Column int
}
```

Файл `Token.go`:
```go
package main

type Token struct {
	Tag      string
	StartPos Position
	EndPos   Position
	Value    string
}
```

Файл `Scanner.go`:
```go
func NewLexer(reader io.Reader) *Lexer {
	l := &Lexer{
		reader: bufio.NewReader(reader),
		pos:    Position{Line: 1, Column: 0},
		symbolClasses: map[rune]int{
			' ': 0, '\t': 0, '\r': 0,
			':': 1, '{': 2, '}': 3, '!': 4,
			'\n': 8,
			's':  9, // для 'switch'
			'w':  10,
			'i':  11,
			't':  12,
			'c':  13, // для 'case'
			'h':  14,
			'a':  15,
			'e':  16,
		},
		transitions: [][]int{
			// ws  :   {   }   !  letter digit other \n  s   w   i   t   c   h   a   e
			{0, 2, 2, 2, 4, 1, 3, -1, 0, 5, 1, 1, 1, 6, 1, 1, 1, 15, 1, 1},                   
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},              
			{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
			{-1, -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},  
			{4, 4, 4, 4, 4, 4, 4, 4, -1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4},                    
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1},              
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 8, 1, 1, 1, 1},              
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 9, 1, 1, 1, 1, 1, 1, 1, 1},              
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},             
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 11, 1, 1, 1, 1, 1, 1, 1},             
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 12, 1, 1, 1},             
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 13, 1, 1, 1, 1, 1, 1},             
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},              
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 14, 1, 1, 1, 1, 1},             
			{-1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},              
		},
		finalStates: map[int]string{
			1:  "IDENTIFIER",
			2:  "OPERATOR",
			3:  "NUMBER",
			4:  "COMMENT",
			12: "KEYWORD",    // 'case'
			14: "KEYWORD",    // 'switch'
			5:  "IDENTIFIER", // s1
			6:  "IDENTIFIER", // c1
			7:  "IDENTIFIER", // s2
			8:  "IDENTIFIER", // c2
			9:  "IDENTIFIER", // s3
			10: "IDENTIFIER", // c3
			11: "IDENTIFIER", // s4
			13: "IDENTIFIER", // s5
			15: "IDENTIFIER", // e1
			16: "IDENTIFIER", // e2
		},
		keywordStates: map[int]string{},
	}

	for ch := 'a'; ch <= 'z'; ch++ {
		if _, exists := l.symbolClasses[ch]; !exists {
			l.symbolClasses[ch] = 5 
		}
	}
	for ch := 'A'; ch <= 'Z'; ch++ {
		if _, exists := l.symbolClasses[ch]; !exists {
			l.symbolClasses[ch] = 5 
		}
	}
	for ch := '0'; ch <= '9'; ch++ {
		l.symbolClasses[ch] = 6
	}

	l.readRune()
	return l
}

func (l *Lexer) readRune() {
	r, _, err := l.reader.ReadRune()
	if err != nil {
		l.current = 0
		return
	}
	l.current = r
	if r == '\n' {
		l.pos.Line++
		l.pos.Column = 0
	} else {
		l.pos.Column++
	}
}

func (l *Lexer) NextToken() (*Token, error) {
	if l.current == 0 {
		return nil, io.EOF
	}

	state := 0
	startPos := l.pos
	var tokenValue strings.Builder

	lastFinalState := -1
	lastFinalPos := l.pos
	lastFinalValueLen := 0

	for {
		class, exists := l.symbolClasses[l.current]
		if !exists {
			class = 7
		}

		nextState := -1
		if state >= 0 && state < len(l.transitions) && 
		class >= 0 && class < len(l.transitions[state]) {
			nextState = l.transitions[state][class]
		}

		if nextState == -1 {
			if lastFinalState != -1 {
				tag := l.finalStates[lastFinalState]
				tokenStr := tokenValue.String()[:lastFinalValueLen]

				if state == 0 && lastFinalState == 0 {
					state = 0
					startPos = l.pos
					tokenValue.Reset()
					lastFinalState = -1
					continue
				}

				return &Token{
					Tag:      tag,
					StartPos: startPos,
					EndPos:   lastFinalPos,
					Value:    tokenStr,
				}, nil
			}

			if tokenValue.Len() == 0 {
				errChar := l.current
				errPos := l.pos
				l.readRune()
				return nil, fmt.Errorf("unknown character at (%d,%d): %q",
					errPos.Line, errPos.Column, string(errChar))
			} else {
				return nil, fmt.Errorf("unrecognized token at (%d,%d): %s",
					startPos.Line, startPos.Column, tokenValue.String())
			}
		}

		tokenValue.WriteRune(l.current)
		state = nextState

		if _, isFinal := l.finalStates[state]; isFinal {
			lastFinalState = state
			lastFinalPos = l.pos
			lastFinalValueLen = tokenValue.Len()
		}

		l.readRune()

		if l.current == 0 {
			if tag, isFinal := l.finalStates[state]; isFinal {
				return &Token{
					Tag:      tag,
					StartPos: startPos,
					EndPos:   l.pos,
					Value:    tokenValue.String(),
				}, nil
			}

			if lastFinalState != -1 {
				tag := l.finalStates[lastFinalState]
				return &Token{
					Tag:      tag,
					StartPos: startPos,
					EndPos:   lastFinalPos,
					Value:    tokenValue.String()[:lastFinalValueLen],
				}, nil
			}
		}
	}
}
```

Файл `main.go`:
```go
package main

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: lexer <inputfile>")
		return
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer file.Close()

	lexer := NewLexer(file)

	for {
		token, err := lexer.NextToken()
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Println("Error:", err)
			continue
		}

		displayValue := strings.ReplaceAll(token.Value, "\n", "")
		if token.Tag == "UNKNOWN" {
			fmt.Printf("Unknown character at (%d,%d): %q\n", 
			token.StartPos.Line, token.StartPos.Column, displayValue)
		} else {
			fmt.Printf("%s (%d,%d)-(%d,%d): %s\n",
				token.Tag,
				token.StartPos.Line, token.StartPos.Column,
				token.EndPos.Line, token.EndPos.Column,
				displayValue)
		}
		if token.Tag == "KEYWORD" && displayValue == "EOF" {
			break
		}
	}
}
```


# Тестирование

Входные данные

```
switch case x := 42
12344
! This is a comment
{
}
unknown123
1221
switc
cas
```

Вывод на `stdout`

```
KEYWORD (1,1)-(1,6): switch
KEYWORD (1,7)-(1,11):  case
IDENTIFIER (1,12)-(1,13):  x
OPERATOR (1,14)-(1,15):  :
Error: unknown character at (1,16): "="
NUMBER (1,17)-(1,19):  42
NUMBER (2,0)-(2,5): 12344
COMMENT (3,0)-(3,19): ! This is a comment
OPERATOR (4,0)-(4,1): {
OPERATOR (5,0)-(5,1): }
IDENTIFIER (6,0)-(6,10): unknown123
NUMBER (7,0)-(7,4): 1221
IDENTIFIER (8,0)-(8,5): switc
IDENTIFIER (9,0)-(9,3): cas
```

# Вывод
В ходе работы я освоил проектирование лексических анализаторов на основе конечных автоматов. 
Регулярные выражения позволили формализовать правила для доменов языка, а их преобразование в 
детерминированный автомат с факторизацией алфавита — оптимизировать распознавание. 
Реализация на Go с использованием таблицы переходов и отслеживанием позиций в коде научила меня 
эффективно управлять состояниями и обрабатывать ошибки. 
Работа углубила понимание этапов лексического анализа: от теории (НКА/ДКА) до практики, 
что подтвердилось тестированием на пограничных случаях.