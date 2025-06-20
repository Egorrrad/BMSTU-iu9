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
			return nil, fmt.Errorf("parser error: stack empty but input not fully consumed (current: %v)", current.Value)
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
					currentNode.Name = fmt.Sprintf("'%s'", p.compiler.Names()[current.Value.(int)])
				} else {
					currentNode.Name = fmt.Sprintf("'%s'", current.Value)
				}
				// переход к следующему токену
				if len(tokens) > 0 {
					current = tokens[0]
					tokens = tokens[1:]
				}
			} else {
				return nil, fmt.Errorf("syntax error: expected %s, found %v", currentNode.Name, current.Value)
			}
		} else {
			// Обработка нетерминалов
			production, ok := ParseTable[currentNode.Name][terminalOf(current)]
			if !ok {
				return nil, fmt.Errorf("no rule for non-terminal %s with terminal %v", currentNode.Name, current.Value)
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
