package lexer

import "fmt"

// TokenType определяет тип токена для лексического анализа.
type TokenType int

const (
	TokenEOF        TokenType = iota // Конец файла
	TokenNumber                      // Числовой литерал
	TokenIdentifier                  // Идентификатор
	TokenAssign                      // Оператор присваивания
	TokenPlus                        // Оператор сложения
	TokenMinus                       // Оператор вычитания
	TokenMul                         // Оператор умножения
	TokenDiv                         // Оператор деления
	TokenLT                          // Оператор меньше
	TokenGT                          // Оператор больше
	TokenEQ                          // Оператор равенства
	TokenIf                          // Ключевое слово if
	TokenElse                        // Ключевое слово else
	TokenWhile                       // Ключевое слово while
	TokenLBrace                      // Открывающая фигурная скобка ({)
	TokenRBrace                      // Закрывающая фигурная скобка (})
	TokenLParen                      // Открывающая круглая скобка (()
	TokenRParen                      // Закрывающая круглая скобка ())
	TokenSemicolon                   // Точка с запятой (;)
)

// Token представляет токен с типом и значением.
type Token struct {
	Type  TokenType
	Value string
}

// Lex выполняет лексический анализ входной строки, возвращая список токенов.
func Lex(input string) []Token {
	var tokens []Token
	i := 0

	for i < len(input) {
		ch := input[i]

		switch {
		case ch == ' ' || ch == '\t' || ch == '\n':
			i++
			continue

		case ch == '=':
			tokens = append(tokens, Token{Type: TokenAssign})
			i++

		case ch == '+':
			tokens = append(tokens, Token{Type: TokenPlus})
			i++

		case ch == '-':
			tokens = append(tokens, Token{Type: TokenMinus})
			i++

		case ch == '*':
			tokens = append(tokens, Token{Type: TokenMul})
			i++

		case ch == '/':
			tokens = append(tokens, Token{Type: TokenDiv})
			i++

		case ch == '<':
			tokens = append(tokens, Token{Type: TokenLT})
			i++

		case ch == '>':
			tokens = append(tokens, Token{Type: TokenGT})
			i++

		case ch == '{':
			tokens = append(tokens, Token{Type: TokenLBrace})
			i++

		case ch == '}':
			tokens = append(tokens, Token{Type: TokenRBrace})
			i++

		case ch == '(':
			tokens = append(tokens, Token{Type: TokenLParen})
			i++

		case ch == ')':
			tokens = append(tokens, Token{Type: TokenRParen})
			i++

		case ch == ';':
			tokens = append(tokens, Token{Type: TokenSemicolon})
			i++

		case isDigit(ch):
			start := i
			for i < len(input) && isDigit(input[i]) {
				i++
			}
			tokens = append(tokens, Token{
				Type:  TokenNumber,
				Value: input[start:i],
			})

		case isLetter(ch):
			start := i
			for i < len(input) && (isLetter(input[i]) || isDigit(input[i])) {
				i++
			}
			word := input[start:i]

			// Распознаём ключевые слова (if, else, while) или идентификаторы.
			if word == "if" {
				tokens = append(tokens, Token{Type: TokenIf})
			} else if word == "else" {
				tokens = append(tokens, Token{Type: TokenElse})
			} else if word == "while" {
				tokens = append(tokens, Token{Type: TokenWhile})
			} else {
				tokens = append(tokens, Token{
					Type:  TokenIdentifier,
					Value: word,
				})
			}

		default:
			panic(fmt.Sprintf("unknown character: %c", ch))
		}
	}

	tokens = append(tokens, Token{Type: TokenEOF})
	return tokens
}

// isDigit проверяет, является ли символ цифрой.
func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

// isLetter проверяет, является ли символ буквой.
func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}
