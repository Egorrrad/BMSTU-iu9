package parser

import (
	"OptimalCodeGen/lab4/ast"
	"OptimalCodeGen/lab4/lexer"
	"fmt"
	"strconv"
)

// Parser выполняет синтаксический анализ токенов.
type Parser struct {
	tokens  []lexer.Token
	current int
}

func NewParser(tokens []lexer.Token) *Parser {
	return &Parser{tokens: tokens}
}

// Parse разбирает токены в AST-блок.
func (p *Parser) Parse() (*ast.Block, error) {
	block := &ast.Block{}
	for !p.isAtEnd() {
		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		block.Statements = append(block.Statements, stmt)
	}
	return block, nil
}

// parseStatement разбирает одно выражение
func (p *Parser) parseStatement() (ast.Stmt, error) {
	switch p.peek().Type {
	case lexer.TokenIf:
		return p.parseIf()
	case lexer.TokenWhile:
		return p.parseWhile()
	case lexer.TokenIdentifier:
		if p.peekNext().Type == lexer.TokenAssign {
			return p.parseAssignment()
		}
		fallthrough
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseIf() (ast.Stmt, error) {
	if err := p.consume(lexer.TokenIf, "expect 'if'"); err != nil {
		return nil, err
	}

	cond, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	thenBlock, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	var elseBlock *ast.Block
	if p.match(lexer.TokenElse) {
		elseBlock, err = p.parseBlock()
		if err != nil {
			return nil, err
		}
	}

	return ast.IfStmt{Cond: cond, Then: thenBlock, Else: elseBlock}, nil
}

func (p *Parser) parseWhile() (ast.Stmt, error) {
	if err := p.consume(lexer.TokenWhile, "expect 'while'"); err != nil {
		return nil, err
	}

	cond, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return ast.WhileStmt{Cond: cond, Body: body}, nil
}

// parseAssignment разбирает присваивание
func (p *Parser) parseAssignment() (ast.Stmt, error) {
	name := p.peek().Value
	if err := p.consume(lexer.TokenIdentifier, "expect identifier"); err != nil {
		return nil, err
	}
	if err := p.consume(lexer.TokenAssign, "expect '=' after identifier"); err != nil {
		return nil, err
	}
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if err := p.consume(lexer.TokenSemicolon, "expect ';' after assignment"); err != nil {
		return nil, err
	}
	return ast.Assignment{Name: name, Expr: expr}, nil
}

func (p *Parser) parseExpressionStatement() (ast.Stmt, error) {
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if err := p.consume(lexer.TokenSemicolon, "expect ';' after expression"); err != nil {
		return nil, err
	}
	return ast.Assignment{Name: "_", Expr: expr}, nil
}

func (p *Parser) parseBlock() (*ast.Block, error) {
	if err := p.consume(lexer.TokenLBrace, "expect '{' before block"); err != nil {
		return nil, err
	}

	block := &ast.Block{}
	for !p.check(lexer.TokenRBrace) && !p.isAtEnd() {
		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		block.Statements = append(block.Statements, stmt)
	}

	if err := p.consume(lexer.TokenRBrace, "expect '}' after block"); err != nil {
		return nil, err
	}
	return block, nil
}

// parseExpression разбирает выражение
func (p *Parser) parseExpression() (ast.Expr, error) {
	return p.parseBinary()
}

func (p *Parser) parseBinary() (ast.Expr, error) {
	expr, err := p.parsePrimary()
	if err != nil {
		return nil, err
	}

	for p.isBinaryOp() {
		op := p.peek().Type
		p.advance()
		right, err := p.parsePrimary()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryOp{Left: expr, Op: p.opToString(op), Right: right}
	}

	return expr, nil
}

func (p *Parser) parsePrimary() (ast.Expr, error) {
	switch p.peek().Type {
	case lexer.TokenNumber:
		val, err := strconv.Atoi(p.advance().Value)
		if err != nil {
			return nil, fmt.Errorf("invalid number: %v", err)
		}
		return ast.Number{Value: val}, nil
	case lexer.TokenIdentifier:
		return ast.Variable{Name: p.advance().Value}, nil
	case lexer.TokenLParen:
		p.advance()
		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if err := p.consume(lexer.TokenRParen, "expect ')' after expression"); err != nil {
			return nil, err
		}
		return expr, nil
	default:
		return nil, fmt.Errorf("unexpected token: %v", p.peek())
	}
}

// isBinaryOp проверяет, является ли токен бинарной операцией.
func (p *Parser) isBinaryOp() bool {
	switch p.peek().Type {
	case lexer.TokenPlus, lexer.TokenMinus, lexer.TokenMul, lexer.TokenDiv, lexer.TokenLT, lexer.TokenGT, lexer.TokenEQ:
		return true
	default:
		return false
	}
}

// opToString преобразует токен в строковый оператор.
func (p *Parser) opToString(t lexer.TokenType) string {
	switch t {
	case lexer.TokenPlus:
		return "+"
	case lexer.TokenMinus:
		return "-"
	case lexer.TokenMul:
		return "*"
	case lexer.TokenDiv:
		return "/"
	case lexer.TokenLT:
		return "<"
	case lexer.TokenGT:
		return ">"
	case lexer.TokenEQ:
		return "=="
	default:
		return "?"
	}
}

func (p *Parser) advance() lexer.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) consume(t lexer.TokenType, msg string) error {
	if p.check(t) {
		p.advance()
		return nil
	}
	return fmt.Errorf("%s at token %v", msg, p.peek())
}

func (p *Parser) check(t lexer.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Type == t
}

func (p *Parser) match(types ...lexer.TokenType) bool {
	for _, t := range types {
		if p.check(t) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *Parser) peek() lexer.Token {
	return p.tokens[p.current]
}

func (p *Parser) peekNext() lexer.Token {
	if p.current+1 >= len(p.tokens) {
		return p.peek()
	}
	return p.tokens[p.current+1]
}

func (p *Parser) previous() lexer.Token {
	return p.tokens[p.current-1]
}

func (p *Parser) isAtEnd() bool {
	return p.peek().Type == lexer.TokenEOF
}
