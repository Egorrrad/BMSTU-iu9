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
		params = append(params, Parameter{TypeName: typeName, IsArray: isArray, Pos: p.Current.Coords})
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
			options = append(options, MethodAction{MethodName: action, Expression: expr, Pos: p.Current.Coords})
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
				options = append(options, MethodAction{MethodName: altAction, Expression: altExpr, Pos: p.Current.Coords})
			} else {
				options = append(options, altExpr)
			}
		}

		return Alternative{Options: options, Pos: p.Current.Coords}, "", nil
	}

	if action != "" {
		return MethodAction{MethodName: action, Expression: expr, Pos: p.Current.Coords}, "", nil
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

		if seq, ok := expr.(Sequence); ok && len(seq.Items) >= 2 && seq.Items[0].(Terminal).Value == "COMMA" {
			if hasRep {
				return Repetition{Item: Grouped{Expression: expr, Pos: coords}, Pos: coords}, nil
			}
			return Repetition{Item: Grouped{Expression: expr, Pos: coords}, Pos: coords}, nil
		}

		if hasRep {
			return Repetition{Item: Grouped{Expression: expr, Pos: coords}, Pos: coords}, nil
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
