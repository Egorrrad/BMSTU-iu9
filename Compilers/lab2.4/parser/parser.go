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
