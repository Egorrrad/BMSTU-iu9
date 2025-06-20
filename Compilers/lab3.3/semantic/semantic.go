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
			DefinedAt: len(a.spec.Tokens) + len(a.spec.Types) + len(a.spec.Methods) + len(a.spec.Grammar),
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
			Msg: fmt.Sprintf("terminal %s should not have grammar rules", terminal),
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
						Msg: fmt.Sprintf("non-terminal %s has %d rules (should be exactly 1)", name, count),
						Pos: rule.Pos,
					}
				}
			}
		}
	}

	for _, rule := range a.spec.Grammar {
		if err := a.checkNonTerminalReferences(rule.Production, nonTerminals); err != nil {
			return err
		}
	}

	return nil
}

func (a *Analyzer) checkNonTerminalReferences(expr parser.Expression, nonTerminals map[string]bool) error {
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
							Msg: fmt.Sprintf("symbol %s appears in %%types %d times (should be at most 1)", symbol, count),
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
		err = a.checkMethodCompatibility(rule.Name, typ, isArray, rule.Action, rule.Production, table, rule.Pos)
		if err != nil {
			return err
		}
	}
	return nil
}

func (a *Analyzer) checkMethodCompatibility(ruleName string, ruleType string, ruleIsArray bool, methodName string, expr parser.Expression, table *SymbolTable, pos lexer.Fragment) error {
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
					if ma, ok := item.(parser.MethodAction); ok && ma.MethodName == methodName {
						for _, seqItem := range seq.Items {
							if seqItem == item {
								typedElements = append(typedElements, a.collectTypedElements(ma.Expression, table)...)
								break
							}
							typedElements = append(typedElements, a.collectTypedElements(seqItem, table)...)
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
		if i < len(typedElements) && !a.typesMatch(typedElements[i].TypeName, typedElements[i].IsArray, param.TypeName, param.IsArray) {
			return SemanticError{
				Msg: fmt.Sprintf("parameter %d type mismatch in method %s in rule %s: expected %s%s, got %s%s",
					i+1, methodName, ruleName, param.TypeName, map[bool]string{true: "[]", false: ""}[param.IsArray],
					typedElements[i].TypeName, map[bool]string{true: "[]", false: ""}[typedElements[i].IsArray]),
				Pos: pos,
			}
		}
	}

	if !a.typesMatch(ruleType, ruleIsArray, methodSym.Type, methodSym.IsArray) {
		return SemanticError{
			Msg: fmt.Sprintf("type mismatch in rule %s: expected %s%s, got %s%s",
				ruleName, methodSym.Type, map[bool]string{true: "[]", false: ""}[methodSym.IsArray],
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

func (a *Analyzer) collectTypedElements(expr parser.Expression, table *SymbolTable) []Parameter {
	var typed []Parameter
	switch e := expr.(type) {
	case parser.Terminal:
		if typ, isArr := a.getSymbolType(e.Value, table); typ != "" {
			typed = append(typed, Parameter{TypeName: typ, IsArray: isArr})
		} else {
			for _, typeMapping := range a.spec.Types {
				for _, symbol := range typeMapping.Symbols {
					if symbol == e.Value {
						typed = append(typed, Parameter{TypeName: typeMapping.TypeName, IsArray: typeMapping.IsArray})
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

func (a *Analyzer) checkSemantics(expr parser.Expression, table *SymbolTable) (string, bool, error) {
	switch e := expr.(type) {
	case parser.Terminal:
		if _, exists := table.FindSymbol(e.Value, CatNonTerminal); exists {
			return a.checkNonTerminalSemantics(parser.NonTerminal{Name: e.Value, Pos: e.Pos}, table)
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

func (a *Analyzer) checkTerminalSemantics(t parser.Terminal, table *SymbolTable) (string, bool, error) {
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

func (a *Analyzer) checkNonTerminalSemantics(nt parser.NonTerminal, table *SymbolTable) (string, bool, error) {
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

func (a *Analyzer) checkSequenceSemantics(s parser.Sequence, table *SymbolTable, isMethodAction bool) (string, bool, error) {
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

func (a *Analyzer) checkAlternativeSemantics(alt parser.Alternative, table *SymbolTable) (string, bool, error) {
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

func (a *Analyzer) checkMethodActionSemantics(m parser.MethodAction, table *SymbolTable) (string, bool, error) {
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
			Msg: fmt.Sprintf("method %s expects %d parameters, got %d", m.MethodName, len(methodSym.Params), len(typedElements)),
			Pos: m.Pos,
		}
	}
	for i, param := range methodSym.Params {
		if i < len(typedElements) && !a.typesMatch(typedElements[i].TypeName, typedElements[i].IsArray, param.TypeName, param.IsArray) {
			return "", false, SemanticError{
				Msg: fmt.Sprintf("parameter %d type mismatch in method %s: expected %s%s, got %s%s",
					i+1, m.MethodName, param.TypeName, map[bool]string{true: "[]", false: ""}[param.IsArray],
					typedElements[i].TypeName, map[bool]string{true: "[]", false: ""}[typedElements[i].IsArray]),
				Pos: m.Pos,
			}
		}
	}
	return methodSym.Type, methodSym.IsArray, nil
}

func (a *Analyzer) checkUnaryOpSemantics(u parser.UnaryOp, table *SymbolTable) (string, bool, error) {
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
			Msg: fmt.Sprintf("method %s expects %d parameters, got %d", m.MethodName, len(methodSym.Params), len(typedElements)),
			Pos: m.Pos,
		}
	}
	for i, param := range methodSym.Params {
		if i < len(typedElements) && !a.typesMatch(typedElements[i].TypeName, typedElements[i].IsArray, param.TypeName, param.IsArray) {
			return "", false, SemanticError{
				Msg: fmt.Sprintf("parameter %d type mismatch in method %s: expected %s%s, got %s%s",
					i+1, m.MethodName, param.TypeName, map[bool]string{true: "[]", false: ""}[param.IsArray],
					typedElements[i].TypeName, map[bool]string{true: "[]", false: ""}[typedElements[i].IsArray]),
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
