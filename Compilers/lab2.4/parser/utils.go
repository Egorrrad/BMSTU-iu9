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
		fmt.Printf("  Symbols: %s, TypeName: %s, IsArray: %t\n", strings.Join(t.Symbols, ", "), t.TypeName, t.IsArray)
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
