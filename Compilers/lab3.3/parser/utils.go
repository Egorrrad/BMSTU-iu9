package parser

import (
	"fmt"
	"strings"
)

func PrintSpecification(spec *Specification) {
	fmt.Printf("ClassName: %s (at %s)\n", spec.ClassName, spec.Pos.String())
	fmt.Println("Tokens:", strings.Join(spec.Tokens, ", "))
	fmt.Println("Types:")
	for _, t := range spec.Types {
		fmt.Printf("  Symbols: %s, TypeName: %s", strings.Join(t.Symbols, ", "), t.TypeName)
		if t.IsArray {
			fmt.Print("[]")
		}
		fmt.Printf(" (at %s)\n", t.Pos.String())
	}
	fmt.Println("Methods:")
	for _, m := range spec.Methods {
		fmt.Printf("  %s", m.ReturnType)
		if m.IsArray {
			fmt.Print("[]")
		}
		fmt.Printf(" %s(", m.Name)
		for i, p := range m.Params {
			if i > 0 {
				fmt.Print(", ")
			}
			fmt.Print(p.TypeName)
			if p.IsArray {
				fmt.Print("[]")
			}
		}
		fmt.Printf(") (at %s)\n", m.Pos.String())
	}
	fmt.Println("Grammar:")
	for _, r := range spec.Grammar {
		fmt.Printf("  %s = ", r.Name)
		PrintExpression(r.Production, 2)
		if r.Action != "" {
			fmt.Printf(" / %s", r.Action)
		}
		fmt.Printf(" (at %s)\n", r.Pos.String())
	}
	fmt.Printf("Axiom: %s (at %s)\n", spec.Axiom, spec.Pos.String())
}

func PrintExpression(expr Expression, indentLevel int) {
	indent := strings.Repeat("  ", indentLevel)
	switch e := expr.(type) {
	case Sequence:
		fmt.Printf("Sequence{\n%s", indent)
		for i, item := range e.Items {
			if i > 0 {
				fmt.Printf(",\n%s", indent)
			}
			PrintExpression(item, indentLevel+1)
		}
		if e.Action != "" {
			fmt.Printf("\n%s/ %s", indent, e.Action)
		}
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case Alternative:
		fmt.Printf("Alternative{\n%s", indent)
		for i, opt := range e.Options {
			if i > 0 {
				fmt.Printf("\n%s| ", indent)
			} else {
				fmt.Printf("%s", indent)
			}
			PrintExpression(opt, indentLevel+1)
		}
		if e.Action != "" {
			fmt.Printf("\n%s/ %s", indent, e.Action)
		}
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case Repetition:
		fmt.Printf("Repetition{\n%s", indent)
		PrintExpression(e.Item, indentLevel+1)
		if e.Action != "" {
			fmt.Printf("\n%s/ %s", indent, e.Action)
		}
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case Terminal:
		fmt.Printf("Terminal{%s} (at %s)", e.Value, e.Pos.String())
	case NonTerminal:
		fmt.Printf("NonTerminal{%s} (at %s)", e.Name, e.Pos.String())
	case Grouped:
		fmt.Printf("Grouped(\n%s", indent)
		PrintExpression(e.Expression, indentLevel+1)
		if e.Action != "" {
			fmt.Printf("\n%s/ %s", indent, e.Action)
		}
		fmt.Printf("\n%s) (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case UnaryOp:
		fmt.Printf("UnaryOp{\n%s  Op: %s,\n%s  Right: ", indent, e.Op, indent)
		PrintExpression(e.Right, indentLevel+1)
		if e.Action != "" {
			fmt.Printf(",\n%s  Action: %s", indent, e.Action)
		}
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case MethodAction:
		fmt.Printf("MethodAction{\n%s  Method: %s,\n%s  Expr: ", indent, e.MethodName, indent)
		PrintExpression(e.Expression, indentLevel+1)
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case BinaryOp:
		fmt.Printf("BinaryOp{\n%s  Left: ", indent)
		PrintExpression(e.Left, indentLevel+1)
		fmt.Printf(",\n%s  Op: %s,\n%s  Right: ", indent, e.Op, indent)
		PrintExpression(e.Right, indentLevel+1)
		if e.Action != "" {
			fmt.Printf(",\n%s  Action: %s", indent, e.Action)
		}
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	case MethodCall:
		fmt.Printf("MethodCall{\n%s  Method: %s,\n%s  Args: ", indent, e.MethodName, indent)
		for i, arg := range e.Args {
			if i > 0 {
				fmt.Printf(",\n%s", indent)
			}
			PrintExpression(arg, indentLevel+1)
		}
		fmt.Printf("\n%s} (at %s)", strings.Repeat("  ", indentLevel-1), e.Pos.String())
	default:
		fmt.Printf("UnknownExpressionType (at unknown position)")
	}
}
