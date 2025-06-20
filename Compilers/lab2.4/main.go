package main

import (
	"Compilers/lab2.4/lexer"
	"Compilers/lab2.4/parser"
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

	comp := lexer.NewCompiler()
	scan := lexer.NewScanner(file, comp)

	var tokens []lexer.Token
	for {
		token := scan.NextToken()
		fmt.Printf("%s %s", lexer.TagToString(token.Tag), token.Coords)
		if token.Tag == lexer.IdentifierTag {
			nameCode := token.Value.(int)
			name := comp.Names()[nameCode]
			fmt.Printf(": %s", name)
		} else if token.Value != nil {
			fmt.Printf(": %v", token.Value)
		}
		fmt.Println()
		tokens = append(tokens, token)
		if token.Tag == lexer.EOFTag {
			break
		}
	}

	for e := comp.Messages.Front(); e != nil; e = e.Next() {
		fmt.Println(e.Value)
	}

	p := parser.NewParser(tokens, comp)
	spec, err := p.Parse()
	if err != nil {
		fmt.Println("Parse error:", err)
		fmt.Println(p.Current)
		for e := comp.Messages.Front(); e != nil; e = e.Next() {
			fmt.Println(e.Value)
		}
		os.Exit(1)
	}

	fmt.Println("\nParsed Specification:")
	parser.PrintSpecification(spec)
}
