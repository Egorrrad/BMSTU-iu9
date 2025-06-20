package main

import (
	"Compilers/lab2.3/graphviz"
	"Compilers/lab2.3/lexer"
	"Compilers/lab2.3/parser"
	"fmt"
	"log"
	"os"
	"os/exec"
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
		if token.Value != nil {
			fmt.Printf(": %v", token.Value)
		}
		fmt.Println()
		tokens = append(tokens, token)
		if token.Tag == lexer.EOFTag {
			break
		}
	}

	/*
		fmt.Println("\nТаблица нетерминалов:")
		for code, name := range comp.Names() {
			fmt.Printf("%d: %s\n", code, name)
		}

	*/

	p := parser.NewParser(tokens, comp)
	tree, err := p.Parse()
	if err != nil {
		fmt.Println("Parse error:", err)
		os.Exit(1)
	}

	//вывод дерева как есть
	parser.PrintNode(tree, 0)

	filename := "parse_tree.dot"
	err = graphviz.ExportToDot(tree, filename)
	if err != nil {
		fmt.Println("Error exporting to DOT:", err)
		os.Exit(1)
	}

	fmt.Println("Parse tree exported to parse_tree.dot")

	outputFile := "graph.png"
	cmd := exec.Command("dot", "-Tpng", filename, "-o", outputFile)

	if err := cmd.Run(); err != nil {
		log.Fatalf("Error running Graphviz: %v\nMake sure Graphviz is installed and available in PATH", err)
	}
	fmt.Printf("Graph saved in %s\n", outputFile)
}
