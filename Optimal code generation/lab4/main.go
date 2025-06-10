package main

import (
	"OptimalCodeGen/lab4/cfg"
	"OptimalCodeGen/lab4/lexer"
	"OptimalCodeGen/lab4/parser"
	"OptimalCodeGen/lab4/ssa"
	"fmt"
	"os"
	"os/exec"
)

func main() {
	source := `
x = 10;
if (x > 5) {
    y = x + 1;
} else {
    y = x - 1;
}
z = y;
`

	// Лексический анализ.
	fmt.Println("=== Lexing ===")
	tokens := lexer.Lex(source)
	for _, token := range tokens {
		fmt.Printf("%v\n", token)
	}

	// Синтаксический анализ.
	fmt.Println("\n=== Parsing ===")
	parser := parser.NewParser(tokens)
	ast, err := parser.Parse()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing: %v\n", err)
		os.Exit(1)
	}
	fmt.Println(ast.String())

	// Построение CFG.
	fmt.Println("\n=== Building CFG ===")
	cfgGraph := cfg.BuildCFG(ast)
	err = cfg.VisualizeCFG(cfgGraph, "cfg.dot")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error visualizing CFG: %v\n", err)
		os.Exit(1)
	}
	fmt.Println("CFG graph generated: cfg.dot")

	// Преобразование в SSA.
	fmt.Println("\n=== Converting to SSA ===")
	ssaGraph := ssa.Convert(cfgGraph)

	// Визуализация SSA.
	fmt.Println("\n=== Visualizing SSA ===")
	err = ssa.VisualizeSSA(ssaGraph, "ssa.dot")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error visualizing SSA: %v\n", err)
		os.Exit(1)
	}
	fmt.Println("SSA graph generated: ssa.dot")

	// Генерация изображений с помощью Graphviz.
	fmt.Println("\n=== Generating Images ===")
	if err := checkGraphviz(); err != nil {
		fmt.Fprintf(os.Stderr, "GraphViz not installed or not found: %v\n", err)
		fmt.Println("Skipping image generation. Install GraphViz to generate PNG images.")
	} else {
		if err := generateImage("cfg.dot", "cfg.png"); err != nil {
			fmt.Fprintf(os.Stderr, "Error generating cfg.png: %v\n", err)
		} else {
			fmt.Println("CFG image generated: cfg.png")
		}
		if err := generateImage("ssa.dot", "ssa.png"); err != nil {
			fmt.Fprintf(os.Stderr, "Error generating ssa.png: %v\n", err)
		} else {
			fmt.Println("SSA image generated: ssa.png")
		}
	}
}

// checkGraphviz проверяет наличие Graphviz (dot).
func checkGraphviz() error {
	_, err := exec.LookPath("dot")
	return err
}

// generateImage создаёт PNG-изображение из dot-файла.
func generateImage(dotFile, outputFile string) error {
	cmd := exec.Command("dot", "-Tpng", dotFile, "-o", outputFile)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("%v: %s", err, string(output))
	}
	return nil
}
