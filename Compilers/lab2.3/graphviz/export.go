package graphviz

import (
	"Compilers/lab2.3/parser"
	"fmt"
	"os"
)

func ExportToDot(root *parser.Node, filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	fmt.Fprintln(file, "digraph ParseTree {")
	id := 0
	exportNode(file, root, &id)
	fmt.Fprintln(file, "}")

	return nil
}

func exportNode(file *os.File, node *parser.Node, id *int) int {
	currentID := *id
	fmt.Fprintf(file, "  node%d [label=\"%s\"];\n", currentID, node.Name)
	*id++

	for _, child := range node.Children {
		childID := exportNode(file, child, id)
		fmt.Fprintf(file, "  node%d -> node%d;\n", currentID, childID)
	}

	return currentID
}
