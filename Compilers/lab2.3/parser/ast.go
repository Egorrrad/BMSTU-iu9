package parser

import (
	"fmt"
	"strings"
)

type Node struct {
	Name     string
	Children []*Node
}

func PrintNode(node *Node, depth int) {
	indent := strings.Repeat("  ", depth)
	fmt.Printf("%sNode{\n", indent)
	fmt.Printf("%s  Name: \"%s\",\n", indent, node.Name)
	fmt.Printf("%s  Children: [\n", indent)

	for _, child := range node.Children {
		PrintNode(child, depth+2)
	}

	fmt.Printf("%s  ],\n", indent)
	fmt.Printf("%s}\n", indent)
}
