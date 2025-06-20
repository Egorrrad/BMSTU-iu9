package main

import (
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

	compiler := NewCompiler()
	lexer := NewScanner(file, compiler)

	for {
		token := lexer.NextToken()
		fmt.Printf("%s %s", tagToString(token.Tag), token.Coords)
		if token.Value != nil {
			fmt.Printf(": %v", token.Value)
		}
		fmt.Println()

		if token.Tag == EndOfProgram {
			break
		}
	}

	fmt.Println("\nСообщения об ошибках:")
	for msg := compiler.messages.Front(); msg != nil; msg = msg.Next() {
		fmt.Println(msg.Value)
	}

	fmt.Println("\nТаблица идентификаторов:")
	for code, name := range compiler.names {
		fmt.Printf("%d: %s\n", code, name)
	}
}
