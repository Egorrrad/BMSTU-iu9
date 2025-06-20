package parser

import (
	"Compilers/lab2.3/lexer"
	"fmt"
)

/*

вариант:
F  `is "n" `or "(" E ")" `end
T  `is F T1 `end
T1 `is "*" F T1 `or `epsilon `end
`axiom E  `is T E1 `end
E1 `is "+" T E1 `or `epsilon `end

Лексическая структура:
axiom	= `axiom
epsilon =	`epsilon
end =	`end
or =	`or
is =	`is
n =	[A-Z][A-Z0-9]*
string =	^\"[^\"]\"

Грамматика:
S   → K S | ε
K   → L is R end
L   → axiom n | n
R   → F A T
A   → epsilon A | n A | string A | ε
T   → or F A T | ε
F   → epsilon | n | string

*/

type Production []string

var ParseTable = map[string]map[string]Production{
	"S": {
		"`axiom":     {"K", "S"},
		"Neterminal": {"K", "S"},
		"EOF":        {}, // ε
	},
	"K": {
		"`axiom":     {"L", "`is", "R", "`end"},
		"Neterminal": {"L", "`is", "R", "`end"},
	},
	"L": {
		"`axiom":     {"`axiom", "Neterminal"},
		"Neterminal": {"Neterminal"},
	},
	"R": {
		"`epsilon":   {"F", "A", "T"},
		"Neterminal": {"F", "A", "T"},
		"String":     {"F", "A", "T"},
	},
	"A": {
		"`epsilon":   {"`epsilon", "A"},
		"Neterminal": {"Neterminal", "A"},
		"String":     {"String", "A"},
		"`or":        {}, // ε
		"`end":       {}, // ε
	},
	"T": {
		"`or":  {"`or", "F", "A", "T"},
		"`end": {}, // ε
	},
	"F": {
		"`epsilon":   {"`epsilon"},
		"Neterminal": {"Neterminal"},
		"String":     {"String"},
	},
}

func isTerminal(symbol string) bool {
	return symbol != "S" && ParseTable[symbol] == nil
}

func terminalOf(token lexer.Token) string {
	switch token.Tag {
	case lexer.KeywordTag:
		return fmt.Sprintf("%v", token.Value)
	case lexer.NeterminalTag:
		return "Neterminal"
	case lexer.StringTag:
		return "String"
	case lexer.EOFTag:
		return "EOF"
	default:
		return ""
	}
}

func matchTerminal(expected string, token lexer.Token) bool {
	switch expected {
	case "Neterminal":
		return token.Tag == lexer.NeterminalTag
	case "String":
		return token.Tag == lexer.StringTag
	case "EOF":
		return token.Tag == lexer.EOFTag
	default:
		return token.Tag == lexer.KeywordTag && fmt.Sprintf("%v", token.Value) == expected
	}
}
