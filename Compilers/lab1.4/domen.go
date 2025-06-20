package main

// Описание лексических доменов модельного языка в виде регулярных выражений

const (
	Whitespace = "[ \t\n\r]+"
	Identifier = "[a-zA-Z][a-zA-Z0-9]*"
	Number     = "[0-9]+"
	Keyword    = "switch|case"
	Operator   = "[:{}]"
	Comment    = "!.*\n"
)
