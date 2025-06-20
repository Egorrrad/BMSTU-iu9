package main

type DomainTag int

const (
	IDENT DomainTag = iota
	NUMBER
	KeywordOn
	KeywordOff
	KeywordDoublestar
	EndOfProgram
)

func tagToString(tag DomainTag) string {
	return [...]string{
		"IDENT", "NUMBER",
		"KEYWORD_ON", "KEYWORD_OFF", "KEYWORD_DOUBLESTAR",
		"END_OF_PROGRAM",
	}[tag]
}
