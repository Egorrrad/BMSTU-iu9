package lexer

type DomainTag int

const (
	KeywordTag DomainTag = iota
	IdentifierTag
	SemicolonTag
	CommaTag
	ColonTag
	EqualsTag
	SlashTag
	BarTag
	LParTag
	RParTag
	LBracketTag
	RBracketTag
	EOFTag
	ErrorTag
)

func TagToString(tag DomainTag) string {
	return [...]string{
		"Keyword",
		"Identifier",
		"Semicolon",
		"Comma",
		"Colon",
		"Equals",
		"Slash",
		"Bar",
		"LPar",
		"RPar",
		"LBracket",
		"RBracket",
		"EOF",
		"Error",
	}[tag]
}
