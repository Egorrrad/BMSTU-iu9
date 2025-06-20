package lexer

type DomainTag int

const (
	KeywordTag DomainTag = iota
	NeterminalTag
	StringTag
	EOFTag
	ErrorTag
)

func TagToString(tag DomainTag) string {
	return [...]string{
		"Keyword",
		"Neterminal",
		"String",
		"EOF",
		"Error",
	}[tag]
}
