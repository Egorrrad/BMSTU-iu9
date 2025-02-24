from lexer import Lexer, LexerError
from makeCFG import create_cfg
from parser import Parser, ParserError


def main():
    input_regex = input("Write regex: ")
    try:
        lex = Lexer(input_regex)
        print(f"Your regex: {lex.regex}")
        tokens = lex.get_tokens()

        parser = Parser(tokens)
        root = parser.parse()
    except Exception as e:
        if e.__class__ == LexerError:
            print(f"ERROR in Lexer: {e.__str__()}")
        elif e.__class__ == ParserError:
            print(f"ERROR in Parser: {e.__str__()}")
        else:
            print(f"Unexpected error: PANIC!\n{e.__str__()}")
        return
    print("Regular expression is correct")
    # Генерация грамматики
    print("Grammar")
    grammar = create_cfg(root)
    # Вывод результата
    print(grammar)


main()
