from lab4.lexer import Lexer, LexerError
from lab4.parser import Parser, ParserError


def test(filename: str = "tests.txt"):
    with open(filename, 'r') as file:
        line = file.readline()
        while line:
            test_exr = line.split(' ')
            test_reg, expected_res = test_exr[0], int(test_exr[1].replace('\n', ''))
            try:
                lex = Lexer(test_reg)
                tokens = lex.get_tokens()
                parser = Parser(tokens)
                root = parser.parse()
                res = 1
            except Exception as e:
                if e.__class__ in [LexerError, ParserError]:
                    res = 0
                else:
                    print(e.__str__())
                    return
            line = file.readline()
            if res == expected_res:
                print(f"Test: {test_exr} passed")
            else:
                print(f"Test: {test_exr} FAILED")


test()
