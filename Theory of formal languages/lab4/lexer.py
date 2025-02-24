class LexerError(SyntaxError):
    def __init__(self, message, pos, character):
        super().__init__(message)
        self.pos = pos
        self.character = character
        self.message = message

    def __str__(self):
        # Форматированный вывод ошибки с деталями
        char_info = f" character: '{self.character}'" if self.character else ""
        pos_info = f" at position {self.pos}"
        return f"{self.message}{char_info}{pos_info}"


def is_letter(char: chr) -> bool:
    return char is not None and 'a' <= char <= 'z'


def is_num(char: chr) -> bool:
    return char is not None and char.isdigit() and 1 <= int(char) <= 9


class Token:
    def __init__(self, token_type: str, value=None):
        self.type = token_type
        self.value = value

    def __repr__(self):
        return f"Token({self.type}, {repr(self.value)})"


class Lexer:
    def __init__(self, input_regex: str):
        # убираем пробелы, если они есть
        self.regex = input_regex.replace(' ', '')
        self.len = len(self.regex)
        self._pos = 0
        self._tokens = []

        self._alternative = '|'
        self._star = '*'
        self._open_bracket = '('
        self._close_bracket = ')'
        self._question = '?'
        self._equal = '='
        self._two_dots = ':'

        self._tokenize()

    def raise_error(self, message, character):
        raise LexerError(message, pos=self._pos, character=character)

    def _current(self):
        if self._pos < self.len:
            return self.regex[self._pos]
        return None

    def _next(self):
        self._pos += 1

    def _tokenize(self):
        while self._current() is not None:
            current = self._current()
            match current:
                case self._alternative:
                    self._tokens.append(Token("ALTERNATIVE", current))
                    self._next()
                case self._star:
                    self._tokens.append(Token("STAR", current))
                    self._next()
                case _ if is_letter(current):
                    self._tokens.append(Token("LETTER", current))
                    self._next()
                case self._open_bracket:
                    self._tokens.append(Token("OPEN_BRACKET", current))
                    self._next()
                    current1 = self._current()
                    if current1 == self._question:
                        self._next()
                        current2 = self._current()
                        match current2:
                            case self._equal:
                                self._tokens.append(Token("LOOKAHEAD", current1 + current2))
                                self._next()
                            case self._two_dots:
                                self._tokens.append(Token("NO_CAPTURED", current1 + current2))
                                self._next()
                            case _ if is_num(current2):
                                self._tokens.append(Token("REFERENCE_EXPR", current2))
                                self._next()
                            case _:
                                self.raise_error(f"Unexpected symbol after '{self._question}'", current2)
                case self._close_bracket:
                    self._tokens.append(Token("CLOSE_BRACKET", current))
                    self._next()
                case _:
                    self.raise_error("Unexpected symbol", current)

    def get_tokens(self) -> list[Token]:
        return self._tokens
