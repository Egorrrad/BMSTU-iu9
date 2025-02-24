from lexer import Token


class ParserError(SyntaxError):
    def __init__(self, message, pos=None, token=None):
        super().__init__(message)
        self.pos = pos
        self.token = token
        self.message = message

    def __str__(self):
        # Форматированный вывод ошибки с деталями
        token_info = f" (Token: {self.token})" if self.token else ""
        pos_info = f" at position {self.pos}" if self.pos is not None else ""
        return f"{self.message}{token_info}{pos_info}"


class Node:
    def __init__(self, node_type: str, value=None):
        self._type = node_type
        self._value = value
        self._children = []

    def add_child(self, child):
        self._children.append(child)

    def get_children(self):
        return self._children

    def get_type(self):
        return self._type

    def get_value(self):
        return self._value

    def print_tree(self, depth=0):
        indent = "  " * depth
        if self._value:
            print(f"{indent}{self._type}: {self._value}")
        else:
            print(f"{indent}{self._type}")
        for child in self._children:
            child.print_tree(depth + 1)


class Parser:
    def __init__(self, tokens: list[Token]):
        self._tokens = tokens
        self._pos = 0
        self._capture_groups = 1
        self._lookahead_flag = False

    def peek(self):
        return self._tokens[self._pos] if self._pos < len(self._tokens) else None

    def consume(self):
        current_token = self.peek()
        if current_token is None:
            return None
        self._pos += 1
        return current_token.value

    def match(self, token_type):
        if self.peek() and self.peek().type == token_type:
            self.consume()
            return True
        return False

    def raise_error(self, message, token=None):
        raise ParserError(message, pos=self._pos, token=token)

    def parse_lookahead(self):
        lookahead_node = Node("LOOKAHEAD")
        expr = self.parse_rg()
        if expr is None:
            self.raise_error("Lookahead is empty")
        lookahead_node.add_child(expr)
        self._lookahead_flag = False
        self.consume()
        return lookahead_node

    def parse_no_capture(self):
        no_capture_node = Node("NO_CAPTURED")
        expr = self.parse_rg()
        if expr is None:
            self.raise_error("No captured group is empty")
        no_capture_node.add_child(expr)
        self.consume()
        return no_capture_node

    def parse_reference(self):
        ref_num = self.consume()
        self.consume()
        ref_node = Node("REFERENCE_EXPR")
        ref_node.add_child(Node("NUMBER", ref_num))
        return ref_node

    def parse_question(self):
        current = self.peek()
        if current.type == "NO_CAPTURED":
            self.consume()
            return self.parse_no_capture()
        elif current.type == "LOOKAHEAD":
            self._lookahead_flag = True
            self.consume()
            return self.parse_lookahead()
        elif current.type == "REFERENCE_EXPR":
            return self.parse_reference()
        return None

    def parse_rg(self):
        expr = self.parse_primary()

        while True:
            if self.match("STAR") and expr.get_type() != "CONCAT":
                star_node = Node("STAR")
                star_node.add_child(expr)
                expr = star_node
            elif self.match("ALTERNATIVE"):
                alternative_node = Node("ALTERNATIVE")
                alternative_node.add_child(expr)
                alternative_node.add_child(self.parse_rg())
                expr = alternative_node
                if None in expr.get_children():
                    self.raise_error(f"Unexpected token", self.peek())
            else:
                next_expr = self.parse_primary()
                if next_expr:
                    concat_node = Node("CONCAT")
                    concat_node.add_child(expr)
                    concat_node.add_child(next_expr)
                    expr = concat_node
                else:
                    break
        return expr

    def parse_primary(self):
        current = self.peek()
        if current is None:
            return None
        if current.type == "OPEN_BRACKET":
            self.consume()
            current1 = self.peek()
            if self._lookahead_flag and current1 is not None and current1.type not in {"NO_CAPTURED", "REFERENCE_EXPR"}:
                self.raise_error("Unexpected token in lookahead", current)
            if current1 is not None and current1.type in {"NO_CAPTURED", "LOOKAHEAD", "REFERENCE_EXPR"}:
                return self.parse_question()
            group_node = Node("CAPTURE_GROUP", self._capture_groups)
            self._capture_groups += 1
            expr = self.parse_rg()
            if not self.match("CLOSE_BRACKET"):
                self.raise_error("Expected ')'", self.peek())
            if expr is None:
                self.raise_error(f"Capture group {group_node.get_value()} is empty")
            group_node.add_child(expr)
            return group_node
        elif current.type == "LETTER":
            letter_node = Node("LETTER", self.consume())
            if self.peek() and self.peek().type == "STAR":
                star_node = Node("STAR")
                star_node.add_child(letter_node)
                self.consume()
                return star_node
            return letter_node
        elif current.type == "CLOSE_BRACKET":
            return None
        else:
            self.raise_error("Unexpected token!")

    def parse(self):
        result = self.parse_rg()
        if self._pos < len(self._tokens):
            self.raise_error("Unexpected token in expression!")
        if self._capture_groups > 9:
            self.raise_error("The number of capture groups exceeds 9")
        return result
