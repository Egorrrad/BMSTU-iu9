from parser import Node


class Grammar:
    def __init__(self, start_symbol):
        self.start_symbol = start_symbol
        self.rules = {}

    def add_rule(self, lhs, rhs):
        if lhs not in self.rules:
            self.rules[lhs] = []
        self.rules[lhs].extend(rhs)

    def __repr__(self):
        result = [f"Start symbol: {self.start_symbol}"]
        if self.start_symbol in self.rules:
            result.append(
                f"{self.start_symbol} -> {' | '.join([' '.join(prod) for prod in self.rules[self.start_symbol]])}"
            )
        result.extend(
            f"{lhs} -> {' | '.join([' '.join(prod) for prod in rhs])}"
            for lhs, rhs in self.rules.items()
            if lhs != self.start_symbol
        )
        return "\n".join(result)


class Counter:
    def __init__(self):
        self.counters = {"K": 0, "N": 0, "C": 0, "A": 0}

    def next(self, key):
        self.counters[key] += 1
        return f"{key}{self.counters[key] - 1}"


def create_rule(node: Node, lhs, grammar: Grammar, counter: Counter):
    if not node:
        raise ValueError("Node is None!")

    match node.get_type():
        case "LETTER":
            grammar.add_rule(lhs, [[node.get_value()]])
        case "CAPTURE_GROUP":
            group_symbol = f"G{node.get_value()}"
            create_rule(node.get_children()[0], group_symbol, grammar, counter)
            grammar.add_rule(lhs, [[group_symbol]])
        case "NO_CAPTURED":
            non_terminal = counter.next("N")
            create_rule(node.get_children()[0], non_terminal, grammar, counter)
            grammar.add_rule(lhs, [[non_terminal]])
        case "LOOKAHEAD":
            grammar.add_rule(lhs, [["ε"]])
        case "CONCAT":
            concat_symbols = [counter.next("C") for _ in node.get_children()]
            for child, symbol in zip(node.get_children(), concat_symbols):
                create_rule(child, symbol, grammar, counter)
            grammar.add_rule(lhs, [concat_symbols])
        case "ALTERNATIVE":
            for child in node.get_children():
                alt_symbol = counter.next("A")
                create_rule(child, alt_symbol, grammar, counter)
                grammar.add_rule(lhs, [[alt_symbol]])
        case "STAR":
            star_symbol = counter.next("K")
            create_rule(node.get_children()[0], star_symbol, grammar, counter)
            grammar.add_rule(lhs, [["ε"], [lhs, star_symbol]])
        case "REFERENCE_EXPR":
            referenced_group = f"G{node.get_children()[0].get_value()}"
            grammar.add_rule(lhs, [[referenced_group]])
        case _:
            raise ValueError(f"Unknown node type: {node.get_type()}")


def create_cfg(ast):
    grammar = Grammar("S")
    counter = Counter()
    create_rule(ast, grammar.start_symbol, grammar, counter)
    return grammar
