def parse(code):
    tokens = code.split()
    articles = {}
    i = 0

    def parse_body(end_keywords):
        nonlocal i
        res = []
        while i < len(tokens):
            token = tokens[i]
            if token in end_keywords:
                i += 1
                break
            elif token == "if":
                i += 1
                condition = parse_body(["endif"])
                res.append(["if", condition])
            elif token == "while":
                i += 1
                condition = parse_body(["do"])
                body = parse_body(["wend"])
                res.append(["while", condition, body])
            elif token == "define":
                if not res:
                    i += 1
                    if i >= len(tokens):
                        return None
                    word = tokens[i]
                    i += 1
                    body = parse_body(["end"])
                    articles[word] = body
                else:
                    return None
            else:
                try:
                    res.append(int(token))
                except ValueError:
                    res.append(token)
                i += 1
        return res


    try:
        main_body = parse_body([])
        if main_body is None or len(main_body) == 0:
            return None

        if i != len(tokens):
            return None
        return [articles, main_body]
    except:
        return None

print(parse("define abs dup 0 < if -1 * endif end 10 abs -10 abs"))

print(parse("x dup 0 swap if drop -1 endif"))

print(parse("""define -- 1 - end
define =0? dup 0 = end
define =1? dup 1 = end
define factorial
    =0? if drop 1 exit endif
    =1? if drop 1 exit endif
    1 swap
    while dup 0 > do
        1 - swap over * swap
    wend
    drop
end
0 factorial
1 factorial
2 factorial
3 factorial
4 factorial"""))

print(parse("define word w1 w2 w3"))
