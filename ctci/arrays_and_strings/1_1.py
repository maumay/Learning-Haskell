def is_unique1(query):
    """ When additional data structures allowed """
    charset = set()
    for char in query:
        if char in charset:
            return False
        else:
            charset.add(char)
    return True


def is_unique2(query):
    """ When no additional data structure allowed """
    sorted_query = sorted(query)
    for i in range(len(query) - 1):
        if sorted_query[i] == sorted_query[i + 1]:
            return False
    return True


def test(unique_fn):
    cases = [('', True), ('ahgs yk', True), ('bdjsasxkja ', False), ('asgw\t\n\r\t', False)]
    for (query, expected) in cases:
        assert expected == unique_fn(query), str((query, expected))


test(is_unique1)
test(is_unique2)

