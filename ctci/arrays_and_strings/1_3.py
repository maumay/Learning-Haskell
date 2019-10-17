def urlify(query, size):
    """
    Time complexity: O(n)
    Space complexity: O(1)
    """
    offset = len(query) - 1
    for i in range(size - 1, -1, -1):
        c = query[i]
        if c == ' ':
            query[offset - 2: offset + 1] = ['%', '2', '0']
            offset -= 3
        else:
            query[offset] = c
            offset -= 1
    return


query = list("Mr John Smith    ")
print(query)
urlify(query, 13)
print(query)
