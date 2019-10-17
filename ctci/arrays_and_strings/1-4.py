from collections import Counter


def palin_perm(query):
    """
    Time complexity: O(n)
    Space complexity: O(n)
    """
    char_counts = Counter(x.lower() for x in query if x != ' ')
    total_chars = sum(char_counts.values())
    odd_counts = (1 for x in char_counts.values() if x % 2 == 1)
    return (total_chars % 2) == sum(odd_counts)


def test():
    cases = [("Taco cat", True), ("aaabbb", False)]
    for (query, expected) in cases:
        assert palin_perm(query) == expected, str((query, expected))
    return


test()
