from collections import Counter


def are_palindromes(left, right):
    return len(left) == len(right) and Counter(left) == Counter(right)


def test():
    cases = [("", "", True), ("", "a", False), ("abcdde", "cbdaed", True), ("abcdde", "cbkaed", False)]
    for (left, right, expected) in cases:
        assert expected == are_palindromes(left, right), str((left, right, expected))


test()
