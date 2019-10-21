def is_one_away(left, right):
    """
    Time complexity: O(n)
    Space complexity: O(1)
    """
    len_left, len_right = len(left), len(right)
    if abs(len_left - len_right) > 1:
        return False
    elif len_left == len_right:
        diff_count = sum(1 for (l, r) in zip(left, right) if l != r)
        return diff_count < 2
    else:
        longest = left if len_left > len_right else right
        shortest = right if len_left > len_right else left
        removed, i = False, 0
        while i < len(shortest):
            offset = 1 if removed else 0
            if longest[i + offset] != shortest[i]:
                if removed:
                    return False
                else:
                    removed = True
            else:
                i += 1
        return True


def test():
    cases = [("pale", "ple", True), ("pales", "pale", True), ("pale", "bale", True), ("pale", "bake", False), ("pale", "ble", False)]
    for (left, right, expected) in cases:
        assert is_one_away(left, right) == expected, str((left, right, expected))


test()
