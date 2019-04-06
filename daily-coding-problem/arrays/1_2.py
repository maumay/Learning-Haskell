
"""
This was my answer that took half an hour. Spent roughly
7 minutes thinking and then the rest implementing. Need
to get faster at typing.
"""


def answer(xs):
    # assume non empty
    forward_max, backward_min = [xs[0]], [xs[-1]]
    for i in range(1, len(xs)):
        forward_max.append(max(xs[i], forward_max[-1]))
        backward_min.append(min(xs[-1 - i], backward_min[-1]))
    backward_min = list(reversed(backward_min))
    # Now find bounds
    lo, lo_max = 0, len(xs) - 1
    hi, hi_min = lo_max, lo
    while lo < lo_max and forward_max[lo] <= backward_min[lo + 1]:
        lo += 1
    while hi > hi_min and forward_max[hi - 1] <= backward_min[hi]:
        hi -= 1
    return lo, hi


def test_answer():
    case1 = answer([3, 7, 5, 6, 9])
    assert case1 == (1, 3), case1

    case2 = answer([1, 2, 3, 4, 5])
    assert case2 == (4, 0), case2


test_answer()
