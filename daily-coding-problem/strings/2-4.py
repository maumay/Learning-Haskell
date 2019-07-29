'''
In this exercise I learnt to READ THE QUESTION PROPERLY
'''

def compute_smallest_rotation(k, input):
    if k == 1:
        return compute_smallest_rotation_1(list(input))
    else:
        return "".join(sorted(list(input)))


def compute_smallest_rotation_1(input):
    min_index = min(range(len(input)), key = lambda i: input[i])
    return "".join(input[min_index:] + input[:min_index])


def test():
    first = compute_smallest_rotation(1, 'daily')
    assert 'ailyd' == first, first


test()