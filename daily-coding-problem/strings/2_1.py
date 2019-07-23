from collections import Counter

def compute_anagram_indices(w, s):
    ''' 
    Works but is slow, would have been better to use a Counter
    than to sort.
    '''
    n, sorted_w = len(w), sorted(w);
    dest = []
    for i in range(0, len(s) - n + 1):
        if sorted(s[i: i + n]) == sorted_w:
            dest.append(i)
    return dest

def compute_anagram_indices2(w, s):
    '''
    Slightly better but still doing unecessary copies
    '''
    # assume len(w) <= len(s)
    dest, w_len, s_len = [], len(w), len(s)
    word_counter, window_counter = Counter(w), Counter(s[:w_len])
    if word_counter == window_counter:
        dest.append(0)
    for i in range(1, s_len - w_len + 1):
        to_remove, to_add = s[i - 1], s[i + w_len - 1]
        window_counter.subtract(Counter({to_remove: 1}))
        window_counter.subtract(Counter({to_add: -1}))
        if +window_counter == word_counter:
            dest.append(i)
    return dest

def test_result():
    result1 = compute_anagram_indices2('ab', 'abxaba')
    assert [0, 3, 4] == result1, result1
    #result2 = compute_anagram_indices2('abz', 'aazzbazczzab')
    #assert [3, 4, 9]

test_result()
print(int())
