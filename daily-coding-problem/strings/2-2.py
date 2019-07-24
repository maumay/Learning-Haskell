'''
If two words have same length parity then we only need to check 
one ordered tuple, if they have different parity do we need to
check both ways?

This way works but the complexity isn't much better than brute 
force, they used a clever prefix/suffix trick with dictionaries
in the book.
'''

def palindrome_pairs(words):
    dest = []
    even_words, odd_words = split_words_on_parity(words)
    dest.extend(equal_parity_palindrome_pairs(even_words))
    dest.extend(equal_parity_palindrome_pairs(odd_words))
    for i in range(len(even_words)):
        for j in range(len(odd_words)):
            word_i, word_j = even_words[i], odd_words[j]
            if is_concatenated_palindrome(word_i[1], word_j[1]):
                dest.append((word_i[0], word_j[0]))
            if is_concatenated_palindrome(word_j[1], word_i[1]):
                dest.append((word_j[0], word_i[0]))
    return dest


def equal_parity_palindrome_pairs(words):
    dest = []
    for i in range(len(words)):
        for j in range(i + 1, len(words)):
            word_i, word_j = words[i], words[j]
            if is_concatenated_palindrome(word_i[1], word_j[1]):
                dest.append((word_i[0], word_j[0]))
                dest.append((word_j[0], word_i[0]))
    return dest


def split_words_on_parity(words):
    even, odd = [], []
    for i, word in enumerate(words):
        if len(word) % 2 == 0:
            even.append((i, word))
        else:
            odd.append((i, word))
    return even, odd


def is_concatenated_palindrome(left, right):
    total = left + right
    for i in range(len(total) // 2):
        if not total[i] == total[-1 - i]:
            return False
    return True 


def palindrome_test():
    assert is_concatenated_palindrome("code", "edoc")
    assert is_concatenated_palindrome("code", "fedoc")
    assert is_concatenated_palindrome("da", "d")
    assert not is_concatenated_palindrome("dacaaa", "d")


def test():
    first_result = set(palindrome_pairs(['code', 'edoc', 'da', 'd']))
    assert set([(0, 1), (1, 0), (2, 3)]) == first_result, first_result


palindrome_test()
test()