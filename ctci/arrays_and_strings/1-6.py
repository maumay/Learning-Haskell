def compress(query):
    n = len(query)
    if n < 3:
        return query
    counts = compute_counts(query)
    total_len = sum(1 + len(str_count) for (count, str_count) in counts)
    if n <= total_len:
        return query
    else:
        compressed, index = [], 0
        for (count, str_count) in counts:
            compressed.append(query[index])
            compressed.append(str_count)
            index += count
        return "".join(compressed)


def compute_counts(query):
    if len(query) == 0:
        return []
    dest, prev = [1], query[0]
    for i in range(1, len(query)):
        next = query[i]
        if next == prev:
            dest[-1] += 1
        else:
            prev = next
            dest[-1] = (dest[-1], str(dest[-1]))
            dest.append(1)
    dest[-1] = (dest[-1], str(dest[-1]))
    return dest


print(compress("aabcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccaaa"))