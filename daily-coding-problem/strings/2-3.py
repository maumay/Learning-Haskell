'''
1/ First split the string into substrings by the line the letter will be printed on.
2/ Each line can then be printed with three value, the initial offset, down-gap, up-gap
   where d + u = k (unless periphery lines which just have one gap)
'''

def init_lines(k, input):
    dest = [[input[0]]]
    for i in range(1, k):
        dest.append([])
    return dest


def split_string(k, input):
    lines = init_lines(k, input)
    direction_switch_count, move_down, line_index = 1, True, 1
    for letter in input[1:]:
        lines[line_index].append(letter)
        if direction_switch_count % (k - 1) == 0:
            move_down = not move_down
            direction_switch_count = 1
        else:
            direction_switch_count += 1
        line_index += 1 if move_down else -1
    return lines


def test_split_string():
    example_result = [''.join(x) for x in split_string(4, 'thisisazigzag')]
    assert ['tag', 'hsza', 'iiiz', 'sg'] == example_result, example_result


test_split_string()

def odd(i):
    return 2 * i - 1

def print_inner_line(k, line_index, letters):
    offset = line_index
    down_gap = odd(k - 1 - line_index)
    up_gap = odd(line_index)
    padded_line = [' '] * offset
    padded_line.append(letters[0])
    for i, letter in enumerate(letters[1:]):
        gap = ' ' * (down_gap if (i % 2 == 0) else up_gap)
        padded_line.append(gap)
        padded_line.append(letter)
    print(''.join(padded_line))

def print_end_line(k, top, letters):
    offset = 0 if top else k - 1
    gap = odd(k - 1)
    padded_line = [' '] * offset
    padded_line.append(letters[0])
    for letter in letters[1:]:
        padded_line.extend(' ' * gap)
        padded_line.append(letter)
    print(''.join(padded_line))

print_end_line(4, True, 'tag')
print_inner_line(4, 1, 'hsza')
print_inner_line(4, 2, 'iiiz')
print_end_line(4, False, 'sg')



