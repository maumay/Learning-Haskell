class Node:
    def __init__(self, data, next=None):
        self.data = data
        self.next = next

def equal(left_node, right_node):
    left_none, right_none = left_node is None, right_node is None
    if left_none and right_none:
        return True
    elif left_none != right_none:
        return False
    else:
        return (left_node.data == right_node.data 
            and equal(left_node.next, right_node.next))

def string(node):
    if node is None:
        return str(None)
    else:
        return f"Node({node.data}, {string(node.next)})"


'''
Sum two linked lists where 321 is represented as 1 -> 2 -> 3
'''

def next(node):
    if node is None:
        return None
    else:
        return node.next


def data(node):
    if node is None:
        return 0
    else:
        return node.data


def compute_sum(left, right):
    index, residue, result = 0, 0, 0
    while left is not None or right is not None or residue > 0:
        step_sum = data(left) + data(right) + residue
        result += (step_sum % 10) * (10 ** index)
        residue = step_sum // 10
        index += 1
        left, right = next(left), next(right)
    head = Node(result % 10)
    result //= 10
    tmp = head
    while result > 0:
        tmp.next = Node(result % 10)
        tmp = tmp.next
        result //= 10
    return head



left = Node(9, next=Node(1, next=Node(9, next=Node(9))))
right = Node(4, next=Node(5, next=Node(2, next=Node(5))))

print(string(compute_sum(left, right)))