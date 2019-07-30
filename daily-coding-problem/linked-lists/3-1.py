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

def iter_reverse(node):
    curr, prev = node, None
    while curr is not None:
        next_curr = curr.next
        curr.next = prev
        prev = curr
        curr = next_curr
    return prev


def reverse(node):
    head, _ = _reverse(node)
    return head

def _reverse(node):
    if node is None:
        return None, None
    elif node.next is None:
        return node, node
    else:
        rest = node.next
        node.next = None
        r_head, r_tail = _reverse(rest)
        r_tail.next = node
        return r_head, node


# Reverse in place
def test_reverse():
    example = Node(1, next=Node(2, next=Node(3)))
    example = iter_reverse(example)
    assert Node(3, next=Node(2, next=Node(1))) == example, example

#test_reverse()