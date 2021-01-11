
class MedianTracker() {
    var node: Node? = null

    fun insert(k: Int) {
        if (node == null) {
            node = Node(key = k, count = 1, size = 1, level = 2)
        } else {
            node = node?.insert(k)
        }
    }

    fun median() = node?.let { it.median() }
}

data class Node (
    // The number this node represents
    var key: Int,
    // How many times this number has
    // appeared in the stream
    var count: Int = 1,
    // The number of stream entries
    // contained in this subtree
    var size: Int = count,
    // For maintaining the balance of the tree
    var level: Int,
    // The left subtree
    var left: Node? = null,
    // The right subtree
    var right: Node? = null,
) {
    fun median(): Int {
        val medianLo = if (size % 2 == 0) size / 2 - 1 else size / 2
        val medianHi = size / 2

        val stack = ArrayDeque<Node>(listOf(this))
        val processed = hashSetOf<Int>()
        var lo = 0
        var medianParts = mutableListOf<Int>()
        while (stack.isNotEmpty()) {
            val curr = stack.removeLast()
            if (curr.left == null || processed.contains(curr.left!!.key)) {
                processed.add(curr.key)
                lo += curr.count
                if (medianLo < lo && medianParts.isEmpty()) {
                    medianParts.add(curr.key)
                }
                if (medianHi < lo) {
                    medianParts.add(curr.key)
                    break
                }
                if (curr.right != null) {
                    stack.addLast(curr.right!!)
                }
            } else {
                stack.addLast(curr)
                val l = curr.left!!
                if (medianLo < lo + l.size) {
                    stack.addLast(l)
                } else {
                    lo += l.size
                    processed.add(l.key)
                }
            }
        }
        return medianParts.sum() / 2
    }

    fun insert(k: Int): Node {
        val (l, r) = left to right
        size++
        if (k == key) {
            count++
        } else if (k > key) {
            right = r?.insert(k) ?: Node(key = k, count = 1, size = 1, level = 2)
        } else {
            left = l?.insert(k) ?: Node(key = k, count = 1, size = 1, level = 2)
        }
        return skew().split()
    }

    fun skew(): Node {
        val l = left
        return if (l == null || l.level != level) {
            this
        } else {
            val lRightSize = l.right?.size ?: 0
            // Do a right rotation preserving sizes
            size -= l.size
            l.size -= lRightSize
            size += lRightSize
            l.size += size
            this.left = l.right
            l.right = this
            l
        }
    }

    fun split(): Node {
        val (right, rightright) = right to right?.right
        return if (right == null || rightright == null || right.level != level || rightright.level != level) {
            this
        } else {
            val oldRightLeft = right.left
            size -= right.size
            right.size -= right.left?.size ?: 0
            size += right.left?.size ?: 0
            right.size += size
            right.left = this
            right.level += 1
            this.right = oldRightLeft
            right
        }
    }
}

//fun testSplit() {
//    // No split required
//    Node(key = 0, level = 3, left = Node(key = -1, level = 2)).let { it to it.split() }.also {
//        assert(it.first == it.second) { it.second }
//    }
//    // No split required
//    Node(key = 0, level = 3, left = Node(key = -1, level = 2), right = Node(key = 1, level = 3))
//        .let { it to it.split() }
//        .also {
//            assert(it.first == it.second) { it.second }
//        }
//    Node(key = 0, level = 3, left = Node(key = -1, level = 2), right = Node(key = 1, level = 2))
//        .let { it to it.split() }
//        .also {
//            assert(it.first == it.second) { it.second }
//        }
//    Node(key = 0, level = 3, left = Node(key = -1, level = 2), right = Node(key = 1, level = 3, right = Node(key = 2, level = 2)))
//        .let { it to it.split() }
//        .also {
//            assert(it.first == it.second) { it.second }
//        }
//    // Split required
//    Node(key = 0, level = 3, left = Node(key = -1, level = 2), right = Node(key = 1, level = 3, right = Node(key = 2, level = 3))).split()
//        .also {
//            assert(it == Node(
//                key = 1,
//                level = 4,
//                left = Node(
//                    key = 0,
//                    level = 3,
//                    left = Node(key = -1, level = 2)
//                ),
//                right = Node(
//                    key = 2,
//                    level = 3
//                )
//            )) { it }
//        }
//
//}
//
//testSplit()

fun testSkew() {
    // No skew required
    Node(key = 0, level = 3, left = Node(key = -1, level = 2), right = Node(key = 2, level = 3)).skew().also {
        assert(it == Node(
            key = 0,
            level = 3,
            left = Node(key = -1, level = 2),
            right = Node(key = 2, level = 3)
        ))
    }
    // Test an actual skew case
    Node(
        key = 0,
        level = 3,
        count = 50,
        size = 1156,
        left = Node(
            key = -3,
            level = 3,
            count = 5,
            size = 106,
            left = Node(key = -4, level = 2, count = 1, size = 1),
            right = Node(key = -2, level = 2, count = 100, size = 100)
        ),
        right = Node(key = 1, level = 2, count = 1000, size = 1000)
    ).skew().also {
        assert(it == Node(
            key = -3,
            level = 3,
            count = 5,
            size = 1156,
            left = Node(key = -4, level = 2, count = 1, size = 1),
            right = Node(
                key = 0,
                level = 3,
                count = 50,
                size = 1150,
                left = Node(key = -2, level = 2, count = 100, size = 100),
                right = Node(key = 1, level = 2, count = 1000, size = 1000)
            )
        ))
    }
}

testSkew()

fun testSolution(stream: List<Int>, expectedMedian: Int?) {
    val tracker = MedianTracker()
    for (n in stream) tracker.insert(n)
    assert(expectedMedian == tracker.median()) {
        "Median: ${tracker.median()}, tracker ${tracker.node ?: "null" }"
    }
}

testSolution(listOf(), null)
testSolution(listOf(2), 2)
testSolution(listOf(3, 3, 4, 1, 6, 8, 4), 4)
testSolution(listOf(3, 3, 4, 1, 6, 8, 4, 12, 9, 10), 5)
testSolution(listOf(3, 3, 4, 1, 6, 8, 4, 12, 9, 10, -2), 4)

