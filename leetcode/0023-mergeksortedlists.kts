
data class ListNode(var `val`: Int, var next: ListNode? = null)

fun mergeLists(lists: Array<ListNode?>): ListNode? {
    val populated = lists.filterNotNull().toMutableList()
    if (populated.isEmpty()) {
        return null
    }
    var dest: ListNode? = null
    while (populated.size > 1) {
        populated.sortBy { -it.`val` }
        val secondVal = populated[populated.size - 2].`val`
        var min = populated.last()
        while (min.`val` <= secondVal) {
            val tmp = dest
            dest = ListNode(min.`val`)
            dest.next = tmp
            if (min.next == null) {
                populated.removeLast()
                break
            } else {
                min = min.next!!
                populated[populated.size - 1] = min
            }
        }
    }
    // Reverse dest and append the last populated list
    var reversed = populated.first()
    while (dest != null) {
        val next = dest.next
        dest.next = reversed
        reversed = dest
        dest = next
    }
    return reversed
}

fun testSolution() {
    mergeLists(emptyArray()).also { assert(null == it) { it ?: "null" } }
    mergeLists(arrayOf(ListNode(1, ListNode(2, ListNode(3))))).also {
        assert(ListNode(1, ListNode(2, ListNode(3))) == it) { it ?: "null" }
    }
    mergeLists(arrayOf(
        ListNode(1, ListNode(4, ListNode(5))),
        ListNode(1, ListNode(3, ListNode(4))),
        ListNode(2, ListNode(6))
    )).also {
        assert(
            ListNode(1, ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(4, ListNode(5, ListNode(6)))))))) == it
        ) { it ?: "null" }
    }
}

testSolution()
