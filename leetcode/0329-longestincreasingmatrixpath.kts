
data class Pos(val row: Int, val col: Int)

data class Matrix(val entries: Array<IntArray>, val ncol: Int) {

    fun entry(row: Int, col: Int) = entries[row][col]

    fun legalNeighbours(pos: Pos): List<Pos> {
        val currEntry = entry(pos.row, pos.col)
        val dest = mutableListOf<Pos>()
        if (pos.row > 0 && currEntry < entry(pos.row - 1, pos.col))
            dest.add(Pos(pos.row - 1, pos.col))
        if (pos.row < entries.size - 1 && currEntry < entry(pos.row + 1, pos.col))
            dest.add(Pos(pos.row + 1, pos.col))
        if (pos.col > 0 && currEntry < entry(pos.row, pos.col - 1))
            dest.add(Pos(pos.row, pos.col - 1))
        if (pos.col < ncol - 1 && currEntry < entry(pos.row, pos.col + 1))
            dest.add(Pos(pos.row, pos.col + 1))
        return dest
    }
}

fun solution(matrix: Array<IntArray>): Int {
    // Identify all possible start points and then
    // depth first search each? Seems like brute force

    // if P0 and P1 are the start/end points of the
    // path then all neighbours of P0 are >= and at least one >
    //       and all neighbours of P1 are <= and at least one <

    // Keep track of the nodes that were visted and how many steps
    // it took to reach on the longest path

    // Keep track of the longest path computed from each node,
    // then for new path which reaches that node you can immediately
    // return the result, then iterate over the possible start
    // points doing dfs
    if (matrix.isEmpty()) {
        return 0
    }
    val m = Matrix(matrix, matrix.first().size)
    val store = hashMapOf<Pos, Int>()
    var maxPathLength = 1
    val stack = ArrayDeque<Pos>()
    for (row in 0 until m.entries.size) {
        for (col in 0 until m.ncol) {
            val pos = Pos(row, col)
            if (!store.containsKey(pos)) {
                stack.addLast(pos)
                while (stack.isNotEmpty()) {
                    val next = stack.removeLast()
                    val neighbours = m.legalNeighbours(next)
                    val uncomputedNeighbours = neighbours.filterNot(store::containsKey)
                    if (uncomputedNeighbours.isNotEmpty()) {
                        stack.addLast(next)
                        stack.addAll(uncomputedNeighbours)
                    } else {
                        val pathLenFromNext = 1 + (neighbours.mapNotNull { store.get(it) }.maxOrNull() ?: 0)
                        maxPathLength = maxOf(maxPathLength, pathLenFromNext)
                        store.put(next, pathLenFromNext)
                    }
                }
            }
        }
    }

    return maxPathLength
}


fun testSolution() {
    solution(arrayOf(intArrayOf(9,9,4), intArrayOf(6,6,8), intArrayOf(2,1,1))).also {
        assert(it == 4) { it }
    }

    solution(arrayOf(intArrayOf(3,4,5), intArrayOf(3,2,6), intArrayOf(2,2,1))).also {
        assert(it == 4) { it }
    }

    solution(arrayOf(intArrayOf(3,5,4))).also {
        assert(it == 2) { it }
    }

    solution(arrayOf()).also {
        assert(it == 0) { it }
    }
}

testSolution()