package leetcode.medium

class TreeNode(var `val`: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}

fun deepestLeavesSum(root: TreeNode): Int {
    val levelSums = mutableListOf<Int>()
    computeLevelSums(root, levelSums, 0)
    return levelSums.last()
}

private fun computeLevelSums(root: TreeNode?, levelSums: MutableList<Int>, level: Int) {
    root?.let {
        if (level >= levelSums.size) levelSums.add(0)
        levelSums[level] += root.`val`
        computeLevelSums(root.left, levelSums, level + 1)
        computeLevelSums(root.right, levelSums, level + 1)
    }
}