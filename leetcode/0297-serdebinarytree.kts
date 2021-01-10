
data class TreeNode(
    var `val`: Int,
    var left: TreeNode? = null,
    var right: TreeNode? = null
)

class Codec() {
    fun serialize(root: TreeNode?): String {
        return if (root == null) {
            ""
        } else {
            val none = TreeNode(-1)
            val dest = mutableListOf<String>()
            val stack = ArrayDeque<TreeNode>(listOf(root))
            while (stack.isNotEmpty()) {
                val n = stack.removeLast()
                if (n === none) {
                    dest.add("n")
                } else {
                    dest.add(n.`val`.toString())
                    stack.addLast(if (n.right == null) none else n.right!!)
                    stack.addLast(if (n.left == null) none else n.left!!)
                }
            }
            dest.joinToString(",")
        }
    }

    fun deserialize(data: String): TreeNode? {
        return if (data == "") {
            null
        } else {
            val cmps = data.split(",")
            val root = TreeNode(cmps.first().toInt())
            val stack = ArrayDeque<Pair<TreeNode, Boolean>>(listOf(root to false))
            var count = 1
            while (count < cmps.size && stack.isNotEmpty()) {
                val (curr, checkedLeft) = stack.removeLast()
                val nextCmp = cmps[count++]
                val nextNode = if (nextCmp == "n") null else TreeNode(nextCmp.toInt())
                if (curr.left == null && !checkedLeft) {
                    curr.left = nextNode
                    stack.addLast(curr to true)
                } else {
                    curr.right = nextNode
                }
                if (nextNode != null) {
                    stack.addLast(nextNode to false)
                }
            }
            root
        }
    }
}

fun testSerde(){
    val tree = TreeNode(1, TreeNode(2), TreeNode(3, TreeNode(4), TreeNode(5)))
    assert("1,2,n,n,3,4,n,n,5,n,n" == Codec().serialize(tree))
    assert(tree == Codec().deserialize("1,2,n,n,3,4,n,n,5,n,n"))
    assert("" == Codec().serialize(null))
    assert(null == Codec().deserialize(""))
    Codec().serialize(TreeNode(5)).also { assert("5,n,n" == it) { it } }
    assert(TreeNode(5) == Codec().deserialize("5,n,n"))
}

testSerde()
