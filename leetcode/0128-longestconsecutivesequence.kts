
fun longestConsecutive(nums: IntArray): Int {
    val uniques = nums.toHashSet()
    var maxWindow = 0
    while (uniques.isNotEmpty()) {
        val curr = uniques.first().also { uniques.remove(it) }
        var windowLength = 1
        var bigger = curr + 1
        while (uniques.remove(bigger)) {
            windowLength++
            bigger++
        }
        var smaller = curr - 1
        while (uniques.remove(smaller)) {
            windowLength++
            smaller--
        }
        maxWindow = maxOf(maxWindow, windowLength)
    }
    return maxWindow
}

fun testSolution() {
    longestConsecutive(intArrayOf(100,4,200,1,3,2)).also { assert(it == 4) { it } }
    longestConsecutive(intArrayOf(0,3,7,2,5,8,4,6,0,1)).also { assert(it == 9) { it } }
    longestConsecutive(intArrayOf()).also { assert(it == 0) { it } }
    longestConsecutive(intArrayOf(5)).also { assert(it == 1) { it } }
}

testSolution()