
fun solution(nums: IntArray): List<Int> {
    val elementCounts = sortedMapOf<Int, Int>()
    for (num in nums) elementCounts.incrementCount(num)
    val dest = ArrayList<Int>(nums.size)
    for (i in 0 until nums.size) {
        elementCounts.decrementCount(nums[i])
        dest.add(elementCounts.headMap(nums[i]).values.sum())
    }
    return dest
}

fun MutableMap<Int, Int>.decrementCount(key: Int) {
    if (this.containsKey(key)) {
        val currCount = this[key] ?: 1
        if (currCount == 1)
            this.remove(key)
        else
            this.put(key, currCount - 1)
    }
}

fun MutableMap<Int, Int>.incrementCount(key: Int) {
    if (this.containsKey(key)) {
        this.put(key, this[key]!! + 1)
    } else {
        this[key] = 1
    }
}

solution(intArrayOf(5,2,6,1)).also {
    assert(it == listOf(2,1,1,0)) { it }
}

solution(intArrayOf()).also {
    assert(it == listOf<Int>()) { it }
}

solution(intArrayOf(4)).also {
    assert(it == listOf<Int>(0)) { it }
}
