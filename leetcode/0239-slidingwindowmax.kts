
fun solution(nums: IntArray, k: Int): IntArray {
    val n = nums.size
    return if (k >= n) {
        intArrayOf(nums.maxOrNull() ?: 0)
    } else if (k == 1) {
        nums
    } else {
        val dest = IntArray(n - k + 1)
        val window = sortedMapOf<Int, Int>()
        for (i in 0 until k) window.incrementCount(nums[i])
        dest[0] = window.lastKey()
        for (i in 1 until dest.size) {
            window.decrementCount(nums[i - 1])
            window.incrementCount(nums[i + k - 1])
            dest[i] = window.lastKey()
        }
        dest
    }
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

solution(intArrayOf(1,3,-1,-3,5,3,6,7), 3).also {
    assert(it.toList() == listOf(3,3,5,5,6,7)) { "Failed: ${it.toList()}" }
}

solution(intArrayOf(1), 1).also {
    assert(it.toList() == listOf(1)) { "Failed: ${it.toList()}" }
}

solution(intArrayOf(1, -1), 1).also {
    assert(it.toList() == listOf(1, -1)) { "Failed: ${it.toList()}" }
}

solution(intArrayOf(-7,-8,7,5,7,1,6,0), 4).also {
    assert(it.toList() == listOf(7,7,7,7,7)) { "Failed: ${it.toList()}" }
}