fun firstMissingPositive(nums: IntArray): Int {
    // First rearrange the array so that n is assigned
    // to position n - 1 if 1 <= n <= nums.size
    var counter = 0
    while (counter < nums.size) {
        var curr = nums[counter]
        while (1 <= curr && curr <= nums.size && curr != nums[curr - 1]) {
            val tmp = nums[curr - 1]
            nums[curr - 1] = curr
            curr = tmp
        }
        counter++
    }
    // Find the first number which is missing
    for (i in 0 until nums.size) {
        if (i != nums[i] - 1) {
            return i + 1
        }
    }
    return nums.size + 1
}


fun testSolution() {
    assert(3 == firstMissingPositive(listOf(1, 2, 0).toIntArray()))
    assert(2 == firstMissingPositive(listOf(3, 4, -1, 1).toIntArray()))
    assert(1 == firstMissingPositive(listOf(7, 8, 9, 11, 12).toIntArray()))
    assert(1 == firstMissingPositive(IntArray(0)))
}

testSolution()
