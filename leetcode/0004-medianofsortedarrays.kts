fun solution(nums1: List<Int>, nums2: List<Int>) : Double {
    val totalLen = nums1.size + nums2.size
    val indicesRequired = if (totalLen % 2 == 0) {
        setOf(totalLen / 2, totalLen / 2 - 1)
    } else {
        setOf(totalLen / 2)
    }
    val (entryDest, indicesAcquired) = mutableListOf<Int>() to mutableSetOf<Int>()
    var (iNums1, iNums2) = 0 to 0
    for (i in 0 until totalLen) {
        val curr = when {
            iNums1 == nums1.size -> nums2[iNums2++]
            iNums2 == nums2.size -> nums1[iNums1++]
            nums1[iNums1] <= nums2[iNums2] -> nums1[iNums1++]
            else -> nums2[iNums2++]
        }
        if (indicesRequired.contains(i)) {
            entryDest.add(curr)
            indicesAcquired.add(i)
        }
        if (indicesAcquired == indicesRequired) {
            break
        }
    }
    return entryDest.sum().toDouble() / entryDest.size
}

fun testExamples() {
    assert(2.0 == solution(listOf(1, 2), listOf(2)))
    assert(2.5 == solution(listOf(1, 2), listOf(3, 4)))
    assert(0.0 == solution(listOf(0, 0), listOf(0, 0)))
    assert(1.0 == solution(listOf(), listOf(1)))
}
