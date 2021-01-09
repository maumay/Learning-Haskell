data class MatchWindow(val lo: Int, val hi: Int)

fun solution(s: String, pattern: String): Boolean {
    val npattern = normalise(pattern)
    if (npattern == "*") {
        return true
    }
    val p_split = npattern.split("*")
    return when {
        p_split.size == 1 -> isDirectMatch(s, pattern)
        s.length < p_split.sumBy { it.length } -> false
        else -> {
            val l_anchor = p_split[0]
            val l_anchor_matcher = s.slice(0 until l_anchor.length)
            val r_anchor = p_split.last()
            val r_anchor_matcher = if (r_anchor.isEmpty()) {
                ""
            } else {
                s.slice(s.length - r_anchor.length until s.length)
            }
            val slidingPatterns = p_split.slice(1 until p_split.size - 1)
            val sliding_matcher = if (r_anchor.isEmpty()) {
                s.slice(l_anchor.length until s.length)
            } else {
                s.slice(l_anchor.length until s.length - r_anchor.length)
            }
            isDirectMatch(l_anchor_matcher, l_anchor)
                && isDirectMatch(r_anchor_matcher, r_anchor)
                && slidingMatch(sliding_matcher, slidingPatterns)
        }
    }
}

fun normalise(s: String): String {
    if (s.isEmpty()) {
        return ""
    } else {
        var lastC = s[0]
        val dest = mutableListOf(lastC)
        for (i in 1 until s.length) {
            val c = s[i]
            if (c == '*') {
                if (lastC != c) {
                    dest.add(c)
                }
            } else {
                dest.add(c)
            }
            lastC = c
        }
        return dest.joinToString("")
    }
}

fun getAllMatches(s: String, p: String): List<MatchWindow> {
    val plen = p.length
    return (0 until s.length)
        .filter { i -> isDirectMatch(s.slice(i until minOf(i + plen, s.length)), p) }
        .map { i -> MatchWindow(i, i + plen) }
        .toList()
}

fun isDirectMatch(s: String, pattern: String): Boolean {
    return s.length == pattern.length
        && s.zip(pattern).all { (c, p) -> p == '?' || c == p }
}

fun slidingMatch(s: String, patterns: List<String>): Boolean {
    var lastWindow: MatchWindow? = null
    for (p in patterns) {
        val windows = getAllMatches(s, p)
        when {
            windows.isEmpty() -> return false
            lastWindow == null -> lastWindow = windows[0]
            else -> {
                val lw = lastWindow
                when (val window = windows.find { it.lo >= lw.hi }) {
                    null -> return false
                    else -> lastWindow = window
                }
            }
        }
    }
    return true
}

fun testExamples() {
    assert(!solution("aa", "a"))
    assert(solution("aa", "aa"))
    assert(!solution("ss", "a"))
    assert(solution("aa", "*"))
    assert(!solution("cb", "?a"))
    assert(solution("adceb", "*a*b"))
    assert(solution("adceb", "*a**b"))
    assert(!solution("acdcb", "a*c?b"))
    assert(!solution("mississippi", "m??*ss*?i*pi"))
}

fun testEmpty() {
    assert(solution("", ""))
    assert(solution("", "*"))
    assert(solution("", "**"))
    assert(!solution("", "?"))
}

testExamples()
testEmpty()
