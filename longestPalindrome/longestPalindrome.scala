object Solution {
    def longestPalindrome(s: String): String = {
        val tS = s.toList.mkString("#","#","#")
        def getR(s: String, i: Int): Int = {
            def helper(s: String, i: Int, r: Int): Int = {
                r match {
                    case a if a >= 0 && a <= math.min(i, s.length-i-1) =>
                      if (s(i-a) == s(i+a)) helper(s, i, r + 1)
                      else a - 1
                    case a => a - 1
                }
            }
            helper(s, i, 0)
        }
        // val resultTp = (0 to tS.length - 1).map(i => (i, getR(tS, i))).sortBy(-_._2).head
        var i, j = 0
        for (x <- 0 to tS.length - 1) {
            if (getR(tS, x) > j) {
                i = x
                j = getR(tS, x)
            }
        }
        // tS.substring(resultTp._1 - resultTp._2, resultTp._1 + resultTp._2 + 1).replace("#","")
        tS.substring(i - j, i + j + 1).replace("#","")
    }
}
