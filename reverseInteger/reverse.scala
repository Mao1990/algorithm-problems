object Solution {
    def reverse(x: Int): Int = {
        def helper(s: List[Char], acc: String): String = s match {
            case '-' :: xs => '-' +: helper(xs, acc)
            case x :: xs => helper(xs, x +: acc)
            case Nil => acc
        }
        def helper2(i: Int): Int = if (i % 10 == 0 && i != 0) helper2(i / 10) else i
        val result = helper(helper2(x).toString.toList, "")
        if (result == "") 0 else if (result.toLong.isValidInt) result.toInt else 0
    }
}
