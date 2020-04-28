object Solution {
    def lengthOfLongestSubstring(s: String): Int = {
        def findNonRepeat(str1: List[Char], str2: List[Char]): List[Char] = {
            str1 match {
                case x :: xs => if (str2.contains(x)) str2 else helper(xs, str2 :+ x)
                case x :: Nil => if (str2.contains(x)) str2 else str2 :+ x
                case Nil => str2
            }
        }
        def scanInput(str: List[Char], len: Int): Int = {
            str match {
                case x :: xs => 
                    helper2(
                        xs,
                        if (len >= helper(x :: xs, Nil).length) len 
                        else helper(x :: xs, Nil).length)
                case x :: Nil => 
                    if (len >= helper(x :: Nil, Nil).length) len 
                    else helper(x :: Nil, Nil).length
                case Nil => 
                    if (len >= helper(Nil, Nil).length) len 
                    else helper(Nil, Nil).length
            }
        }
        helper2(s.toList, 0)
    }
}
