object Solution {
    def isMatch(s: String, p: String): Boolean = {
        def helper(s: List[Char], p: List[Char]): Boolean = p match {
            case Nil => s.isEmpty
            case p :: '*' :: ps => s match {
                case s :: ss if (p == s || p == '.') => 
                    helper(ss, p :: '*' :: ps) || helper(s :: ss, ps)
                case ss => helper(ss, ps)
            }
            case p :: ps => s match {
                case s :: ss if (p == s || p == '.') => helper(ss, ps)
                case _ => false
            }
        }
        helper(s.toList, p.toList)
    }
}
