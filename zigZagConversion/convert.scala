object Solution {
    def convert(s: String, numRows: Int): String = {
        val l = s.length
        val group = (0 to numRows-1) ++ (numRows-2 to 1 by -1)
        val t = l/numRows
        val groups = (0 to t).flatMap(_ => group)
        val sTp = s.zip(groups)
        (0 to numRows-1).flatMap{
            i => sTp.filter(tp => tp._2 == i).map(_._1)
        }.mkString
    }
}
