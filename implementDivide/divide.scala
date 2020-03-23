object Solution {
    def divide(dividend: Int, divisor: Int): Int = {
        @scala.annotation.tailrec
        def rec(c: Int, d: Int, acc: Int): Int = c match {
            case c if c >= d => rec(c-d, d, acc + 1)
            case c if c < d => acc
        }
        dividend match {
            case i if i == Int.MinValue => divisor match {
                case 1 => Int.MinValue
                case -1 => Int.MaxValue
                case j if j == Int.MinValue => 1
                case j if j < 0 => rec(Int.MaxValue+j -1 , 0-j, 1)
                case j => 0 - rec(Int.MaxValue-j+1, j, 1)
            }
            case i if i < 0 => divisor match {
                case j if j == Int.MinValue => 0
                case j if j < 0 => rec(0-i, 0-j, 0)
                case j => 0 - rec(0-i, j, 0)
            }
            case i => divisor match {
                case j if j == Int.MinValue => 0
                case j if j < 0 => 0 - rec(i, 0-j, 0)
                case j => rec(i, j, 0)
            }
        }
    }
}
