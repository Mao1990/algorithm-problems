object Solution {
    def shortestSubarray(A: Array[Int], K: Int): Int = {
        val a = Array.ofDim[Int](A.length + 1)
        for (i <- 0 until A.length) {
            a(i + 1) = a(i) + A(i)
        }
        @scala.annotation.tailrec
        def rec(vec: Vector[Int], res: Int, iter: Int): Int = iter match {
            case x if x == a.length => res
            case x => vec match {
                case v if !v.isEmpty && (a(iter) - a(v.head)) >= K => 
                  rec(v.tail, res min (iter - v.head), iter)
                case v if !v.isEmpty && a(iter) <= a(v.last) => 
                  rec(v.init, res, iter)
                case v => rec(v :+ iter, res, iter + 1)
            }
        }
        val res = rec(Vector[Int](), A.length + 1, 0)
        if (res > A.length) -1 else res
    }
}
