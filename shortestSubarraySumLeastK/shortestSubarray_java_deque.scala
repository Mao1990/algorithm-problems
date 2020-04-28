object Solution {
    def shortestSubarray(A: Array[Int], K: Int): Int = {
        val a = Array.ofDim[Int](A.length + 1)
        for (i <- 0 until A.length) {
            a(i + 1) = a(i) + A(i)
        }
        var res = A.length + 1
        val que = new java.util.ArrayDeque[Int]
        for (j <- 0 until a.length) {
            while(que.size > 0 && a(j) - a(que.getFirst()) >= K) {
                res = res min (j - que.pollFirst())
            }
            while(que.size > 0 && a(j) <= a(que.getLast())) {
                que.pollLast()
            }
            que.addLast(j)
        }
        if (res > A.length) -1 else res
    }
}
