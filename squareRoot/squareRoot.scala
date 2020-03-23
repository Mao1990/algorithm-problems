def sqRoot(i: Int): Double = {
    @scala.annotation.tailrec
    def rec(t: Int, low: Double, up: Double, limit: Int): Double = {
        val mid: Double = (low + up)/2
        if (limit < 0) {
            mid
        } else {
            if (mid*mid > t) rec(t, low, mid, limit - 1)
            else if (mid*mid < t) rec(t, mid, up, limit - 1)
            else mid
        }
    }
    rec(i, 0, i, 1000)
}
