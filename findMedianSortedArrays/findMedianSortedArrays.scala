object Solution {
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
        val medianIndex = {
            val total = nums1.length + nums2.length
            (total / 2).toInt + (total % 2)
        }
        def helper(f: (Int, Int) => Boolean)(
            nums1: List[Int], 
            nums2: List[Int], 
            targetIndex: Int): Int = { 
            (nums1, nums2) match {
                case (x :: xs, y :: ys) if f(x, y) => 
                  if (targetIndex == 1) {
                      x
                  } else {
                      helper(f)(xs, y :: ys, targetIndex - 1)
                  }
                case (x :: xs, y :: ys) if !f(x, y) =>
                  if (targetIndex == 1) {
                      y
                  } else {
                      helper(f)(x :: xs, ys, targetIndex - 1)
                  }
                case (Nil, y :: ys) => 
                  if(targetIndex == 1) {
                      y
                  } else {
                      helper(f)(Nil, ys, targetIndex - 1)
                  }
                case (x :: xs, Nil) => 
                  if(targetIndex == 1) {
                    x  
                  } else {
                      helper(f)(xs, Nil, targetIndex - 1)
                  }
            }
        }
        val a = helper(
            (a: Int, b: Int) => a <= b)(
            nums1.toList, nums2.toList, medianIndex)
        val b = helper(
            (a: Int, b: Int) => a >= b)(
            nums1.toList.reverse, nums2.toList.reverse, medianIndex)
        (a + b).toDouble/2
    }
}
