object Solution {
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
        def findI(iMin: Int, iMax: Int, nums1: Array[Int], nums2: Array[Int]): (Int, Int) = {
            val i = (iMin + iMax) / 2
            val j = ((nums1.length + nums2.length + 1) / 2) - i
            (i, j) match {
                case (i, j) if (i < iMax && nums2(j-1) > nums1(i)) =>
                    findI(iMin + 1, iMax, nums1: Array[Int], nums2: Array[Int])
                case (i, j) if (i > iMin && nums1(i-1) > nums2(j)) => 
                    findI(iMin, iMax - 1, nums1: Array[Int], nums2: Array[Int])
                case (i, j) => (i, j)
            }
        }
        def findTwoNum(i: Int, j: Int, nums1: Array[Int], nums2: Array[Int]) = {
            lazy val maxLeft = if (i == 0) nums2(j-1)
                else if (j == 0) nums1(i-1)
                else math.max(nums2(j-1), nums1(i-1))
            lazy val minRight = if (i == nums1.length) nums2(j)
                else if (j == nums2.length) nums1(i)
                else math.min( nums2(j), nums1(i))
            if ((nums1.length + nums2.length) % 2 == 1) maxLeft
            else (maxLeft + minRight) / 2.0            
        }
        if (nums1.length > nums2.length) {
          val (i, j) = findI(0, nums2.length, nums2, nums1)
          findTwoNum(i, j, nums2, nums1)    
        } else {
          val (i, j) = findI(0, nums1.length, nums1, nums2)
          findTwoNum(i, j, nums1, nums2)  
        }
        
    }
}
