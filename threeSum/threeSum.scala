object Solution {
    def threeSum(nums: Array[Int]): List[List[Int]] = {
        val sortedNums = nums.toList.sorted
        @scala.annotation.tailrec
        def helper(nums: List[Int], acc: List[List[Int]]): List[List[Int]] = {
            @scala.annotation.tailrec
            def helper2(t: Int, ns: List[Int], acc: List[List[Int]]): List[List[Int]] = {
                ns match {
                    case ls if ls.length < 2 => acc
                    case ls if (t + ls.head + ls.last) == 0 => 
                        helper2(t, ns.tail, acc :+ List(t, ls.head + ls.last))
                    case ls if (t + ls.head + ls.last) > 0 => 
                        helper2(t, ns.init, acc)
                    case ls if (t + ls.head + ls.last) < 0 => 
                        helper2(t, ns.tail, acc)
                }
            }
            nums match {
                case x :: xs if xs.length >= 2 => 
                    helper(xs, acc ++ helper2(x, xs, acc))
                case _ => acc
            }
        }
        helper(sortedNums, Nil)
    }
}


    def threeSum(nums: Array[Int]): List[List[Int]] = {
        val sortedNums = nums.sorted
        var results: List[List[Int]] = List()
        for (i <- 0 until nums.length - 2) {
            var j = i + 1
            var k = nums.length - 1
            while (j < k) {
                val v = sortedNums(i) + sortedNums(j) + sortedNums(k)
                if (v == 0) 
                  results = results :+ List(sortedNums(i), sortedNums(j), sortedNums(k))
                  j += 1
                else if (v > 0)
                  k -= 1
                else 
                  j += 1
            }
        }
        results
    }