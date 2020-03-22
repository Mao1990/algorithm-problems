import scala.annotation.tailrec
object Solution {  
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
        @tailrec
        def helper(iterNum: Int, nums: List[Int]):Array[Int] = nums match {
            case x :: xs => xs.indexOf(target - x) match {
                case i if i >=0 => Array(iterNum, i+iterNum+1)
                case i => helper(iterNum + 1, xs)
            }
            case x :: Nil => Array(-1 ,-1)
        }
        helper(0, nums.toList)
    }
} 
