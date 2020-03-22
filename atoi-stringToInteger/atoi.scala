object Solution {
    def myAtoi(str: String): Int = {
        val pattern = "(^\\s*)([+-]{0,1}\\d+)".r
        val tempDouble = {
            val tempArr = pattern.findAllIn(str).toArray
            if (!tempArr.isEmpty) tempArr.head.toDouble else 0.0
        }
        if (tempDouble.isValidInt) tempDouble.toInt 
        else if (tempDouble < Int.MinValue) Int.MinValue
        else if (tempDouble > Int.MaxValue) Int.MaxValue
        else 0
    }
}
