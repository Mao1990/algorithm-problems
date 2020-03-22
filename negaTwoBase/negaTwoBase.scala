def exppressToInt(a: Array[Int], base: Int): Int = {
  @scala.annotation.tailrec
  def helper(a: List[Int], acc: Int, b: Int): Int = a match {
    case x :: xs => helper(xs, acc + x * scala.math.pow(base, b).toInt, b + 1) 
    case Nil => acc
  }
  helper(a.toList, 0, 0)
}

def intToExpress(t: Int): Array[Int] = {
  def findPosiBit(t: Int, acc: Int, b: Int): Int = 
    if (t <= acc) b * 2 
    else findPosiBit(t, acc + (1 * scala.math.pow(4, b+1)).toInt, b+1)
  def findNageBit(t: Int, acc: Int, b: Int): Int =
    if (t >= acc) b * 2 + 1
    else findNageBit(t, acc + (-2 * scala.math.pow(4, b+1)).toInt, b+1)
  def bitToInt(b: Int): Int = scala.math.pow(-2, b).toInt
    val limitBit: Int = 
      if (t > 0) findPosiBit(t, 1, 0)
      else if (t < 0) findNageBit(t, -2, 0)
      else 0
  val result = Array.ofDim[Int](limitBit + 1)
  @scala.annotation.tailrec
  def helper(t: Int, acc: Array[Int]): Array[Int] = t match {
    case 0 => acc
    case x if x > 0 => 
      acc(findPosiBit(x, 1, 0)) = 1
      helper(t - bitToInt(findPosiBit(x, 1, 0)), acc)
    case x if x < 0 => 
      acc(findNageBit(x, -2, 0)) = 1
      helper(t - bitToInt(findNageBit(x, -2, 0)), acc)
  }
  helper(t, result)
}

def intToExpress(t: Int, base: Int): Array[Int] = {
  def helper(n: Int, acc: Array[Int]): Array[Int] = n match {
    case 0 => acc
    case x => x % base match {
      case n if n < 0 => helper(x/base + 1, acc :+ (n - base))
      case n => helper(x/base, acc :+ n)
    }
  }
  helper(t, Array())
}
