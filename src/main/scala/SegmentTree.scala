object SegmentTree {

  val N = Math.pow(2, 21).toInt
  //      val N = 8
  val tree = new Array[Int](2 * N)

  def build(a: Array[Int]): Unit = {
    for (i <- N until tree.length) {
      tree(i) = a(i - N)
    }
    for (i <- N - 1 to 1 by -1) {
      tree(i) = tree(2 * i) + tree(2 * i + 1)
    }
  }

  def sum(l: Int, r: Int): Int = {
    var sum = 0
    var left = l + N
    var right = r + N
    while (left < right) {
      if (left % 2 == 1) {
        sum += tree(left)
        left += 1
      }
      if (right % 2 != 1) {
        sum += tree(right)
        right -= 1
      }
      left /= 2
      right /= 2
    }
    if (left == right) {
      return sum + tree(left)
    }
    return sum
  }

  def dec(pos: Int): Unit = {
    var i = pos + N
    while (i > 0) {
      tree(i) -= 1
      i /= 2
    }
  }
}