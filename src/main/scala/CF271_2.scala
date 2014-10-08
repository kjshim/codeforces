import scala.annotation.tailrec

object    CF271_2 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  def binarySearch(a: Array[Long], needle: Int): Int = {
    @tailrec
    def binarySearch(low: Int, high: Int): Int = {
      if (low <= high) {
        val middle = low + (high - low) / 2

        if (a(middle) == needle)
          middle
        else if (a(middle) < needle)
          binarySearch(middle + 1, high)
        else
          binarySearch(low, middle - 1)
      } else
        -(low + 1)
    }
    binarySearch(0, a.length - 1)
  }

  val Array(n) = readInts(1)
  val a = readInts(n)
  val Array(m) = readInts(1)
  val juicyWorms = readInts(m)

  val cumA = Array.ofDim[Long](n)

  var cumsum = 0
  for(i <- 0 until n) {
    cumsum += a(i)
    cumA(i) = cumsum
  }

  for(juicyWorm <- juicyWorms) {
    val v = binarySearch(cumA, juicyWorm)
    if(v>=0) {
      println(v + 1)
    } else {
      println(-v)
    }

  }
}
