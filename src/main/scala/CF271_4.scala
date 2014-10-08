import scala.annotation.tailrec

object    CF271_4 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(t,k) = readInts(2)
  for(testcase <- 0 until t) {
    val Array(a,b) = readInts(2)
  }
}
