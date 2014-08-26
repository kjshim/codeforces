/**
 * Created by lurker on 2014. 8. 15..
 */
object CF262_3 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(n, m, w) = readInts(3)
  val arr = readInts(n)

  // binary search of the solution space

  // we can check if it's possible or not in O(n)
  // solution space is 10^9+10^5

  def checkIfCorrect(v:Long) = {

  }
}
