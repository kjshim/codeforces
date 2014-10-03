import scala.collection.mutable

/**
 * Created by lurker on 2014. 8. 15..
 * http://codeforces.com/problemset/problem/467/C
 */
object CF467C extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(n,m,k) = readInts(3)
  val p = readInts(n)
  //val cumsum_p = Array.ofDim[Long]

  val d = Array.ofDim[Long](n, k)

}
