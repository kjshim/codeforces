import java.math.BigInteger
import java.util
import scala.util

/**
 * Created by lurker on 2014. 8. 15..
 */
object CF261_5 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }


}
