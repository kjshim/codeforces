import scala.collection.mutable

/**
 * Created by lurker on 2014. 8. 15..
 */
object CF263_2 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  val Array(n, k) = readInts(2)
  val inputStr = readLine()

  val map = for((k, v) <- inputStr.groupBy(identity)) yield {
    (k, v.length)
  }
  val l = map.toArray.sortBy(-_._2)
  var remaink = k
  var i = 0
  var result = 0
  while(remaink > 0) {
    val cantake = remaink min l(i)._2
    result += (cantake * cantake)
    remaink -= cantake
    i += 1
  }
  println(result)

}
