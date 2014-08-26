import scala.collection.mutable

/**
 * Created by lurker on 2014. 8. 15..
 */
object CF263_3 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  val Array(n) = readInts(1)
  val arr  = readInts(n)

  val sortedArr = arr.sortBy(identity)
  var result = 0L
  var i = 0
  var multiplier = if(n==1) 1L else 2L
  while(i<n){
    result += (sortedArr(i).toLong * multiplier)
    multiplier = (multiplier + 1) min n
    i+=1
  }
  println(result)
}
