/**
 * Created by lurker on 2014. 8. 15..
 */
object CF262_1 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(n,m) = readInts(2)

  var result = 0
  var curn = n

  while(curn > 0) {
    curn -= 1
    result += 1
    if(result % m == 0) {
      curn += 1
    }

  }
  println(result)

}
