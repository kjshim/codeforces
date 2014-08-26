/**
 * Created by lurker on 2014. 8. 15..
 */
object CF262_2 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(a, b, c) = readInts(3)
  val result = collection.mutable.ArrayBuffer[Long]()

  var cur_sx = 1
  while(cur_sx <= 90) {
    val x:Long = b.toLong * Math.pow(cur_sx, a).toLong + c.toLong
    var s = 0L
    var curi = x
    while (curi > 0) {
      s += (curi % 10L)
      curi /= 10
    }
    if(s == cur_sx && x > 0L && x < 10e9 ){
      result.append(x)
    }
    cur_sx += 1
  }
  println(result.length)
  println(result.mkString(" "))

}
