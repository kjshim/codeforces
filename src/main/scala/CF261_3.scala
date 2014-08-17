import java.math.BigInteger

/**
 * Created by lurker on 2014. 8. 15..
 */
object CF261_3 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)

  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(n, k, d) = readInts(3)

  def nToBaseKWithDLen(n:Int):Array[Int] = {
    val res = Array.fill(d)(0)
    var curn = n
    var i = 0
    while(i < d) {
      res(i) = curn % k + 1
      curn = curn / k
      i = i + 1
    }
    res
  }

  val kbig = new BigInteger(k.toString)
  val dbig = new BigInteger(d.toString)
  val nbig = new BigInteger(n.toString)

  if(kbig.pow(d.toInt).compareTo(nbig) < 0){
    println(-1)
  } else {
    val solutions = (1 to n).map(nToBaseKWithDLen(_))
    for(day <- (1 to d)){
      println(solutions.map(_(day - 1)).mkString(" "))
    }
  }
}
