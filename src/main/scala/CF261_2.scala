/**
 * Created by lurker on 2014. 8. 15..
 */
object CF261_2 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)

  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toLong)
  }

  val Array(n) = readInts(1)
  val b = readInts(n.toInt)

  val maxb = b.max
  val minb = b.min

  val nmax:Long = b.filter(_ == maxb).length
  val nmin:Long = b.filter(_ == minb).length

  val r:Long = maxb - minb
  val result = nmax * nmin

  val rcases:Long = if(maxb != minb){
    nmax * nmin
  } else {
    nmax * (nmax - 1) / 2L
  }
  println(s"$r $rcases")
}
