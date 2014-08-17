/**
 * Created by lurker on 2014. 8. 15..
 */
object CF261_1 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)

  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(x1, y1, x2, y2) = readInts(4)

  val minx = x1 min x2
  val maxx = x1 max x2
  val miny = y1 min y2
  val maxy = y1 max y2


  if (minx == maxx) {
    val dist = maxy - miny
    val Array(x3, y3, x4, y4) = Array(minx + dist, miny, minx + dist, maxy)
    println(s"${x3} ${y3} ${x4} ${y4}")
  } else if (miny == maxy) {
    val dist = maxx - minx
    val Array(x3, y3, x4, y4) = Array(minx, miny + dist, maxx, miny + dist)
    println(s"${x3} ${y3} ${x4} ${y4}")
  } else {
    if (maxx - minx != maxy - miny) {
      println("-1")
    } else {
      val Array(x3, y3, x4, y4) = Array(x1, y2, x2, y1)
      println(s"${x3} ${y3} ${x4} ${y4}")
    }
  }
}
