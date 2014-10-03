object  CF269_3 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  // 1000000000000

  val n = readLong()
  var count = 0
  for(i <- 1L to 1000000L) {
    val v:Long = 2L * i + i * ( i -1L) / 2L * 3L
    if((n - v) % 3 == 0 && (n >= v)){
      count += 1
    }
  }
  println(count)
}
