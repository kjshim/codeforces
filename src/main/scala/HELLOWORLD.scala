object HELLOWORLD {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  def main(args: Array[String]): Unit = {
    val Array(n) = readInts(1)
    for (i <- 1 to n) {
      val name = readLine()
      println(s"Hello, $name")
    }
  }

}
