object  BY2015_1 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(n) = readInts(1)
  val arr = Array.ofDim[String](34)
  for(i <- 0 until n) {
    arr(i) = "O"
  }
  for(i <- n until 34){
    arr(i) = "#"
  }
  println("+------------------------+\n" +
    s"|${arr(0)}.${arr(4)}.${arr(7)}.${arr(10)}.${arr(13)}.${arr(16)}.${arr(19)}.${arr(22)}.${arr(25)}.${arr(28)}.${arr(31)}.|D|)\n" +
    s"|${arr(1)}.${arr(5)}.${arr(8)}.${arr(11)}.${arr(14)}.${arr(17)}.${arr(20)}.${arr(23)}.${arr(26)}.${arr(29)}.${arr(32)}.|.|\n" +
    s"|${arr(2)}.......................|\n" +
    s"|${arr(3)}.${arr(6)}.${arr(9)}.${arr(12)}.${arr(15)}.${arr(18)}.${arr(21)}.${arr(24)}.${arr(27)}.${arr(30)}.${arr(33)}.|.|)\n+------------------------+")
}
