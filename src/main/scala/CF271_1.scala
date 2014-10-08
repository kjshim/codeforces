object    CF271_1 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }


  val keys = Array("qwertyuiop", "asdfghjkl;", "zxcvbnm,./")

  val lr = readLine()
  val inputstr = readLine()

  def R(v:Char):Char = {
    var result = ' '
    for(key<-keys){
      val idx = key.indexOf(v.toString)
      if (idx>=0) {
        result = key((idx - 1) max 0)
      }
    }
    result
  }
  def L(v:Char):Char = {
    var result = ' '
    for(key<-keys){
      val idx = key.indexOf(v.toString)
      if (idx>=0) {
        result = key((idx + 1) min (key.length-1))
      }
    }
    result
  }

  val sol = lr match {
    case "L" => inputstr.map(L).mkString("")
    case "R" => inputstr.map(R).mkString("")
  }
  println(sol)

}
