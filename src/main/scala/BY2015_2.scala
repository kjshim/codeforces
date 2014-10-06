object    BY2015_2 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }

  val Array(n, m) = readInts(2)
  val lr = readLine()
  val ud = readLine()

  var can = true
  val topleft = lr(0).toString + ud(0).toString
  if(topleft == ">v" || topleft == "<^") {
    can = false
  }
  val topright = lr(0).toString + ud(ud.length - 1).toString
  if(topright == ">^" || topright == "<v") {
    can = false
  }
  val botleft = lr(lr.length - 1).toString + ud(0).toString
  if(botleft == ">^" || botleft  == "<v"){
    can = false
  }
  val botright = lr(lr.length -1).toString + ud(ud.length - 1).toString
  if(botright == ">v" || botright == "<^") {
    can = false
  }
  if(can){
    println("YES")
  }else{
    println("NO")
  }
}
