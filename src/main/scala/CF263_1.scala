/**
 * Created by lurker on 2014. 8. 15..
 */
object CF263_1 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  val Array(n) = readInts(1)
  val arr = Array.fill(n)(readLine)

  def isEven(i:Int, j:Int):Boolean = {
    var sumo = 0
    for( (di, dj) <- Seq((-1,0), (1,0), (0, -1), (0, 1))) {
      val newi = i + di
      val newj = j + dj
      if(0 <= newi && newi < n && 0 <= newj && newj < n) {
        if(arr(newi)(newj) == 'o') {
          sumo += 1
        }
      }
    }
    sumo %2 == 0
  }

  var result = true
  for(i <- 0 until n; j <- 0 until n) {
    result = result && isEven(i, j)
  }
  if(result){
    println("YES")
  } else {
    println("NO")
  }
}
