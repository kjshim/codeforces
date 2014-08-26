import scala.collection.mutable

/**
 * Created by lurker on 2014. 8. 15..
 */
object CF263_4 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  val Array(n) = readInts(1)
  val connections = readInts(n-1)
  val rb = readInts(n)
  val blackOut = Array.fill(n)(0)
  val neighbors = Array.fill(n)(mutable.ListBuffer[Int]())
  var curi = 1
  while(curi < n) {
    neighbors(curi).append(connections(curi-1))
    neighbors(connections(curi-1)).append(curi)
    curi += 1
  }
  val blackN = neighbors.map(l => l.filter(v => rb(v) == 1).length)
  var result = 0L
  val visited = collection.mutable.Set[Int]()
  var cnt = 0

  var i = 0

  def visit(v:Int, cumv:List[Int]):List[Int] = {
    visited.add(v)
    if(rb(v) == 1) {
      cumv
    } else {

    }

  }
  while(i < n){
    if(!visited.contains(i)){

    }
    i += 1
  }
}
