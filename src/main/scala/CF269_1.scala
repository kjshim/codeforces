/**
 * Created by lurker on 2014. 8. 15..
 */
object  CF269_1 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }




  val sticks = readInts(6)
  val stickGroups = sticks.groupBy(identity)
  val stickSizes = for(sq <- stickGroups) yield {
    sq._2.length
  }


  val sortedSizes = stickSizes.toList.sorted.toList
  val result = sortedSizes match {
    case List(2,4) => "Elephant"
    case List(1,1,4) => "Bear"
    case List(6) => "Elephant"
    case List(1,5) => "Bear"
    case _ => "Alien"
  }
  println(result)
}
