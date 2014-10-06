object BY2015_6 extends App {
  case class Point(x:Long, y:Long)
  type Universe = Seq[Point]

  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  def readLongs(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toLong)
  }

  def splitUniverse(points:Universe):Option[(Universe,Universe)] = {
    if(points.size == 0 || points.size == 1) {
      None
    } else {
      // try x
      val xSorted = points.toList.sortBy(_.x)
      var lastX = xSorted(0).x
      for(p <- xSorted.drop(1)) {
        if(p.x > lastX + 1) {
          // split at lastX + 1
          val pivot = lastX + 1
          val left = xSorted.filter(_.x < pivot)
          val right = xSorted.filter(_.x > pivot)
          return Some(left, right)
        }
        lastX = p.x
      }

      val ySorted = points.toList.sortBy(_.y)
      var lastY = ySorted(0).y
      for(p <- ySorted.drop(1)) {
        if(p.y > lastY + 1){
          val pivot = lastY + 1
          val left = xSorted.filter(_.y < pivot)
          val right = xSorted.filter(_.y > pivot)
          return Some(left, right)
        }
        lastY = p.y
      }
      None
    }
  }

  val waitingUniverses = collection.mutable.Queue[Universe]()
  val Array(n) = readInts(1)

  val points = for(i <- 1 to n) yield {
    val Array(x, y) = readLongs(2)
    Point(x, y)
  }
  val initialPoints = points.toList

  waitingUniverses.enqueue(initialPoints)
  var nUniverse = 1

  while(waitingUniverses.nonEmpty) {
    val univ = waitingUniverses.dequeue()
    val spl = splitUniverse(univ)
    spl match {
      case Some(tup) => {
        nUniverse += 1
        waitingUniverses.enqueue(tup._1)
        waitingUniverses.enqueue(tup._2)
      }
      case None => {

      }
    }
  }
  println(nUniverse)
}
