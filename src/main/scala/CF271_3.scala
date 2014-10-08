import scala.annotation.tailrec


object    CF271_3 extends App {
  case class Mole(x:Long, y:Long, a:Long, b:Long)

  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  def readLongs(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toLong)
  }

  def rotate(x:Long, y:Long, a:Long, b:Long):(Long, Long) = {
    ((a + b - y), (x - a +b))
  }

  def rotateMole(mole:Mole, n:Int):(Long,Long) = {
    var vp = (mole.x, mole.y)
    for(i <- 0 until n) {
      vp = rotate(vp._1, vp._2, mole.a, mole.b)
    }
    vp
  }

  def distance (p1: (Long, Long), p2: (Long, Long)) = {
    val (p1x, p1y) = p1
    val (p2x, p2y) = p2
    val dx = p1x - p2x
    val dy = p1y - p2y
    Math.sqrt(dx*dx + dy*dy)
  }

  def checkSqr(points:Array[(Long, Long)]):Boolean = {
    val Array(x,y,w,z) = points
    val yd = distance(x, y)
    val wd = distance(x, w)
    val zd = distance(x, z)
    if(yd == 0.0 || wd == 0.0 || zd == 0.0) {
      return false
    }
    val (s1, s2, s3) = if(yd == zd) {
      (y,z,w)
    } else if (wd == zd) {
      (w, z, y)
    } else if (yd == wd) {
      (y, w, z)
    } else {
      return false
    }
    if(distance(s2, s3) != distance(s1, s3)) {
      return false
    }

    distance(x, s2) == distance(s2, s3) && distance(x, s3) == distance(s1, s2)
    //distance(x, s3) == distance(s1, s2) &&
  }

  val Array(n) = readInts(1)
  for(testcase <- 0 until n) {
    val moles = (1 to 4).toArray.map(_ => {
      val Array(x, y, a, b) = readLongs(4)
      val mole = Mole(x, y, a, b)
      mole
    })

    var sol = -1
    for(r1 <- 0 to 3; r2 <- 0 to 3; r3 <- 0 to 3; r4 <- 0 to 3) {
      val sum = r1 + r2 + r3 + r4
      if((sol >= 0 && sum < sol) || sol == -1){
        val points = Array(
          rotateMole(moles(0), r1),
          rotateMole(moles(1), r2),
          rotateMole(moles(2), r3),
          rotateMole(moles(3), r4)
        )
        if(checkSqr(points)){
          //println(r1,r2,r3,r4)
          sol = sum
        }
      }
    }
    println(sol)
  }



}
