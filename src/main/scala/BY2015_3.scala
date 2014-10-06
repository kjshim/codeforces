object    BY2015_3 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  val Array(n,m) = readInts(2)
  val mat = Array.ofDim[Boolean](n, m)
  var xcount = 0
  var maxx = -1
  var maxy = -1

  for(i <- 0 until n) {
    val line = readLine()
    for(ch <- line.zipWithIndex) {
      mat(i)(ch._2) = ch._1 match {
        case 'X' => {
          xcount += 1
          true
        }
        case _ => false
      }
    }
  }

  var minx = -1
  var miny = -1
  for(i <- 0 until n; j <- 0 until m){
    if(minx == -1 && mat(i)(j) == true) {
      minx = i
      miny = j
    }
    if(mat(i)(j)){
      maxx = i
      maxy = j
    }
  }

  def verify(x:Int, y:Int):Boolean = {
    // check first brush
    var checked = 0
    for(i <- minx until minx + x; j <- miny until miny + y){
      if(!mat(i)(j)) {
        return false
      } else {
        checked += 1
      }
    }
    var curx = minx
    var cury = miny

    while(curx + x <= maxx || cury + y <= maxy) {
      // go x
      if( curx + x <= maxx && mat(curx + x)(cury)) {
        // all x in x
        var isOk = true
        for(k <- cury until cury + y){
          isOk = isOk && mat(curx + x)(k)
          checked += 1
        }
        if(!isOk) {
          return false
        }
        curx += 1
      }
        // go y
      else {
        // all x in x
        var isOk = true
        for(k <- curx until curx + x){
          isOk = isOk && mat(k)(cury+y)
          checked += 1
        }
        if(!isOk) {
          return false
        }
        cury += 1
      }
    }

    if(checked != xcount){
      return false
    }
    true
  }
  def process():Long = {
    for (x <- 0 until (n - 1); y <- 0 until (m - 1)) {
      if (mat(x)(y) && (!mat(x)(y + 1)) && (mat(x + 1)(y)) && mat(x + 1)(y + 1)) {
        var curx = x + 1
        var cury = y
        while (curx < n && mat(curx)(cury)) {
          curx += 1
        }
        curx -= 1
        while (cury >= 0 && mat(curx)(cury)) {
          cury -= 1
        }
        curx += 1
        val xsize = curx - x
        val ysize = y - cury
        if(verify(xsize ,ysize)){
          return xsize * ysize
        }
      }

      if (mat(x)(y) && (mat(x)(y + 1)) && (!mat(x + 1)(y)) && mat(x + 1)(y + 1)) {
        var curx = x
        var cury = y + 1
        while (curx >= 0 && mat(curx)(cury)) {
          curx -= 1
        }
        curx += 1
        while (cury < n && mat(curx)(cury)) {
          cury += 1
        }
        curx -= 1
        val xsize = x - curx
        val ysize = cury - y
        if(verify(xsize ,ysize)){
          return xsize*ysize
        }
      }
    }
    return (maxx - minx + 1) min (maxy - miny + 1)
  }
  println(process())
}
