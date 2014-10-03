object  CF269_4 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  val Array(n, w) = readInts(2)
  val a = readInts(n)
  val b = readInts(w)

  val adiff = Array.ofDim[Long](a.length - 1)
  val bdiff = Array.ofDim[Long](b.length - 1)
  for(i <- 0 to adiff.length) {
    adiff(i) = a(i+1) - a(i)
  }
  for(i <- 0 to bdiff.length){
    bdiff(i) = b(i+1) - b(i)
  }
  println(adiff.toList)
  println(bdiff.toList)

  def kmp_table(w:Array[Int]):Array[Int] = {
    val t = Array.ofDim[Int](w.length)
    var pos = 2
    var cnd = 0

    t(0) = -1
    t(1) = 0

    while(pos < w.length) {
      if(w(pos-1) == w(cnd)) {
        cnd = cnd + 1
        t(pos) = cnd
        pos = pos + 1
      } else if (cnd > 0) {
        cnd = t(cnd)
      } else {
        t(pos) = 0
        pos = pos + 1
      }
    }
    t
  }
  val t = kmp_table(b)
  println(t.toList)
  val sl = a
  val wl = b

  var count = 0

  var m = 0
  var i = 0

  var sadder = wl(0) - sl(0)
  while(m + i < sl.length) {
    println(s"$m $i")
    if(wl(i) == sl(m+i) + sadder) {
      if(i == wl.length - 1) {
        count += 1

        if(t(i) > -1){
          m = m + i - t(i)
          i = t(i)
          sadder = wl(0) - sl(m)
        } else {
          i = 0
          m = m + 1
          sadder = wl(i) - sl(m + i)
        }


      } else {
        i = i + 1
      }
    } else {
      if(t(i) > -1){
        m = m + i - t(i)
        i = t(i)
        sadder = wl(0) - sl(m)
      } else {
        i = 0
        m = m + 1
        sadder = wl(i) - sl(m + i)
      }
    }
  }


  println(count)


}
