class SegmentTree(val a:Array[Int]) {
  val fenwickTree = Array.ofDim[Long](a.length)

  for( i <- 0 to a.length) {
    var idx = i
    do {
      fenwickTree(idx) += a(i)
      idx += (idx & -idx)
    } while(idx < a.length && idx > 0)
  }

//  def read(idx:Int) = {
//    var curIdx = idx
//    var sum = 0
//    while(idx > 0) {
//      sum += fenwickTree(curIdx)
//      var x = curIdx & -curIdx
//      curIdx = curIdx - x
//    }
//  }

  def udpate(idx:Int, value:Long) = {

  }
}