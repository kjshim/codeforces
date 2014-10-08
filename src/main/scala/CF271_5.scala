import java.lang.Comparable
import java.util
import scala.annotation.tailrec

class Entry(val maxLength:Int = 0, val first:Int = -1) extends Comparable[Entry] {
  override def compareTo(o: Entry): Int = {
    maxLength - o.maxLength
  }
}

class BinaryIndexedTree(n:Int) {
  val data = Array.fill[Entry](n){
    new Entry()
  }
  def update(atOrg:Int, by:Entry) = {
    var at = atOrg
    while(at < data.length) {
      if(data(at).compareTo(by) < 0)
        data(at) = by
      at |= (at+1)
    }
  }

  def get(atOrg:Int):Entry = {
    var best = new Entry()
    var at = atOrg
    while(at>=0){
      if(best.compareTo(data(at)) < 0)
        best = data(at)
      at -= ~at & (at+1)
    }
    best
  }
}


object    CF271_5 extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt)
  }
  def readLongs(n: Int) = {
    val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toLong)
  }
  val Array(n, minDiff) = readInts(2)
  val heights = readLongs(n)

  val keyHeight:Array[Long] = heights.toSet.toArray.sorted.toArray
  val smallTree = new BinaryIndexedTree(keyHeight.size)
  val bigTree = new BinaryIndexedTree(keyHeight.size)
  val maxLength = Array.ofDim[Int](n)
  val next = Array.ofDim[Int](n)
  for(i <- (n-1) to 0 by -1 ) {
    // find h(i) - maxDiff lower && h(i) + maxDiff higher (log N)
    // first
    {
      var low = 0
      var high = keyHeight.length - 1
      while (low < high) {
        val mid = (low + high) / 2
        if (keyHeight(mid) >= (heights(i) + minDiff)) {
          high = mid
        }
        else
          low = mid + 1
      }
      if (keyHeight(low) >= heights(i) + minDiff) {
        val other = bigTree.get(keyHeight.length - low - 1)
        if (maxLength(i) < other.maxLength + 1) {
          maxLength(i) = other.maxLength + 1
          next(i) = other.first
        }
      }
    }
    {
      var low = 0
      var high = keyHeight.length - 1
      while (low < high) {
        val mid = (low + high + 1) / 2
        if (keyHeight(mid) >= (heights(i) - minDiff)) {
          low = mid
        }
        else
          high = mid - 1
      }
      if (keyHeight(low) <= heights(i) - minDiff) {
        val other = smallTree.get(low)
        if (maxLength(i) < other.maxLength + 1) {
          maxLength(i) = other.maxLength + 1
          next(i) = other.first
        }
      }
    }
    val entry = new Entry(maxLength(i), i)
    val pos = util.Arrays.binarySearch(keyHeight, heights(i))

    smallTree.update(pos, entry)
    bigTree.update(keyHeight.size - pos - 1, entry)


  }

  var start = 0
  for(i <- 0 until n) {
    if(maxLength(i) > maxLength(start)){
      start = i
    }
  }

  println(maxLength(start))

  while(start != -1){
    print(start + 1)
    print(" ")
    start = next(start)
  }
}
