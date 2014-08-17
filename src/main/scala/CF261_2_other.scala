import java.io.PrintWriter
import java.util.Scanner

/**
 * Created by arseny on 15.08.14.
 */
object B extends App {

  val in = new Scanner(System.in)
  val out = new PrintWriter(System.out)

  def nextInt = in.nextInt
  def nextLong = in.nextLong
  def nextDouble = in.nextDouble
  def nextString = in.next

  def solve() = {
    val n = nextInt
    val bs = Array.fill(n)(0L)
    (0 until n).foreach(i => bs(i) = nextLong)
    val sorted = bs.sorted
    val minBeauty = sorted(0)
    val maxBeauty = sorted(n - 1)
    var maxDiff = maxBeauty - minBeauty
    var minCount = 0L
    var maxCount = 0L
    while (minCount < n && sorted(minCount.toInt) == minBeauty) {
      minCount += 1L
    }
    while (maxCount < n && sorted(n - 1 - maxCount.toInt) == maxBeauty) {
      maxCount += 1L
    }
    val maxDiffCount = if(minBeauty == maxBeauty) n.toLong * (n.toLong - 1) / 2L else maxCount * minCount
    out.print(maxDiff + " " + maxDiffCount)
  }

  try {
    solve()
  } catch {
    case _: Exception => sys.exit(9999)
  } finally {
    out.flush()
    out.close()
  }
}