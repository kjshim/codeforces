import java.io.PrintWriter
import java.util.Scanner

/**
 * Created by arseny on 05.10.14.
 */
object C extends App {
  val in = new Scanner(System.in)
  val out = new PrintWriter(System.out)

  def nextInt = in.nextInt
  def nextLong = in.nextLong
  def nextDouble = in.nextDouble
  def nextString = in.next

  def solve() = {
    val n = nextInt
    val m = nextInt
    val gs = Array.fill(n, m)(true)
    for (i <- 0 until n) {
      gs(i) = nextString.toCharArray.map(_ == 'X')
    }

    def tryPaint(gs: Array[Array[Boolean]], n: Int, m: Int): Int = {
      var lux = 0
      var luy = 0
      var found  = false

      // lowest x,y
      for (i <- 0 until n) {
        for (j <- 0 until m) {
          if (!found && gs(i)(j)) {
            found = true
            lux = i
            luy = j
          }
        }
      }
      //out.println(lux, luy)

      // possible maximum y size from luy
      var maxY = 1
      var foundMaxY = false
      for (i <- luy + 1 until m) {
        if (!foundMaxY && gs(lux)(i) == false) {
          foundMaxY = true
          maxY = i - luy
        }
      }
      if (!foundMaxY) {
        maxY = m - luy
      }
      //out.println(maxY)

      var x = 1

      // possible maximum x size from lux
      var foundX = false
      for (i <- lux + 1 until n) {
        if (!foundX && gs(i)(luy) == false) {
          foundX = true
          x = i - lux
        }
      }
      if (!foundX) {
        x = n - lux
      }

      var y = 1
      if (lux + x < n) {
        var foundY = false
        for (j <- luy + 1 until luy + maxY) {
          if(!foundY && gs(lux + x)(j) == true) {
            foundY = true
            y = luy + maxY - j
          }
        }
        if (!foundY) {
          y = 1
        }
      }

      out.println(x, y)

      val ps = Array.fill(n, m)(false)

      for (i <- lux until lux + x) {
        for (j <- luy until luy + y) {
          ps(i)(j) = true
        }
      }

      def paintRight(x: Int, y: Int, h: Int, w: Int): Boolean = {
        if (y + w == m) {
          false
        } else {
          for (i <- x until x + h) {
            ps(i)(y + w) = true
          }
          true
        }
      }

      def paintDown(x: Int, y: Int, h: Int, w: Int): Boolean = {
        if (x + h == n) {
          false
        } else {
          for (i <- y until y + w) {
            ps(x + h)(i) = true
          }
          true
        }
      }

      var curx = lux
      var cury = luy

      var painted = false
      while (!painted) {
        if (cury + y < m && gs(curx)(cury + y) == true) {
          paintRight(curx, cury, x, y)
          cury += 1
        } else if (curx + x < n && gs(curx + x)(cury) == true) {
          paintDown(curx, cury, x, y)
          curx += 1
        } else {
          painted = true
        }
      }

      //      for (i <- 0 until n) {
      //        for (j <- 0 until m) {
      //          out.print(ps(i)(j))
      //          out.print(" ")
      //        }
      //        out.print("\n")
      //      }

      var canPaint = true
      for (i <- 0 until n) {
        for (j <- 0 until m) {
          if (ps(i)(j) != gs(i)(j)) {
            canPaint = false
          }
        }
      }

      if (canPaint) {
        x * y
      } else {
        -1
      }
    }

    val gst = Array.fill(m, n)(true)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        gst(i)(j) = gs(j)(i)
      }
    }

    val ans1 = tryPaint(gs, n, m)
    val ans2 = tryPaint(gst, m, n)

    if (ans1 == -1 && ans2 == -1) {
      out.print(-1)
    } else {
      if (ans1 == -1) {
        out.print(ans2)
      }
      if (ans2 == -1) {
        out.print(ans1)
      }
      if (ans1 != -1 && ans2 != -1) {
        out.print(math.min(ans1, ans2))
      }
    }
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