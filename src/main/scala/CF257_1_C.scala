import scala.annotation.tailrec

/**
 * Created by lurker on 2014. 8. 11..
 */
object CF257_1_C extends App {
  @inline def tokenizeLine = new java.util.StringTokenizer(readLine)
  def readInts(n: Int) = { val tl = tokenizeLine; Array.fill(n)(tl.nextToken.toInt) }

  val Array(n) = readInts(1)
  val myPrimes = sieve(n)
  val results = collection.mutable.ArrayBuffer[(Int,Int)]()

  // 1. filter out primes which > n/2
  val numbers = (2 to n).filter(v => !((v > n / 2) && myPrimes(v)))

  val primesUnderHalf = numbers.filter(v => (v <= n / 2) && myPrimes(v))


  def sieve(n:Int):collection.mutable.BitSet = {
    val numbers = 2 to n
    val sieve = collection.mutable.BitSet(numbers: _*)
    for( p <- numbers.takeWhile(x => x*x <= n) if sieve(p)){
      sieve --= (p*p to n by p)
    }
    sieve
  }

  @tailrec def GCD(a:Int, b:Int):Int = {
    if(b==0){
      a
    }else{
      GCD(b, a%b)
    }
  }

  println(GCD(108, 28))

}
