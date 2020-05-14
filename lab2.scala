import java.math.BigInteger

class FibNum(fib: List[Int]) {
  val list = fib

  def helper_BigInt2(n: BigInteger): BigInteger = {
    n match {
      case i if i.equals(BigInteger.ONE) => i
      case i if i.equals(BigInteger.ZERO) => i
      case i => helper_BigInt2(n.subtract(BigInteger.ONE)).add(helper_BigInt2(n.subtract(BigInteger.ONE).subtract(BigInteger.ONE)))
    }
  }

  def helper_BigInt1(f: List[Int], res: BigInteger): BigInteger = {
    if (f.nonEmpty) {
      if (f.head == 0)
        helper_BigInt1(f.tail, res)
      else
        helper_BigInt1(f.tail, res.add(helper_BigInt2(BigInteger.valueOf(f.size + 1))))
    }
    else
      res
  }

  def toInteger(): BigInteger = {
    helper_BigInt1(this.list, BigInteger.ZERO)
  }

  //////////////////////////////////

  def helper_sum2(f: List[Int], res: List[Int]): List[Int] = {
    if (f.nonEmpty) {
      if (f.length > 2) {
        if (f.head == 1) {
          if (f.tail.head == 1)
            helper_sum2(1 :: f.tail.tail.tail, res ::: List(0) ::: List(0))
          else
            helper_sum2(f.tail, res ::: List(f.head))
        } else
          helper_sum2(f.tail, res ::: List(f.head))
      } else {
        if (f.length == 2) {
          if (f.head == 1) {
            if (f.tail.head == 1)
              helper_sum2(f.tail.tail, res ::: List(0, 0, 1))
            else helper_sum2(f.tail.tail, res ::: f)
          } else helper_sum2(f.tail.tail, res ::: f)
        } else {
          if (f.length == 1)
            helper_sum2(f.tail, res ::: f)
          else
            helper_sum2(f.tail.tail, res ::: f)
        }
      }
    }
    else res.reverse
  }


  def helper_sum1(f: List[Int]): List[Int] = {
    if (f.length == 1) {
      if (f.head == 1)
        helper_sum2(List(0, 1), List())
      else helper_sum2(1 :: f.tail, List())
    } else {
      if (f.head == 0) {
        helper_sum2(1 :: f.tail, List())
      }
      else
        helper_sum2(0 :: 1 :: f.tail.tail, List())
    }
  }

  def unary_!(): List[Int] = {
    helper_sum1(this.list.reverse)
  }

  /////////////////////////////////////

  def fib_recur(n: Int): Int = {
    n match {
      case i if i < 2 => i
      case i => fib_recur(n - 1) + fib_recur(n - 2)
    }
  }

  def helper3Max(l1: List[Int], l2: List[Int], n: Int): Int = {
    if (l2.nonEmpty) {
      if (l1.contains(l2.head) && (l2.head > n))
        l2.head
      else
        helper3Max(l1, l2.tail, l2.head)
    }
    else
      -1
  }

  def helper2Max(f: List[Int], res: List[Int]): List[Int] = {
    if (f.nonEmpty) {
      if (f.head == 0)
        helper2Max(f.tail, res)
      else
        helper2Max(f.tail, res ::: List(fib_recur(f.size + 1)))
    }
    else
      res
  }

  def helperMax(f1: List[Int], f2: List[Int]): Int = {
    helper3Max(helper2Max(f1, List()), helper2Max(f2, List()), -1)
  }

  def %(obj: FibNum): Int = {
    helperMax(this.list, obj.list)
  }
}

//--------------------------------------------------------------------------
object Main {
  def main(args: Array[String]): Unit = {
    val test54 = new FibNum(List(1, 0, 1, 0, 1, 0, 1, 0))
    val test7 = new FibNum(List(1, 0, 1, 0))
    val test6 = new FibNum(List(1, 0, 0, 1))
    val test46 = new FibNum(List(1, 0, 0, 1, 0, 1, 0, 1))

    println("toInteger:")
    println(test54.toInteger)
    println(test7.toInteger)
    println()

    println("unar:")
    print("!")
    println(test54.toInteger())
    println(!test54)
    //10101010
    //10101011
    //10101100
    //10110000
    //11000000
    //100000000
    print("!")
    println(test6.toInteger())
    println(!test6)
    //1010
    print("!")
    println(test46.toInteger())
    println(!test46)
    //10010101
    //10010110
    //10011000
    //10100000
    print("!")
    println(test7.toInteger())
    println(!test7)
    //1010
    //1011
    //1100
    //10000
    println()

    val test0 = new FibNum(List(0))
    println(!test0)

    println()

    val test20 = new FibNum(List(1, 0, 1, 0, 1, 0))
    val test15 = new FibNum(List(1, 0, 0, 0, 1, 0))
    println(test20.toInteger + " % " + test15.toInteger())
    println(test20 % test15)

    val testN1 = new FibNum(List(1, 0, 1, 0, 1, 0))
    val testN2 = new FibNum(List(1, 0, 1, 0, 0))
    println(testN1 % testN2)
  }
}