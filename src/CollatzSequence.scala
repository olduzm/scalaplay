object CollatzSequence {

  case class CycleResult(initialVal:Long, cycles:Int)
  object CycleResult { def Empty = Nil }

  def main(args: Array[String]) {
    MeasureTime("Max Collatz Cycle Calculation (1 to 1,000,000)") {
      println("Max cycle found " + maxCycle(1, 1000000))
    }
  }

  def maxCycle(lower:Long, upper:Long) : List[CycleResult] =
  maxCycle(lower, upper, CycleResult.Empty)

  private def maxCycle(lower:Long, upper:Long, cycles:List[CycleResult]) : List[CycleResult] =
  compare(_3np1(lower), cycles) match {
    case calculated if (lower == upper) => calculated
    case calculated => maxCycle(lower+1, upper, calculated)
  }

  def _3np1(initial:Long) : CycleResult = _3np1(initial, initial, 0)

  private def _3np1(initial:Long, n:Long, cycles:Int) : CycleResult = {
    n match {
      case _ if (n == 1) => CycleResult(initial, cycles + 1)
      case _ if (n % 2 == 1) => _3np1(initial, 3 * n + 1, cycles + 1)
      case _ => _3np1(initial, n / 2, cycles + 1)
    }
  }

  private def compare(v1:CycleResult, v2:List[CycleResult]) : List[CycleResult] =
    v2 match {
      case Nil => List(v1)
      case x::xs if (v1.cycles == x.cycles) => v1::v2
      case x::xs if (v1.cycles < x.cycles) => v2
      case _ => List(v1)
    }

  object MeasureTime {
    def apply[T](name: String)(block: => T) {
      val start = System.currentTimeMillis
      try {
        block
      } finally {
        val diff = System.currentTimeMillis - start
        println("# \"" + name +"\" completed, time taken: " + diff + " ms (" + diff / 1000.0 + " s)")
      }
    }
  }
}