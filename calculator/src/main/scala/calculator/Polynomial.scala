package calculator


object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b()*b() - 4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val value = delta()
      if(value == 0) Set((-b() + math.sqrt(value))/2*a())
      else if(value > 0) Set((-b() - math.sqrt(value))/2*a(),(-b() + math.sqrt(value))/2*a())
      else Set()
    }
  }
}
