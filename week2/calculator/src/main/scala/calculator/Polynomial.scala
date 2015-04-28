package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    Signal {
      val na = a()
      val nb = b()
      val nc = c()
      (nb * nb) - 4 * na * nc
    }

  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val na = a()
      val nb = b()
      val nc = c()
      val nd = math.sqrt(delta())

      Set((-nb + nd) / (2 * na), (-nb - nd) / (2 * na))
    }
  }
}
  
