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
      val d = delta()
      if (d < 0.0) Set()
      else if (d > 0.0) {
        val nd = math.sqrt(d)

        Set((-nb + nd) / (2 * na), (-nb - nd) / (2 * na))
      } else {
        Set((-nb) / (2 * na))

      }
    }
  }
}
  
