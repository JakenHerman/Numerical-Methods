object NewtonsMethod {

  def main(args: Array[String]) {
    run
  }
  
  def run {
    // the f(x) and f'(x) functions
    val fx = (x: Double) => (scala.math.pow(x, 7) 
                            - 28 * scala.math.pow(x, 6)
                            + 322 * scala.math.pow(x, 5) 
                            - 1960 * scala.math.pow(x, 4)
                            + 6769 * scala.math.pow(x, 3) 
                            - 13132 * scala.math.pow(x, 2)
                            + 13068 * x - 5040)

    val fxPrime = (x: Double) => (7 * scala.math.pow(x, 6) 
                                 - 168 * scala.math.pow(x, 5)
                                 + 1610 * scala.math.pow(x, 4)
                                 - 7840 * scala.math.pow(x, 3)
                                 + 20307 * scala.math.pow(x, 2)
                                 - 26264 * x
                                 + 13068)

    val tolerance = 0.0000001

    for(i <- 0 to 7){
      val guess = 0.9+i
      // pass f(x) and f'(x) to the Newton's Method function, along with the initial guess
      // and tolerance 
      val answer = newtonsMethod(fx, fxPrime, guess, tolerance)
      println("The answer for guess " + guess + " is : " + answer)
    }

  }

  def newtonsMethod(fx: Double => Double, 
                    fxPrime: Double => Double,
                    x: Double,
                    tolerance: Double): Double = {
    var x1 = x
    var xNext = nmh(fx, fxPrime, x1)
    while (Math.abs(xNext - x1) > tolerance) {
      x1 = xNext
      println(xNext)
      xNext = nmh(fx, fxPrime, x1)
    }
    
    return xNext
  }

  /**
   * This is the "x2 = x1 - f(x1)/f'(x1)" calculation 
   */
  def nmh(fx: Double => Double, 
                          fxPrime: Double => Double,
                          x: Double): Double = {
    return x - fx(x) / fxPrime(x)
  }

}