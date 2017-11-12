package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unipd.jandom.domains.numerical.congruence.Congruence.{Congruence, CongruenceBottom}
import it.unipd.jandom.domains.numerical.congruence.{Congruence, CongruenceDomain}
import it.unipd.jandom.domains.numerical.utils.MathLibrary

/**
  * Instantiation of the reduced product domain using the Congruence domain and the BoxDouble domain.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari.1@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @param dom1 the first abstract domain
  * @param dom2 the second abstract domain
  */
class FullyReducedProductCongruenceBoxDoubleDomain(override val dom1 : CongruenceDomain, override val dom2 : BoxDoubleDomain) extends ProductDomain[CongruenceDomain, BoxDoubleDomain](dom1, dom2) {
  override val dom1Todom2 = DomainTransformation.CongruenceToBoxDouble
  override val dom2Todom1 = DomainTransformation.BoxDoubleToCongruence

  override def top(n: Int) =
    new FullyReducedProductCongruenceBoxDouble(dom1.top(n), dom2.top(n))

  override def bottom(n: Int) =
    new FullyReducedProductCongruenceBoxDouble(dom1.bottom(n), dom2.bottom(n))


  /**
    * @inheritdoc
    */
  class FullyReducedProductCongruenceBoxDouble(override val p1 : dom1.Property, override val p2 : dom2.Property) extends Product(p1, p2) {

    /**
    * @inheritdoc
    */
    override def reduce(x1: dom1.Property, x2: dom2.Property): FullyReducedProductCongruenceBoxDouble = {
      //Checking if the properties are empty, or contain a bottom element
      if(x1.isEmpty || x2.isEmpty || x1.elements.contains(CongruenceBottom) || (x2.low, x2.high).zipped.exists(_ > _))
        new FullyReducedProductCongruenceBoxDouble(x1.bottom, x2.bottom)
      else {
        /* Calculate the point-wise fully-reduct product of an array of cogruence and low- and upperbound of box double */
        val res : Array[(Congruence, Double, Double)] = (x1.elements, x2.low, x2.high).zipped.map(
          (congruence,_low, _high) => {
            val low: Int = _low.toInt
            val high: Int = _high.toInt
            congruence match {
              case Congruence.Mod(a, b) =>
                val (a1, b1) = transform(low, high, a, b)
                if (a1 > b1)
                  (CongruenceBottom, Double.PositiveInfinity, Double.NegativeInfinity)
                else if (a1 == b1)
                  (Congruence.Mod(None, a1), a1.toDouble, a1.toDouble)
                else
                  (congruence, a1.toDouble, b1.toDouble)
              case _ => (CongruenceBottom, Double.PositiveInfinity, Double.NegativeInfinity)
            }
          }
        )
        val congruenceElements = res.map(_._1)
        val congruenceIsEmpty = congruenceElements.contains(CongruenceBottom)
        val boxdoublelowelements : Array[Double] = res.map(_._2)
        val boxdoublehighelements : Array[Double] = res.map(_._3)

        new FullyReducedProductCongruenceBoxDouble(
          dom1.createProperty(congruenceElements, congruenceIsEmpty),
          dom2(boxdoublelowelements, boxdoublehighelements)
        )
      }
    }

    /**
      * This function that takes as input an interval [_low, _high] and 
      * a congruence aZ + b. It returns the fully reduced product between
      * [low, high] and aZ + b. Thus, reducing [_low, _high] using aZ + b
      * @param _low the lower bound of the interval
      * @param _high the upper bound of the interval
      * @param a the module part of the congruence aZ+b
      * @param b the constant part of the congruence aZ+b
      * @return (low, high) the (potentially) restricted interval
      */
    private def transform(_low : Int, _high: Int, a : Option[Int], b : Int) : (Int, Int) = {
      var (low, high) = (_low, _high)

      if (!MathLibrary.isCongruent(low, b, a) || 
          !MathLibrary.isCongruent(high, b, a)) 
      {
          if(!MathLibrary.isCongruent(low, b, a))
            low = compute_low(low,a,b)
          if(!MathLibrary.isCongruent(high, b, a))
            high = compute_high(high,a,b)
      }
      (low, high)
    }
  }

/**
  * Computes the (potentially) new upper bound of the interval.
  * If the congruence is a constant then the interval has to match the constant value.
  * Otherwise compute the gap between old_high and the lowest value
  * of the congruence closer to old_high. Then substract it to old_high. 
  * The gap is computed as following: 
  * 1. compute the (| normalized_rest - b | % a) 
  *    which is the rest which makes old_high not congruent to b mod a.
  *    Note that this represents the gap between old_high and the nearest
  *    higher value of the congruence, but we are searching the lowest one.
  * 2. subtract highest_value_gap to a, thus obtaining the gap between the 
  *    lowest value of the congruence closer to old_high and old_high
  * 3. substract lowest_value_gap to old_high to obtain new high
  * @param high the upper bound of the interval
  * @param a0 the module part of the congruenze a0Z+b1
  * @param b1 the constant part of the congruence a0Z+b1
  * @return the new (possibily restriced) upper bound of the interval
  */
  private def compute_high (high : Int, a0 : Option[Int], b1 : Int) : Int = {
    a0 match {
      case None     => b1
      case Some(a1) => high - (a1 - (Math.abs((((high % a1)+ a1)% a1) - b1)% a1))
    }
  }

/**
  * Computes the (potentially) new lower bound of the interval.
  * If the congruence is a constant then the interval has to match the constant value.
  * Otherwise compute the gap between old_low and the highest value
  * of the congruence closer to old_low. Then sum it to old_low.
  * The logic is similar to compute_high
  * @see compute_high
  * @param low the lower bound of the interval 
  * @param a0 the module part of the congruenze a0Z+b1
  * @param b1 the constant part of the congruence a0Z+b1
  * @return the new (possibily restriced) lower bound of the interval
  */
  private def compute_low (low : Int, a0 : Option[Int], b1 : Int) : Int = {
    a0 match {
      case None     => b1
      case Some(a1) => low + (Math.abs((((low % a1)+ a1)% a1) - b1)% a1)
    }
  }

  /**
    * Class constructor
    */
  def apply(p1 : dom1.Property, p2 : dom2.Property) = new FullyReducedProductCongruenceBoxDouble(p1, p2)

}  // end FullyReducedProductCongruenceBoxDoubleDomain class

object FullyReducedProductCongruenceBoxDoubleDomain {
  /**
    * Factory method
    */
  def apply() : FullyReducedProductCongruenceBoxDoubleDomain = new FullyReducedProductCongruenceBoxDoubleDomain(CongruenceDomain(), BoxDoubleDomain())
} // end FullyReducedProductCongruenceBoxDoubleDomain companion object
