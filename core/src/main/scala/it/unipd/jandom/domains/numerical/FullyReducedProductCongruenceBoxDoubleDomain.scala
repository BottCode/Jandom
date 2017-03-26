package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.ProductDomain
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unipd.jandom.domains.numerical.congruence.Congruence.{Congruence, CongruenceBottom}
import it.unipd.jandom.domains.numerical.congruence.{Congruence, CongruenceDomain, CongruenceDomainCore}
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

    override type Domain = FullyReducedProductCongruenceBoxDoubleDomain.this.type

    override def domain : Domain = FullyReducedProductCongruenceBoxDoubleDomain.this

    override def reduce(x1: dom1.Property, x2: dom2.Property): FullyReducedProductCongruenceBoxDouble = {
      //Checking if the properties are empty, or contain a bottom element
      if(x1.isEmpty || x2.isEmpty || x1.elements.contains(CongruenceBottom) || (x2.low, x2.high).zipped.exists(_ > _))
        new FullyReducedProductCongruenceBoxDouble(x1.bottom, x2.bottom)
      else {
        /* Calculate the point-wise fully-reduct product of an array of cogruence and low- and upperbound of box double */
        val res : Array[(Congruence, Double, Double)] = (x1.elements, x2.low, x2.high).zipped.map(
          (congruence,_low, _high) => {
            var low: Int = _low.toInt

            var high: Int = _high.toInt
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
      * Function that takes as input an interval [_low, _high] and an interval aZ + b and returns
      * the fully reduced product between low, high and aZ + b
      */
    private def transform(_low : Int, _high: Int, a : Option[Int], b : Int) : (Int, Int) = {
      var (low, high) = (_low, _high)

      if (!MathLibrary.isCongruent(low, b, a) || !MathLibrary.isCongruent(high, b, a)) {
        /* restrict the interval until it matches with the congruence */
        while (!MathLibrary.isCongruent(low, b, a))
          low = low + 1
        while (!MathLibrary.isCongruent(high, b, a))
          high = high - 1
        (low, high)
      }
      else /* preserve the interval */
        (_low, _high)
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
