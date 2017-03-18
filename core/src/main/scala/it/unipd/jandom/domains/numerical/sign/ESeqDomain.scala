package it.unipd.jandom.domains.numerical.sign

import it.unich.jandom.domains.numerical.LinearForm
import it.unipd.jandom.domains.numerical.BaseNumericalDomain
<<<<<<< Updated upstream

=======
import it.unipd.jandom.domains.numerical.sign.Sign._
import it.unipd.jandom.domains.numerical.sign.ESeq._
>>>>>>> Stashed changes
/**
  * Extended sign domain, i.e. the domain composed of the elements Minus (negative numbers), Plus (positive numbers),
  * Zero (0), Geq0 (>=0), Leq0 (<=0) and Neq0 (!=0). SignTop and SignBottom complete the lattice, providing a greatest
  * and a least element for this set.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>,
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  */
class ESeqDomain extends BaseNumericalDomain[Sign, ESeqDomainCore.type](ESeqDomainCore) {


  override def createProperty(elements: Array[Sign], unreachable: Boolean): Property =
    new Property(elements, unreachable)

  class Property (sign : Array[Sign], unreachable : Boolean) extends BaseProperty(sign, unreachable) {
    private def getLowerSign(s : Sign): Array[Sign] ={
      val l = s match {
        case SignTop => List[Sign](Leq0, Neq0, Geq0, Plus, Minus, Zero)
        case Leq0 => List[Sign](Minus, Zero)
        case Geq0 => List[Sign](Plus, Zero)
        case Neq0 => List[Sign](Plus, Minus)
        case Plus => List[Sign]()
        case Minus => List[Sign]()
        case Zero => List[Sign]()
        case SignBottom => List[Sign]()
      }
      l.toArray
    }

    private def foo() : Array[Array[Sign]] = sign.map(getLowerSign)


    def filterLinearEvaluation(known: Int, homcoeffs: Array[Int], inequality : Boolean): Sign = {

      require(homcoeffs.length <= dimension)
      if (unreachable && homcoeffs.exists { _ != 0 })
        return ESeqDomainCore.top
      var acc: Sign = ESeqDomainCore.alpha(known)
      var coeffs : Array[(Int, Int)] = homcoeffs.zipWithIndex.filter(x => x._1 != 0)


      coeffs.count{ _ != 0} match {
        case 0 => acc
        case 1 =>
          val values: Array[Sign] = getLowerSign(sign(coeffs(0)._2))
          val a = Array.fill[Sign](values.length)(acc)
          for (i <- 0 to a.length-1) {
            if (coeffs(0)._1 < 0)
              a(i) = ESeqDomainCore.sum(a(i), ESeqDomainCore.inverse(values(i)))
            else
              a(i) = ESeqDomainCore.sum(a(i), values(i))
          }

          if(inequality) {
            val b : Array[((Sign, Sign), Int)] = a.zip(values).zipWithIndex

            val c = b.filter(x => x._1._1 match {
              case Leq0 => true
              case Zero => true
              case Minus => true
              case _ => false
            })
            println(c.deep.mkString)



          }
          else {
            val b : Array[((Sign, Sign), Int)] = a.zip(values).zipWithIndex

            val c=  b.filter(x => x._1._1 match {
              case Neq0 => true
              case Plus => true
              case Minus => true
              case _ => false
            })

            println(c.deep.mkString)



          }




          acc

        case _ =>
          linearEvaluation(known, homcoeffs)
      }



      }




    /** @inheritdoc
      */
    override def linearInequality(lf: LinearForm) : Property = {
      filterLinearEvaluation(lf.known.toDouble.toInt, lf.homcoeffs.map(_.toDouble.toInt).toArray, true)
      this
     /* val s : Sign = linearEvaluation(lf)
      if (isEmpty)
        return this
      s match {
        case Plus => bottom
        case SignTop => top
        case SignBottom => bottom
        case _ => this
      }*/
    }

    /**
      * @inheritdoc
      */
    override def linearDisequality(lf: LinearForm) : Property = {
      filterLinearEvaluation(lf.known.toDouble.toInt, lf.homcoeffs.map(_.toDouble.toInt).toArray,false)
      this

      /*if (isEmpty)
        return this
      val s : Sign = linearEvaluation(lf)
      s match {
        case Plus => this
        case Minus => this
        case Zero => bottom
        case SignTop => top //lub(Plus, Minus)
        case SignBottom => bottom
      }*/
    }
  } // end class Property
} // end class ESeqDomain

object ESeqDomain {
  def apply() = new ESeqDomain()
} // end of SignDomain's companion object
