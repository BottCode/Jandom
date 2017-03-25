package it.unipd.jandom.domains.numerical.mod


/**
  * Domain with values with the form `k * Z + a`.
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>
  * @author Stefano Munari <stefano.munari@studenti.unipd.it>
  * @author Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  */
object ModK {
	private var k = 1

	trait ModK
	/** Represents the elements in the equivalence class num modulo k */
	case class RestClass(num : Int) extends ModK {
    override def toString: String = k match {
      case 2 => num match {
        case 0 => "Even"
        case 1 => "Odd"
      }
      case _ => num + " mod " + k
    }
	}
	case object ModKBottom extends ModK {
    override def toString: String = "\u22A5"
  }
	case object ModKTop extends ModK {
    override def toString: String = "\u22A4"
  }

	/**
	  * Singleton object constructor. Sets k to num
	  *
	  * @param num the value of the divisor k
	  */
	def apply(num : Int) {
		k = num
	}

	/** 
	  * Getter function for the divisor k
	  * @return the divisor k
	  */
	def divisor: Int = k
}