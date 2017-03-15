package it.unipd.jandom.domains.numerical

import it.unich.jandom.domains.numerical._
<<<<<<< HEAD:core/src/main/scala/it/unipd/jandom/domains/numerical/SumSignParityDomain.scala
=======
import it.unipd.jandom.domains.numerical.ParityDomainCore._
import it.unipd.jandom.domains.numerical.sign.{SignDomain, SignDomainCore}
>>>>>>> 3940638cc1b1381a02a7252b484bd254b915945a:core/src/main/scala/it/unipd/jandom/domains/numerical/SumSignParity.scala


class SumSignParityDomain(val dom1: SignDomain, val dom2: ParityDomain) extends SumDomain[SignDomain, ParityDomain]{

  type Property = SumSignParity

  class SumSignParity(val p1: dom1.Property, val p2: dom2.Property) extends Sum {

  }

  def apply(p1: dom1.Property, p2: dom2.Property) = new SumSignParity(p1, p2)
}

object SumSignParityDomain {
  private lazy val v = new SumSignParityDomain(SignDomain(), ParityDomain())

  def apply() : SumSignParityDomain = v
}
