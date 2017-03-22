package it.unipd.jandom.domains.numerical.modK

import it.unipd.jandom.domains.numerical.mod.ModK.RestClass
import it.unipd.jandom.domains.numerical.mod.ModKDomainCore
import org.scalatest.FunSuite

/**
  * Created by mirko on 3/22/17.
  */
class ModKDomainCoreSuite extends FunSuite {
  val parity = ModKDomainCore(2)

  test("alpha test") {
    assert(ModKDomainCore.alpha(2) == RestClass(0))
    assert(ModKDomainCore.alpha(1) == RestClass(1))
    assert(ModKDomainCore.alpha(0) == RestClass(0))

  }

}
