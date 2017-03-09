package it.unipd.jandom.domains.numerical

import org.scalatest.FunSuite

/**
  * Created by mirko on 3/9/17.
  */
class CongruenceDomainCoreTestSuite extends FunSuite {

  import CongruenceDomainCore._


  test("getString") {
    assert(getString(Mod(1,1)) == "1Z + 1")
  }

  test("To Congruence") {
    assert(toCongruence(12) == Mod(0, 12))
  }

  test("Sum") {
    assert(sum(Mod(0,0), Mod(1,0)) == Mod(1,0))
  }

  test("Lub") {
    assert(lub(Mod(6,0), Mod(6, 3)) == Mod(3, 0))
    assert(lub(Mod(2,0), Mod(2, 1)) == Mod(1, 0))
  }

  test("Standard form") {
    assert(standardForm(Mod(1,12)) == Mod(1,0))
  }


}
