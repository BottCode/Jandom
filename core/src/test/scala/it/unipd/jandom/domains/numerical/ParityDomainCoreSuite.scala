package it.unipd.jandom.domains.numerical

import it.unipd.jandom.domains.numerical.parity.{Even, Odd, ParityBottom, ParityTop}
import it.unipd.jandom.domains.numerical.parity.ParityDomainCore._
import org.scalatest.FlatSpec

/**
  * Unit Test - Parity Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  */
class ParityDomainCoreSuite extends FlatSpec {

  "ParityDomainCore.inverse" should
    " - return the inverse of the parity value given as input" in {
    assert(inverse(Even).equals(Even))
    assert(inverse(Odd).equals(Odd))
    assert(inverse(ParityTop).equals(ParityTop))
    assert(inverse(ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.sum" should
    " - return the sum of a couple of parity values given as input" in {
    assert(sum(Even, Even).equals(Even))
    assert(sum(Odd, Odd).equals(Odd))
    assert(sum(ParityTop, ParityTop).equals(ParityTop))
    assert(sum(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(sum(Odd, Even).equals(Odd))
    assert(sum(Even, Odd).equals(Odd))
    assert(sum(ParityTop, Even).equals(ParityTop))
    assert(sum(Even, ParityTop).equals(ParityTop))
    assert(sum(ParityTop, Odd).equals(ParityTop))
    assert(sum(Odd, ParityTop).equals(ParityTop))
    assert(sum(ParityBottom, Even).equals(ParityBottom))
    assert(sum(Even, ParityBottom).equals(ParityBottom))
    assert(sum(ParityBottom, Odd).equals(ParityBottom))
    assert(sum(Odd, ParityBottom).equals(ParityBottom))
    assert(sum(ParityBottom, ParityTop).equals(ParityBottom))
    assert(sum(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.mult" should
    " - return the multiplication of a couple of parity values given as input" in {
    assert(mult(Even, Even).equals(Even))
    assert(mult(Odd, Odd).equals(Odd))
    assert(mult(ParityTop, ParityTop).equals(ParityTop))
    assert(mult(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(mult(Odd, Even).equals(Even))
    assert(mult(Even, Odd).equals(Even))
    assert(mult(ParityTop, Even).equals(Even))
    assert(mult(Even, ParityTop).equals(Even))
    assert(mult(ParityTop, Odd).equals(ParityTop))
    assert(mult(Odd, ParityTop).equals(ParityTop))
    assert(mult(ParityBottom, Even).equals(ParityBottom))
    assert(mult(Even, ParityBottom).equals(ParityBottom))
    assert(mult(ParityBottom, Odd).equals(ParityBottom))
    assert(mult(Odd, ParityBottom).equals(ParityBottom))
    assert(mult(ParityBottom, ParityTop).equals(ParityBottom))
    assert(mult(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.division" should
    " - return the division of a couple of parity values given as input" in {
    assert(division(Even, Even).equals(ParityTop))
    assert(division(Odd, Odd).equals(ParityTop))
    assert(division(ParityTop, ParityTop).equals(ParityTop))
    assert(division(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(division(Odd, Even).equals(ParityTop))
    assert(division(Even, Odd).equals(ParityTop))
    assert(division(ParityTop, Even).equals(ParityTop))
    assert(division(Even, ParityTop).equals(ParityTop))
    assert(division(ParityTop, Odd).equals(ParityTop))
    assert(division(Odd, ParityTop).equals(ParityTop))
    assert(division(ParityBottom, Even).equals(ParityBottom))
    assert(division(Even, ParityBottom).equals(ParityBottom))
    assert(division(ParityBottom, Odd).equals(ParityBottom))
    assert(division(Odd, ParityBottom).equals(ParityBottom))
    assert(division(ParityBottom, ParityTop).equals(ParityBottom))
    assert(division(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.remainder" should
    " - return the remainder of a couple of parity values given as input" in {
    assert(remainder(Even, Even).equals(ParityTop))
    assert(remainder(Odd, Odd).equals(Even))
    assert(remainder(ParityTop, ParityTop).equals(ParityTop))
    assert(remainder(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(remainder(Odd, Even).equals(ParityTop))
    assert(remainder(Even, Odd).equals(ParityTop))
    assert(remainder(ParityTop, Even).equals(ParityTop))
    assert(remainder(Even, ParityTop).equals(ParityTop))
    assert(remainder(ParityTop, Odd).equals(ParityTop))
    assert(remainder(Odd, ParityTop).equals(ParityTop))
    assert(remainder(ParityBottom, Even).equals(ParityBottom))
    assert(remainder(Even, ParityBottom).equals(ParityBottom))
    assert(remainder(ParityBottom, Odd).equals(ParityBottom))
    assert(remainder(Odd, ParityBottom).equals(ParityBottom))
    assert(remainder(ParityBottom, ParityTop).equals(ParityBottom))
    assert(remainder(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.compare" should
    " - compare correctly a couple of parity values given as input" in {
    assert(compare(Even, Even).equals(Option(0)))
    assert(compare(Odd, Odd).equals(Option(0)))
    assert(compare(ParityTop, ParityTop).equals(Option(0)))
    assert(compare(ParityBottom, ParityBottom).equals(Option(0)))
    assert(compare(Odd, Even).equals(Option.empty))
    assert(compare(Even, Odd).equals(Option.empty))
    assert(compare(ParityTop, Even).equals(Option(1)))
    assert(compare(Even, ParityTop).equals(Option(-1)))
    assert(compare(ParityTop, Odd).equals(Option(1)))
    assert(compare(Odd, ParityTop).equals(Option(-1)))
    assert(compare(ParityBottom, Even).equals(Option(-1)))
    assert(compare(Even, ParityBottom).equals(Option(1)))
    assert(compare(ParityBottom, Odd).equals(Option(-1)))
    assert(compare(Odd, ParityBottom).equals(Option(1)))
    assert(compare(ParityBottom, ParityTop).equals(Option(-1)))
    assert(compare(ParityTop, ParityBottom).equals(Option(1)))
  }

  "ParityDomainCore.lub" should
    " - return the lub of a couple of parity values given as input" in {
    assert(lub(Even, Even).equals(Even))
    assert(lub(Odd, Odd).equals(Odd))
    assert(lub(ParityTop, ParityTop).equals(ParityTop))
    assert(lub(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(lub(Odd, Even).equals(ParityTop))
    assert(lub(Even, Odd).equals(ParityTop))
    assert(lub(ParityTop, Even).equals(ParityTop))
    assert(lub(Even, ParityTop).equals(ParityTop))
    assert(lub(ParityTop, Odd).equals(ParityTop))
    assert(lub(Odd, ParityTop).equals(ParityTop))
    assert(lub(ParityBottom, Even).equals(Even))
    assert(lub(Even, ParityBottom).equals(Even))
    assert(lub(ParityBottom, Odd).equals(Odd))
    assert(lub(Odd, ParityBottom).equals(Odd))
    assert(lub(ParityBottom, ParityTop).equals(ParityTop))
    assert(lub(ParityTop, ParityBottom).equals(ParityTop))
  }

  "ParityDomainCore.glb" should
    " - return the glb of a couple of parity values given as input" in {
    assert(glb(Even, Even).equals(Even))
    assert(glb(Odd, Odd).equals(Odd))
    assert(glb(ParityTop, ParityTop).equals(ParityTop))
    assert(glb(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(glb(Odd, Even).equals(ParityBottom))
    assert(glb(Even, Odd).equals(ParityBottom))
    assert(glb(ParityTop, Even).equals(Even))
    assert(glb(Even, ParityTop).equals(Even))
    assert(glb(ParityTop, Odd).equals(Odd))
    assert(glb(Odd, ParityTop).equals(Odd))
    assert(glb(ParityBottom, Even).equals(ParityBottom))
    assert(glb(Even, ParityBottom).equals(ParityBottom))
    assert(glb(ParityBottom, Odd).equals(ParityBottom))
    assert(glb(Odd, ParityBottom).equals(ParityBottom))
    assert(glb(ParityBottom, ParityTop).equals(ParityBottom))
    assert(glb(ParityTop, ParityBottom).equals(ParityBottom))
  }

}
