package it.unipd.jandom.domains.numerical.parity

import it.unipd.jandom.domains.numerical.parity.Parity._
import org.scalatest.FlatSpec

/**
  * Unit Test - Parity Domain Core
  *
  * @author Mirko Bez <mirko.bez@studenti.unipd.it>, Sebastiano Valle <sebastiano.valle@studenti.unipd.it>
  *           Stefano Munari <stefano.munari.1@studenti.unipd.it>
  */
class ParityDomainCoreSuite extends FlatSpec {
  val core = ParityDomainCore()
  
  "ParityDomainCore.core.inverse" should
    " - return the core.inverse of the parity value given as input" in {
    assert(core.inverse(Even).equals(Even))
    assert(core.inverse(Odd).equals(Odd))
    assert(core.inverse(ParityTop).equals(ParityTop))
    assert(core.inverse(ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.core.sum" should
    " - return the core.sum of a couple of parity values given as input" in {
    assert(core.sum(Even, Even).equals(Even))
    assert(core.sum(Odd, Odd).equals(Odd))
    assert(core.sum(ParityTop, ParityTop).equals(ParityTop))
    assert(core.sum(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(core.sum(Odd, Even).equals(Odd))
    assert(core.sum(Even, Odd).equals(Odd))
    assert(core.sum(ParityTop, Even).equals(ParityTop))
    assert(core.sum(Even, ParityTop).equals(ParityTop))
    assert(core.sum(ParityTop, Odd).equals(ParityTop))
    assert(core.sum(Odd, ParityTop).equals(ParityTop))
    assert(core.sum(ParityBottom, Even).equals(ParityBottom))
    assert(core.sum(Even, ParityBottom).equals(ParityBottom))
    assert(core.sum(ParityBottom, Odd).equals(ParityBottom))
    assert(core.sum(Odd, ParityBottom).equals(ParityBottom))
    assert(core.sum(ParityBottom, ParityTop).equals(ParityBottom))
    assert(core.sum(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.core.mult" should
    " - return the core.multiplication of a couple of parity values given as input" in {
    assert(core.mult(Even, Even).equals(Even))
    assert(core.mult(Odd, Odd).equals(Odd))
    assert(core.mult(ParityTop, ParityTop).equals(ParityTop))
    assert(core.mult(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(core.mult(Odd, Even).equals(Even))
    assert(core.mult(Even, Odd).equals(Even))
    assert(core.mult(ParityTop, Even).equals(Even))
    assert(core.mult(Even, ParityTop).equals(Even))
    assert(core.mult(ParityTop, Odd).equals(ParityTop))
    assert(core.mult(Odd, ParityTop).equals(ParityTop))
    assert(core.mult(ParityBottom, Even).equals(ParityBottom))
    assert(core.mult(Even, ParityBottom).equals(ParityBottom))
    assert(core.mult(ParityBottom, Odd).equals(ParityBottom))
    assert(core.mult(Odd, ParityBottom).equals(ParityBottom))
    assert(core.mult(ParityBottom, ParityTop).equals(ParityBottom))
    assert(core.mult(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.core.division" should
    " - return the core.division of a couple of parity values given as input" in {
    assert(core.division(Even, Even).equals(ParityTop))
    assert(core.division(Odd, Odd).equals(ParityTop))
    assert(core.division(ParityTop, ParityTop).equals(ParityTop))
    assert(core.division(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(core.division(Odd, Even).equals(ParityTop))
    assert(core.division(Even, Odd).equals(ParityTop))
    assert(core.division(ParityTop, Even).equals(ParityTop))
    assert(core.division(Even, ParityTop).equals(ParityTop))
    assert(core.division(ParityTop, Odd).equals(ParityTop))
    assert(core.division(Odd, ParityTop).equals(ParityTop))
    assert(core.division(ParityBottom, Even).equals(ParityBottom))
    assert(core.division(Even, ParityBottom).equals(ParityBottom))
    assert(core.division(ParityBottom, Odd).equals(ParityBottom))
    assert(core.division(Odd, ParityBottom).equals(ParityBottom))
    assert(core.division(ParityBottom, ParityTop).equals(ParityBottom))
    assert(core.division(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.core.core.remainder" should
    " - return the core.remainder of a couple of parity values given as input" in {
    assert(core.remainder(Even, Even).equals(ParityTop))
    assert(core.remainder(Odd, Odd).equals(Even))
    assert(core.remainder(ParityTop, ParityTop).equals(ParityTop))
    assert(core.remainder(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(core.remainder(Odd, Even).equals(Odd))
    assert(core.remainder(Even, Odd).equals(ParityTop))
    assert(core.remainder(ParityTop, Even).equals(ParityTop))
    assert(core.remainder(Even, ParityTop).equals(ParityTop))
    assert(core.remainder(ParityTop, Odd).equals(ParityTop))
    assert(core.remainder(Odd, ParityTop).equals(ParityTop))
    assert(core.remainder(ParityBottom, Even).equals(ParityBottom))
    assert(core.remainder(Even, ParityBottom).equals(ParityBottom))
    assert(core.remainder(ParityBottom, Odd).equals(ParityBottom))
    assert(core.remainder(Odd, ParityBottom).equals(ParityBottom))
    assert(core.remainder(ParityBottom, ParityTop).equals(ParityBottom))
    assert(core.remainder(ParityTop, ParityBottom).equals(ParityBottom))
  }

  "ParityDomainCore.core.compare" should
    " - core.compare correctly a couple of parity values given as input" in {
    assert(core.compare(Even, Even).equals(Option(0)))
    assert(core.compare(Odd, Odd).equals(Option(0)))
    assert(core.compare(ParityTop, ParityTop).equals(Option(0)))
    assert(core.compare(ParityBottom, ParityBottom).equals(Option(0)))
    assert(core.compare(Odd, Even).equals(Option.empty))
    assert(core.compare(Even, Odd).equals(Option.empty))
    assert(core.compare(ParityTop, Even).equals(Option(1)))
    assert(core.compare(Even, ParityTop).equals(Option(-1)))
    assert(core.compare(ParityTop, Odd).equals(Option(1)))
    assert(core.compare(Odd, ParityTop).equals(Option(-1)))
    assert(core.compare(ParityBottom, Even).equals(Option(-1)))
    assert(core.compare(Even, ParityBottom).equals(Option(1)))
    assert(core.compare(ParityBottom, Odd).equals(Option(-1)))
    assert(core.compare(Odd, ParityBottom).equals(Option(1)))
    assert(core.compare(ParityBottom, ParityTop).equals(Option(-1)))
    assert(core.compare(ParityTop, ParityBottom).equals(Option(1)))
  }

  "ParityDomainCore.core.lub" should
    " - return the core.lub of a couple of parity values given as input" in {
    assert(core.lub(Even, Even).equals(Even))
    assert(core.lub(Odd, Odd).equals(Odd))
    assert(core.lub(ParityTop, ParityTop).equals(ParityTop))
    assert(core.lub(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(core.lub(Odd, Even).equals(ParityTop))
    assert(core.lub(Even, Odd).equals(ParityTop))
    assert(core.lub(ParityTop, Even).equals(ParityTop))
    assert(core.lub(Even, ParityTop).equals(ParityTop))
    assert(core.lub(ParityTop, Odd).equals(ParityTop))
    assert(core.lub(Odd, ParityTop).equals(ParityTop))
    assert(core.lub(ParityBottom, Even).equals(Even))
    assert(core.lub(Even, ParityBottom).equals(Even))
    assert(core.lub(ParityBottom, Odd).equals(Odd))
    assert(core.lub(Odd, ParityBottom).equals(Odd))
    assert(core.lub(ParityBottom, ParityTop).equals(ParityTop))
    assert(core.lub(ParityTop, ParityBottom).equals(ParityTop))
  }

  "ParityDomainCore.core.glb" should
    " - return the core.glb of a couple of parity values given as input" in {
    assert(core.glb(Even, Even).equals(Even))
    assert(core.glb(Odd, Odd).equals(Odd))
    assert(core.glb(ParityTop, ParityTop).equals(ParityTop))
    assert(core.glb(ParityBottom, ParityBottom).equals(ParityBottom))
    assert(core.glb(Odd, Even).equals(ParityBottom))
    assert(core.glb(Even, Odd).equals(ParityBottom))
    assert(core.glb(ParityTop, Even).equals(Even))
    assert(core.glb(Even, ParityTop).equals(Even))
    assert(core.glb(ParityTop, Odd).equals(Odd))
    assert(core.glb(Odd, ParityTop).equals(Odd))
    assert(core.glb(ParityBottom, Even).equals(ParityBottom))
    assert(core.glb(Even, ParityBottom).equals(ParityBottom))
    assert(core.glb(ParityBottom, Odd).equals(ParityBottom))
    assert(core.glb(Odd, ParityBottom).equals(ParityBottom))
    assert(core.glb(ParityBottom, ParityTop).equals(ParityBottom))
    assert(core.glb(ParityTop, ParityBottom).equals(ParityBottom))
  }

}
