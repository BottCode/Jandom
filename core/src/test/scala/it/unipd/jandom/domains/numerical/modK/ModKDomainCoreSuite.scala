package it.unipd.jandom.domains.numerical.modK

import it.unipd.jandom.domains.numerical.mod.ModK.{ModKBottom, ModKTop, RestClass}
import it.unipd.jandom.domains.numerical.mod.ModKDomainCore
import org.scalatest.FunSuite
import spire.std.option

/**
  * Created by mirko on 3/22/17.
  */
class ModKDomainCoreSuite extends FunSuite {


  test("Alpha test") {
    ModKDomainCore(2) //CREATE A MODK object with k = 2
    assert(ModKDomainCore.alpha(2) == RestClass(0))
    assert(ModKDomainCore.alpha(1) == RestClass(1))
    assert(ModKDomainCore.alpha(0) == RestClass(0))
    ModKDomainCore(10) //CREATE A MODK Object with k = 10
    assert(ModKDomainCore.alpha(3) == RestClass(3))
    assert(ModKDomainCore.alpha(100) == RestClass(0))
  }

  test("Inverse test"){
    for(k <- 1 to 100) {
      ModKDomainCore(k)
      for (i <- 1 to k)
        assert(ModKDomainCore.inverse(RestClass(i)) == RestClass(k - i))
    }
  }

  test("Sum test") {
    for (k <- 1 to 100) {
      ModKDomainCore(k)
      assert(ModKDomainCore.sum(ModKDomainCore.alpha(2), ModKDomainCore.alpha(3)) == ModKDomainCore.alpha(5))
      assert(ModKDomainCore.sum(ModKDomainCore.alpha(-2), ModKDomainCore.alpha(3)) == ModKDomainCore.alpha(1))
    }
  }

  test("Multiplication test"){
    for (k <- 1 to 100) {
      ModKDomainCore(k)
      assert(ModKDomainCore.mult(ModKDomainCore.alpha(2), ModKDomainCore.alpha(3)) == ModKDomainCore.alpha(6))
      assert(ModKDomainCore.mult(ModKTop, ModKDomainCore.alpha(k)) == RestClass(0))
    }
  }

  test("Lub test") {
    for (k <- 1 to 100) {
      ModKDomainCore(k)
      for(i <- 0 until k-1) {
        assert(ModKDomainCore.lub(RestClass(i), RestClass(i)) == RestClass(i))
        assert(ModKDomainCore.lub(ModKTop, RestClass(i)) == ModKTop)
        assert(ModKDomainCore.lub(RestClass(i), ModKTop) == ModKTop)
        assert(ModKDomainCore.lub(ModKBottom, RestClass(i)) == RestClass(i))
        assert(ModKDomainCore.lub(RestClass(i), ModKBottom) == RestClass(i))
        assert(ModKDomainCore.lub(RestClass(i), RestClass((i + 1)%k)) == ModKTop)
      }
    }
  }
  test("GLB test") {
    for (k <- 1 until 100) {
      ModKDomainCore(k)
      for(i <- 0 until k-1) {
        assert(ModKDomainCore.glb(RestClass(i), RestClass(i)) == RestClass(i))
        assert(ModKDomainCore.glb(ModKTop, RestClass(i)) == RestClass(i))
        assert(ModKDomainCore.glb(RestClass(i), ModKTop) == RestClass(i))
        assert(ModKDomainCore.glb(ModKBottom, RestClass(i)) == ModKBottom)
        assert(ModKDomainCore.glb(RestClass(i), ModKBottom) == ModKBottom)
        assert(ModKDomainCore.glb(RestClass(i), RestClass((i + 1)%k)) == ModKBottom)
      }
    }
  }

  test("Compare Test") {
    for (k <- 1 until 100) {
      ModKDomainCore(k)
      for(i <- 0 until k-1) {
        assert(ModKDomainCore.compare(RestClass(i), RestClass(i)).contains(0))
        assert(ModKDomainCore.compare(ModKTop, RestClass(i)).contains(1))
        assert(ModKDomainCore.compare(RestClass(i), ModKTop).contains(-1))
        assert(ModKDomainCore.compare(ModKBottom, RestClass(i)).contains(-1))
        assert(ModKDomainCore.compare(RestClass(i), ModKBottom).contains(1))
        assert(ModKDomainCore.compare(RestClass(i), RestClass((i + 1)%k)).isEmpty)
      }
    }
  }
  test("Reamainder Test") {
    for(k <- 1 until 100) {
      ModKDomainCore(k)
      for(i <- 1 until k-1){
        assert(ModKDomainCore.remainder(RestClass(i), ModKTop) == ModKTop)
        assert(ModKDomainCore.remainder(RestClass(i), RestClass(i)) == RestClass(0))
      }
    }
  }

}
