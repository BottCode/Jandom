/**
 * Copyright 2018 Mattia Bottaro, Mauro Carlin
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You shosuld have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.unipd.jandom.domains

/*
final abstract class InfInt(val self: Any) private extends AnyVal {

    def +(val x: NegativeInfinity()): InfInt = self match {
        case NegativeInfinity()=> return InfInt(x)
        case PositiveInfinity() => return InfInt(-1) // dobbiamo ritornare una forma indeterminata
        case Int => return NegativeInfinity()
    }

    def +(valx: PositiveInfinity()): InfInt

}*/

trait InfInt{
    def +(x: InfInt): InfInt
    def >(x: InfInt): Boolean
    def -(x: InfInt): InfInt
    def /(x: InfInt): InfInt
    def >=(x: InfInt): Boolean
    def <=(x: InfInt): InfInt
    def max(x: InfInt): InfInt
    def min(x: InfInt): InfInt
}

case class IntNumber(n: Int) extends InfInt {
    
    def +(x: InfInt): InfInt = {
        x match { 
            case NegativeInfinity() => return NegativeInfinity()
            case PositiveInfinity() => return PositiveInfinity()
            case Undetermined() => return Undetermined()
            case IntNumber(x) => return safeAdd(n,x)
        }
    }

    def >(x: InfInt): Boolean = {
        x match {
            case PositiveInfinity() => return false
            case IntNumber(x) => return n > x
            case _ => true 
        }
    }

    def safeAdd(left: Int, right: Int): InfInt = {
        // require(left should be anIstanceOf[Int])
        if (right > 0 && left > Int.MaxValue - right)
            return PositiveInfinity()
        if (right < 0 && left < Int.MinValue - right)
            return NegativeInfinity()
        return IntNumber(left + right)
    }

}

case class NegativeInfinity() extends InfInt {
    def +(x: InfInt): InfInt = {
        x match {
            case PositiveInfinity() => return Undetermined()
            case Undetermined() => return Undetermined()
            case _ => NegativeInfinity()
        }
    }

   def >(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => return false
            case Undetermined() => return false
            case _ => true 
        }
    }    

}
  
case class PositiveInfinity() extends InfInt {
    def +(x: InfInt): InfInt = {
        x match {
            case NegativeInfinity()=> return Undetermined()
            case Undetermined() => return Undetermined()
            case _ => return PositiveInfinity()
        }
    }

   def >(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => return true
            case Undetermined() => return true
            case _ => false 
        }
    }    

}

// case infinity - infinity
case class Undetermined() extends InfInt {
    def +(x: InfInt): InfInt = {
        Undetermined()
    }

    def >(x:InfInt): Boolean = {
        x match {
           case _ => false
        }
    }

}
