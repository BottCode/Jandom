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

trait InfInt {
    def +(x: InfInt): InfInt
    def /(x: InfInt): InfInt
    def *(x: InfInt): InfInt
    def >(x: InfInt): Boolean
    // def <=(x: InfInt): InfInt TODO ??
    def max(x: InfInt): InfInt
    def min(x: InfInt): InfInt
    def ==(x: InfInt): Boolean 

    def inverse(): InfInt = {
        this match {
            case PositiveInfinity() => return NegativeInfinity()
            case NegativeInfinity() => return PositiveInfinity()
            case IntNumber(x) => return IntNumber(-x)
            case _ => return Undetermined()
        }
    }

    def -(x: InfInt): InfInt = {
        return this.+(inverse())
    }

    def >=(x: InfInt): Boolean = {
        return this.>(x.+(IntNumber(1)))
    }    

}

case class IntNumber(n: Int) extends InfInt {
    
    def +(x: InfInt): InfInt = {
        print("IntNumber+",n," + ",x)
        x match { 
            case NegativeInfinity() => NegativeInfinity()
            case PositiveInfinity() => PositiveInfinity()
            case Undetermined() => Undetermined()
            case IntNumber(m) => safeAdd(n,m)
        }
    }

    def *(x: InfInt): InfInt = {
        if(n == 0) return IntNumber(0)
        x match {
            case IntNumber(m) => safeMul(n,m)
            case Undetermined() => Undetermined()
            case _ => 
                if(n > 0) return x
                return x.inverse()
        }        
    }
    
    def >(x: InfInt): Boolean = {
        x match {
            case PositiveInfinity() => false
            case IntNumber(m) => n > m
            case _ => true // TODO
        }
    }

    def /(x: InfInt): InfInt = {
        x match {
            case IntNumber(m) => IntNumber(n / m)
            case Undetermined() => Undetermined()
            case _ => IntNumber(0)
        }
    }

    def ==(x: InfInt): Boolean = {
        x match {
            case IntNumber(x) => x == n
            case _ => false
        }
    }

    def max(x: InfInt): InfInt = {
        x match {
            case IntNumber(m) => IntNumber(n max m)
            case PositiveInfinity() => PositiveInfinity()
            case NegativeInfinity() => IntNumber(n)
            case _ => Undetermined()
        }
    }

    def min(x: InfInt): InfInt = {
        x match {
            case IntNumber(m) => IntNumber(n min m)
            case PositiveInfinity() => IntNumber(n)
            case NegativeInfinity() => NegativeInfinity()
            case _ => Undetermined()
        }
    }

    def safeAdd(left: Int, right: Int): InfInt = {
        if (right > 0 && left > Int.MaxValue - right)
            return PositiveInfinity()
        
        if (right < 0 && left < Int.MinValue - right)
            return NegativeInfinity()
        
        return IntNumber(left + right)
    }

    def safeMul(left: Int, right: Int): InfInt = { // TODO 
        if (right > 0) {
            if (left > Int.MaxValue / right)
                return PositiveInfinity()
            if (left < Int.MinValue / right)
                return NegativeInfinity()
        }
        if (right < -1) {
            if (left > Int.MinValue / right)
                return NegativeInfinity()
            if (left < Int.MaxValue / right)
                return PositiveInfinity()
        }
        if (right == -1) {
            if (left == Int.MinValue)
                return PositiveInfinity()
            if (left == Int.MaxValue)
                return NegativeInfinity()
        }

        return IntNumber(left * right)    
    }

}

case class NegativeInfinity() extends InfInt {
    def +(x: InfInt): InfInt = {
        print("NegativeInf+",x)
        x match {
            case PositiveInfinity() => Undetermined()
            case Undetermined() => Undetermined()
            case _ => NegativeInfinity()
        }
    }

    def *(x: InfInt): InfInt = {
        x match {
            case IntNumber(m) => 
                if (m > 0) 
                    return NegativeInfinity()
                if (m < 0)
                    return PositiveInfinity()
                return IntNumber(0)
            case Undetermined() => Undetermined()
            case PositiveInfinity() => NegativeInfinity()
            case NegativeInfinity() => PositiveInfinity()
        }        
    }

    def >(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => false
            case Undetermined() => false
            case _ => true 
        }
    }

    def /(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => 
                if(x < 0) // TODO THORW /0
                    return PositiveInfinity()
                return NegativeInfinity()
            case Undetermined() => Undetermined()
            case _ => IntNumber(0) // Inf / Inf = Inf * (1 / Inf) = Inf * 0
        }
    }    

    def ==(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => true
            case _ => false
        }
    }

    def max(x: InfInt): InfInt = {
        x match {
            case IntNumber(m) => IntNumber(m)
            case _ => x
        }
    }

    def min(x: InfInt): InfInt = {
        x match {
            case Undetermined() => Undetermined()
            case _ => NegativeInfinity()
        }
    }

}
  
case class PositiveInfinity() extends InfInt {

    def +(x: InfInt): InfInt = {
        print("NegativeInf+",x)
        x match {
            case NegativeInfinity() => Undetermined()
            case Undetermined() => Undetermined()
            case _ => PositiveInfinity()
        }
    }

    def *(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => 
                if (x > 0) 
                    return PositiveInfinity()
                if (x < 0)
                    return NegativeInfinity()
                return IntNumber(0)
            case _ => x
        }        
    }

    def >(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => true
            case Undetermined() => true // todo think
            case _ => false 
        }
    }

    def ==(x: InfInt): Boolean = {
        x match {
            case PositiveInfinity() => true
            case _ => false
        }
    }

    def /(x: InfInt): InfInt = {
        x match {
            case IntNumber(m) => 
                if(m < 0)
                    return NegativeInfinity()
                return PositiveInfinity() // TODO EXCEPTION /0
            case Undetermined() => Undetermined()
            case _ => IntNumber(0) // Inf / Inf = Inf * (1 / Inf) = Inf * 0
        }
    }

    def max(x: InfInt): InfInt = {
        x match {
            case Undetermined() => Undetermined()
            case _ => PositiveInfinity()
        }
    }

    def min(x: InfInt): InfInt = {
        x match {
            case Undetermined() => Undetermined()
            case _ => x
        }
    }

}

// case infinity - infinity
case class Undetermined() extends InfInt {
    def +(x: InfInt): InfInt = {
        return Undetermined()
    }

    def *(x: InfInt): InfInt = {
        return Undetermined()      
    }

    def >(x:InfInt): Boolean = {
        return false
    }

    def ==(x: InfInt): Boolean = {
        return false
    }

    def /(x: InfInt): InfInt = {
        return Undetermined()
    }

    def max(x: InfInt): InfInt = {
        return Undetermined();
    }

    def min(x: InfInt): InfInt = {
        return Undetermined()
    }

}
