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

    def inverse(x: InfInt): InfInt = {
        x match {
            case PositiveInfinity() => return NegativeInfinity()
            case NegativeInfinity() => return PositiveInfinity()
            case IntNumber(x) => return IntNumber(-x)
            case _ => return Undetermined()
        }
    }

    def -(x: InfInt): InfInt = {
        return this.+(inverse(x))
    }

    def >=(x: InfInt): Boolean = {
        return this.>(x.+(IntNumber(1)))
    }    

}

case class IntNumber(n: Int) extends InfInt {
    
    def +(x: InfInt): InfInt = {
        print("IntNumber+",n," + ",x)
        x match { 
            case NegativeInfinity() => return NegativeInfinity()
            case PositiveInfinity() => return PositiveInfinity()
            case Undetermined() => return Undetermined()
            case IntNumber(x) => return safeAdd(n,x)
        }
    }

    def *(x: InfInt): InfInt = {
        if(n == 0) return IntNumber(0)
        x match {
            case IntNumber(x) => return safeMul(n,x)
            case Undetermined() => return Undetermined()
            case _ => 
                if(n > 0) return x
                return inverse(x)
        }        
    }
    
    def >(x: InfInt): Boolean = {
        x match {
            case PositiveInfinity() => return false
            case IntNumber(x) => return n > x
            case _ => return true 
        }
    }

    def /(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => 
                if (x != 0)
                    return IntNumber(n / x)
                if (n > 0) // && x == 0 
                    return PositiveInfinity()
                else if (n < 0) 
                    return NegativeInfinity()
                return IntNumber(0)
            case Undetermined() => return Undetermined()
            case _ => IntNumber(0)
        }
    }

    def ==(x: InfInt): Boolean = {
        x match {
            case IntNumber(x) => return x == n
            case _ => return false
        }
    }

    def max(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => return IntNumber(n max x)
            case PositiveInfinity() => return PositiveInfinity()
            case NegativeInfinity() => return IntNumber(n)
            case _ => return Undetermined()
        }
    }

    def min(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => return IntNumber(n min x)
            case PositiveInfinity() => return IntNumber(n)
            case NegativeInfinity() => return NegativeInfinity()
            case _ => return Undetermined()
        }
    }

    def safeAdd(left: Int, right: Int): InfInt = {
        // require(left should be anIstanceOf[Int])
        print("safeadd",left,right)
        if (right > 0 && left > Int.MaxValue - right){
            print("Ritorno il positiveInf")
            return PositiveInfinity()
        }
        if (right < 0 && left < Int.MinValue - right){
            print("ritnorno il neginf")
            return NegativeInfinity()
        }
        print("ritorno: ",left+right)
        return IntNumber(left + right)
    }

    def safeMul(left: Int, right: Int): InfInt = {
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
            case PositiveInfinity() => return Undetermined()
            case Undetermined() => return Undetermined()
            case _ => NegativeInfinity()
        }
    }

    def *(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => 
                if (x > 0) 
                    return NegativeInfinity()
                if (x < 0)
                    return PositiveInfinity()
                return IntNumber(0)
            case Undetermined() => return Undetermined()
            case PositiveInfinity() => return NegativeInfinity()
            case NegativeInfinity() => return PositiveInfinity()
        }        
    }

    def >(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => return false
            case Undetermined() => return false
            case _ => true 
        }
    }

    def /(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => 
                if(x < 0) 
                    return PositiveInfinity()
                return NegativeInfinity()
            case _ => return Undetermined()
        }
    }    

    def ==(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => return true
            case _ => return false
        }
    }

    def max(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => return IntNumber(x)
            case PositiveInfinity() => return PositiveInfinity()
            case NegativeInfinity() => return NegativeInfinity()
            case _ => Undetermined()
        }
    }

    def min(x: InfInt): InfInt = {
        x match {
            case Undetermined() => return Undetermined()
            case _ => return NegativeInfinity()
        }
    }

}
  
case class PositiveInfinity() extends InfInt {

    def +(x: InfInt): InfInt = {
        print("NegativeInf+",x)
        x match {
            case NegativeInfinity()=> return Undetermined()
            case Undetermined() => return Undetermined()
            case _ => return PositiveInfinity()
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
            case Undetermined() => return Undetermined()
            case PositiveInfinity() => return PositiveInfinity()
            case NegativeInfinity() => return NegativeInfinity()
        }        
    }

    def >(x: InfInt): Boolean = {
        x match {
            case NegativeInfinity() => return true
            case Undetermined() => return true
            case _ => false 
        }
    }

    def ==(x: InfInt): Boolean = {
        x match {
            case PositiveInfinity() => return true
            case _ => return false
        }
    }

    def /(x: InfInt): InfInt = {
        x match {
            case IntNumber(x) => 
                if(x < 0)
                    return NegativeInfinity()
                return PositiveInfinity()
            case _ => return Undetermined()
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
            case Undetermined() => return Undetermined()
            case NegativeInfinity() => return NegativeInfinity()
            case PositiveInfinity() => return PositiveInfinity()
            case IntNumber(x) => return IntNumber(x)
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
