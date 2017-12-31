/**
 * Copyright 2017 Mirko Bez, Stefano Munari, Sebastiano Valle
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
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

public class NumericalTestExtra {

    static void simple_multiplication_test() {
		int a = 10;
		int b = -2;
		int c = 10 + a*b;
    }
    static void simple_algebra_example() {
    	int a = 10;
    	a = -a;
    	int c = -100 * + a;
    }

    static void simple_loop_with_exit() {
        int a = 0;
        while(a <= 0)
            a = 1;
    }

    static void remainder_of_one() {
        int x = 1;
        x = x % 2;
    }

    @SuppressWarnings("ALL")
    static void filter() {
    	int a = 2;
    	a += -12;
    	if(a < 0)
    		a++;
    	else
    		a--;
    }

    static void divison_by_zero() {
    	int x = 0;
    	int y = 2;
    	int z = y / x;
    }
}
