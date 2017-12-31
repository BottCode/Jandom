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

class Congruence {
	static void basic_congruence_add() {
		int x = 3;
		int y = 12;
		if(x < 12)
			x += 1;
		else 
			x += 2;
	}
	
	static void basic_congruence_mul() {
		int x = 2;
		int y = 3;
		int z = x*y;
		while(z < 10) {
			z *= x;
		}
	}

	/**
	 * Example taken from
	 * <a href="https://www-apr.lip6.fr/~mine/enseignement/mpri/attic/2013-2014/03b-num-nonrel_ok.pdf">
	 * this slides of Minè (91/103)
	 * </a>
	 */
	static void mineCongruence() {
		int x = 0;
		int y = 2;
		while(x < 40) {
			x = x + 2;
			if(x<5)
				y=y+18;
			if(x>8)
				y=y-30;
		}
	}

	/**
	 * Example taken from
	 * <a href="https://www-apr.lip6.fr/~mine/enseignement/mpri/attic/2013-2014/03b-num-nonrel_ok.pdf">
	 * this slides of Minè (96/103)
	 * </a>
	 */
	static void mineProductIntervalCongruence() {
		int x = 1;
		while(x-10 <= 0) {
			x = x + 2;
		}
		if(x-12 >= 0)
			x=0;
	}
}
