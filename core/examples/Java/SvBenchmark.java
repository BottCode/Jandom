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

/**
 * These examples are a Java-Translation of some C examples taken from the the
 * <a href="https://github.com/sosy-lab/sv-benchmarks">sv-benchmark</a>
 */
public class SvBenchmark {
    // taken from sv-benchmarks repository
    static void sum04_false_unreach_call_true_termination(){
        int i = 1;
        int sn = 0;
        while(i <= 8) {
            if (i < 4)
                sn = sn + (2);
            i = i + 1;
        }
        // POST CONDITION: sn == 6
    }

    static void for_infinite_loop_2_true_unreach_call_false_termination() {
        int i_U = 0;
        int x = 0;
        int y = 0;
        int n = x*y*i_U + 12*3*x*y;
        if(!(n > 0)) {
            return;
        }
        boolean z = true;
        for(i_U = 0; z; i_U++) {
            x++;
            //verifierAssert(!x ? 1 : 0);
        }
        //UNREACHABLE CODE
        x += 12;
    }
}
