/**
 */

class Congruence {
	static void test() {
		int x = 3;
		int y = 12;
		if(x < 12)
			x += 1;
		else 
			x += 2;
	}
	
	static void testCongruence() {
		int x = 2;
		int y = 3;
		int z = x*y;
		while(z < 10) {
			z *= x;
		}
	}
}
