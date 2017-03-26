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

	static void mineProductIntervalCongruence() {
		int x = 1;
		while(x-10 <= 0) {
			x = x + 2;
		}
		if(x-12 >= 0)
			x=0;
	}
}
