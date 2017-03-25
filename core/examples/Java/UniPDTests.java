public class UniPDTests {

    static void simple_multiplication_test() {
		int a = 10;
		int b = -2;
		int c = 10 + a*b;
    }
    static void a_little_bit_of_algebra() {
    	int a = 10;
    	a = -a;
    	int c = -100 * + a;
    }

    static void simple_loop_with_exit() {
        int a = 0;
        while(a <= 0)
            a = 1;
    }
<<<<<<< HEAD:core/examples/Java/UniPDTests.java

    static void reduceOddAndGeq() {
        int x = 1;
        x = x % 2;
=======
    @SuppressWarnings("ALL")
    static void filter() {
    	int a = 2;
    	a += -12;
    	if(a < 0)
    		a++;
    	else
    		a--;
    	
>>>>>>> Taken changes from other brancheTaken changes from other branchess:core/examples/Java/Multiplication.java
    }
    static void divisonByZero() {
    	int x = 0;
    	int y = 2;
    	int z = y / x;
    }

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
