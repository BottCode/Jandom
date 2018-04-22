public class TestBox {
    public static int BoxWhile() {
        int x = 10;
        int y = 5;
        while (x > 0) {
            y++;
            x--;

        }
        return x;
    }

    public static void BoxIfWhile() {
        int x = 0;
        int y = 10;
        while (x + y <= 15) {
            x--;
            y = y *2;
        }
        int w = 0;
        if (x - y < 30)
            w = x * y;
    }

    public static void BoxIf() {
        int x = 0;
        int y = 10;
        int w = 0;
        if (x - y < 30)
            w = x * y;
    }

    public static void BoxNull() {
        int x = 0;

    }

    public static void remainder() {
	      int x = 10;
	      int y = 0;
	      while (x < 15) {
            x++;
            y++;
        }
        int w = x % y;
    }

    public static void test2() {
        int x = 7;
        while (x >= -10) {
            x = x - 2;
        }
    }

    public static void test4() {
        int x = 0;
        int y = -20;
        int w = x + y;
        if (w - x != -5) {
            y = y * w;
        } else {
            w = x % y;
        }
    }

    public static void test6() {
        int x = 0;
        int y = 15;
        while (x != 5) {
            x = x * y;
        }
    }

    public static void test8() {
        int x = -5;
        int y = 15;
        while (x <= 5) {
            y = x * y;
            x++;
        }
        if (x >= 6) {
            x = x - 10;
        } else {
            x = x + 10;
        }
    }

    public static void test10() {
        int x = 0;
        int y = 7;
        while (x <= 5) {
            x++;
        }
        int w = y % x;
        int z = x % y;
    }

}
