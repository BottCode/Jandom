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

}
