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

    public static void Test1(){
        int i = 0;
        while(i < 10) {
            i++;
        }
    }

    public static void Test3(){
        int i = 1;
        while(i < 10) {
            i = i * 11;
        }
    }

    public static void Test33(){
        int i = 1;
        while(i < 10) {
            i = i * 11;
        }
    }

    public static void Test7(){
        int k = Integer.MAX_VALUE;
        int i = 0;
        while(i < k){
            i++;
        }
    }

    public static void Test8(){
        int i = 0;
        int x = 2;
        int z = 0;
        while(i < 10){
            i++;
            if(i % 2 == 0){
                x = x * 2;
            }else{
                x = x* (-2);
            }
        }

    }
    

}
