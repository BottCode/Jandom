public class TestBug {
  public static void BoxDoubleDisequality() {
    int x = -5;
    if (x != 5) x++;
  }

  public static void BoxDoubleDisequality1() {
    int x = 5;
    if (x != 5) x++;
  }
}
