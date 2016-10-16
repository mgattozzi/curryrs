int tripleNum(int x) {
  static char *argv[] = { "libchtest.so", 0 }, **argv_ = argv;
  static int argc = 1;

  hs_init(&argc, &argv_);
  int y = triple(x);
  hs_exit();
  return y;
}
