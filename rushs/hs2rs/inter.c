void init(void) {
  static char *argv[] = { "libhs.so", 0 }, **argv_ = argv;
  static int argc = 1;

  hs_init(&argc, &argv_);
}

void fin(void) {
  hs_exit();
}
