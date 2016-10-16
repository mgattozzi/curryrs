#include <HsFFI.h>

static void my_enter(void)
{
  static char *argv[] = { "libhtest.so", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}

static void my_exit(void)
{
  hs_exit();
}

