/* Test-only srand48 override for check_surf_goto_equivalence.py.
   powell_int.c seeds its multi-start fitter from time()+getpid(), overriding
   AFNI_RANDOM_SEEDVAL. Keep the libc random sequence, but use the same starting
   state in baseline and candidate processes. Never link this into production.

   macOS: cc -dynamiclib surf_fixed_seed.c -o /tmp/surf_fixed_seed.dylib
   Linux: cc -shared -fPIC surf_fixed_seed.c -o /tmp/surf_fixed_seed.so
*/
#include <stdlib.h>

/* seed48 uses the same state representation as srand48(713). */
static void surf_fixed_srand48(long ignored)
{
   unsigned short state[3] = { 0x330e, 713, 0 };
   (void)ignored;
   seed48(state);
}

#ifdef __APPLE__
static const struct {
   void (*replacement)(long);
   void (*original)(long);
} surf_seed_interpose __attribute__((used, section("__DATA,__interpose"))) = {
   surf_fixed_srand48, srand48
};
#else
void srand48(long ignored)
{
   surf_fixed_srand48(ignored);
}
#endif
