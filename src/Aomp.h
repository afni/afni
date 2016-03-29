#ifndef _AOMP_INCLUDED__
#define _AOMP_INCLUDED__

#ifndef INLINE
# define INLINE /*nada*/
#endif

/*------------- Macros to be used in OpenMP enabled AFNI code ----------------*/

/* to replace memcpy and memset, which cause trouble inside a parallel region */

#ifdef USE_OMP
static INLINE void AAmemcpy( void *ooo , void *iii , size_t nnn )
{ register size_t jj ; register char *oar, *iar ;
  if( ooo == NULL || iii == NULL || nnn == 0 ) return ;
  oar = (char *)ooo ; iar = (char *)iii ;
  for( jj=0 ; jj < nnn ; jj++ ) *oar++ = *iar++ ;
}
static INLINE void AAmemset( void *ooo , int c , size_t nnn )
{ register size_t jj ; register char cc , *oar ;
  if( ooo == NULL || nnn == 0 ) return ;
  oar = (char *)ooo ; cc = (char)c ;
  for( jj=0 ; jj < nnn ; jj++ ) *oar++ = cc ;
}
#else
# define AAmemcpy memcpy
# define AAmemset memset
#endif
#define AA_memcpy AAmemcpy
#define AA_memset AAmemset

/* to disable ENTRY/RETURN macros (which use static variables) */

#if defined(USE_OMP) && defined(USE_TRACING)
# define AFNI_OMP_START DBG_stoff++
# define AFNI_OMP_END   DBG_stoff--
#else
# define AFNI_OMP_START   /*nada*/
# define AFNI_OMP_END     /*nada*/
#endif

/* Set max number of threads to be at most thn */

#ifdef USE_OMP
# define AFNI_SETUP_OMP(thn)                            \
  do{ int mm=omp_get_max_threads() , nn=thn , ee;       \
      ee = (int)AFNI_numenv("OMP_NUM_THREADS") ;        \
      if( ee <= 0 ){                                    \
        if( nn < 1 ) nn = 15 ; if( mm > nn ) mm = nn ;  \
        omp_set_num_threads(mm) ;                       \
      }                                                 \
  } while(0)
#else
# define AFNI_SETUP_OMP(thn) /*nada*/
#endif

/* Macro to use in -help output */

#ifdef USE_OMP
# define PRINT_AFNI_OMP_USAGE(pnam,extra)                                          \
  printf(                                                                          \
    "\n"                                                                           \
    " =========================================================================\n" \
    "* This binary version of %s is compiled using OpenMP, a semi-\n"              \
    "   automatic parallelizer software toolkit, which splits the work across\n"   \
    "   multiple CPUs/cores on the same shared memory computer.\n"                 \
    "* OpenMP is NOT like MPI -- it does not work with CPUs connected only\n"      \
    "   by a network (e.g., OpenMP doesn't work with 'cluster' setups).\n"         \
    "* For implementation and compilation details, please see\n"                   \
    "   https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html\n"                  \
    "* The number of CPU threads used will default to the maximum number on\n"     \
    "   your system.  You can control this value by setting environment variable\n"\
    "   OMP_NUM_THREADS to some smaller value (including 1).\n"                    \
    "* Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of\n"    \
    "   using all CPUs available.\n"                                               \
    "   ++ However, on some systems (such as the NIH Biowulf), it seems to be\n"   \
    "      necessary to set OMP_NUM_THREADS explicitly, or you only get one CPU.\n"\
    "   ++ On other systems with many CPUS, you probably want to limit the CPU\n"  \
    "      count, since using more than (say) 16 threads is probably useless.\n"   \
    "* You must set OMP_NUM_THREADS in the shell BEFORE running the program,\n"    \
    "   since OpenMP queries this variable BEFORE the program actually starts.\n"  \
    "   ++ You can't usefully set this variable in your ~/.afnirc file or on the\n"\
    "      command line with the '-D' option.\n"                                   \
    "* How many threads are useful?  That varies with the program, and how well\n" \
    "   it was coded.  You'll have to experiment on your own systems!\n"           \
    "* The number of CPUs on this particular computer system is ...... %d.\n"      \
    "* The maximum number of CPUs that will be used is now set to .... %d.\n"      \
    "%s"                                                                           \
    " =========================================================================\n" \
    , (pnam) , omp_get_num_procs() , omp_get_max_threads() ,                       \
      (extra==NULL) ? "\0" : extra                                                 \
  )
#else
# define PRINT_AFNI_OMP_USAGE(pnam,extra)                                          \
  printf(                                                                          \
    "\n"                                                                           \
    " =========================================================================\n" \
    "* This binary version of %s is NOT compiled using OpenMP, a\n"                \
    "   semi-automatic parallelizer software toolkit, which splits the work\n"     \
    "   across multiple CPUs/cores on the same shared memory computer.\n"          \
    "* However, the source code is modified for OpenMP, and can be compiled\n"     \
    "   with an OpenMP-capable compiler, such as gcc 4.2+, Intel's icc, and\n"     \
    "   Sun Studio.\n"                                                             \
    "* If you wish to compile this program with OpenMP, see the man page for\n"    \
    "   your C compiler, and (if needed) consult the AFNI message board, and\n"    \
    "   https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html\n"                  \
    , (pnam)                                                                       \
  )
#endif


#endif /* _AOMP_INCLUDED__ */
