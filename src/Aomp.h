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
#define AO_memcpy AAmemcpy
#define AO_memset AAmemset

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
    "   https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html\n"                 \
    "* The number of CPU threads used will default to the maximum number on\n"     \
    "   your system. You can control this value by setting environment variable\n" \
    "   OMP_NUM_THREADS to some smaller value (including 1).\n"                    \
    "* Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of\n"    \
    "   using all CPUs available.\n"                                               \
    "   ++ However, on some systems, it seems to be necessary to set variable\n"   \
    "      OMP_NUM_THREADS explicitly, or you only get one CPU.\n"                 \
    "   ++ On other systems with many CPUS, you probably want to limit the CPU\n"  \
    "      count, since using more than (say) 16 threads is probably useless.\n"   \
    "* You must set OMP_NUM_THREADS in the shell BEFORE running the program,\n"    \
    "   since OpenMP queries this variable BEFORE the program actually starts.\n"  \
    "   ++ You can't usefully set this variable in your ~/.afnirc file or on the\n"\
    "      command line with the '-D' option.\n"                                   \
    "* How many threads are useful? That varies with the program, and how well\n"  \
    "   it was coded. You'll have to experiment on your own systems!\n"            \
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

/*----------------------------------------------------------------------------*/
/* Some macros for allocating workspace arrays, per thread */

#ifdef USE_OMP

#define AOth omp_get_thread_num()

#define AO_NTH_MAX 99

#define AO_DEFINE_SCALAR(typ,nam)   static typ  AO##nam[AO_NTH_MAX]

#define AO_DEFINE_ARRAY(typ,nam)    static typ *AO##nam[AO_NTH_MAX] ;  \
                                    static int AOL##nam[AO_NTH_MAX]

#define AO_DEFINE_2DARRAY(typ,nam)  static typ **AO##nam[AO_NTH_MAX] ; \
                                    static int AOL1##nam[AO_NTH_MAX] ; \
                                    static int AOL2##nam[AO_NTH_MAX]

#define AO_VALUE(nam)               AO##nam[AOth]

#define AO_ARRAY_LEN(nam)           AOL##nam[AOth]

#define AO_2DARRAY_LEN1(nam)        AOL1##nam[AOth]
#define AO_2DARRAY_LNE2(nam)        AOL2##nam[AOth]

#define AO_RESIZE_ARRAY(typ,nam,len)                                 \
  do{ int hh=AOth ;                                                  \
      if( AOL##nam[hh] < len ){                                      \
        AO##nam[hh] = (typ *)realloc(AO##nam[hh],sizeof(typ)*len) ;  \
        AOL##nam[hh] = len ;                                         \
  } } while(0)

#define AO_FREE_ARRAY(nam)        \
  do{ int hh=AOth;                \
      if( AO##nam[hh] != NULL ){  \
        free(AO##nam[hh]) ;       \
        AO##nam[hh] = NULL ;      \
       AOL##nam[hh] = 0 ;         \
  } } while(0)

#define AO_RESIZE_2DARRAY(typ,nam,len1,len2)                                   \
  do{ int hh=AOth, pp, ll1=AOL1##nam[hh], ll2=AOL2##nam[hh];                   \
      if( ll1 < len1 ){                                                        \
        AO##nam[hh] = (typ **)realloc(AO##nam[hh],sizeof(typ *)*len1) ;        \
        for( pp=ll1 ; pp < len1 ; pp++ ) AO##nam[hh][pp] = NULL ;              \
      }                                                                        \
      if( ll1 != len1 || ll2 != len2 ){                                        \
        for( pp=0 ; pp < len1 ; pp++ )                                         \
          AO##nam[hh][pp] = (typ *)realloc(AO##nam[hh][pp],sizeof(typ)*len2) ; \
        AOL1##nam[hh] = len1 ; AOL2##nam[hh] = len2 ;                          \
      }                                                                        \
  } while(0)

#define AO_FREE_2DARRAY(nam)                                     \
  do{ int hh=AOth, ll1=AOL1##nam[hh], pp ;                       \
      if( AO##nam[hh] != NULL ){                                 \
        for( pp=0 ; pp < ll1 ; pp++ ){                           \
          if( AO##nam[hh][pp] != NULL ) free(AO##nam[hh][pp]) ;  \
        }                                                        \
        free(AO##nam[hh]) ;                                      \
        AO##nam[hh] = NULL ; AOL1##nam[hh] = AOL2##nam[hh] = 0 ; \
  } } while(0)

/*----------------------------------------------------------------------------*/
/* Same macros for allocating workspaces, but just one copy of each */

#else  /* not USE_OMP */

#define AO_NTH_MAX 1

#define AO_DEFINE_SCALAR(typ,nam)   static typ AO##nam

#define AO_DEFINE_ARRAY(typ,nam)    static typ *AO##nam; static int AOL##nam

#define AO_DEFINE_2DARRAY(typ,nam)  static typ **AO##nam ;             \
                                    static int AOL1##nam , AOL2##nam ;

#define AO_VALUE(nam)               AO##nam

#define AO_RESIZE_ARRAY(typ,nam,len)                        \
  do{ if( AOL##nam < len ){                                 \
        AO##nam = (typ *)realloc(AO##nam,sizeof(typ)*len) ; \
        AOL##nam = len ;                                    \
  } } while(0)

#define AO_FREE_ARRAY(nam)                           \
  do{ if( AO##nam != NULL ){                         \
        free(AO##nam); AO##nam = NULL; AOL##nam = 0; \
  } } while(0)


#define AO_RESIZE_2DARRAY(typ,nam,len1,len2)                           \
  do{ int pp, ll1=AOL1##nam, ll2=AOL2##nam;                            \
      if( ll1 < len1 ){                                                \
        AO##nam = (typ **)realloc(AO##nam,sizeof(typ *)*len1) ;        \
        for( pp=ll1 ; pp < len1 ; pp++ ) AO##nam[pp] = NULL ;          \
      }                                                                \
      if( ll1 != len1 || ll2 != len2 ){                                \
        for( pp=0 ; pp < len1 ; pp++ )                                 \
          AO##nam[pp] = (typ *)realloc(AO##nam[pp],sizeof(typ)*len2) ; \
        AOL1##nam = len1 ; AOL2##nam = len2 ;                          \
      }                                                                \
  } while(0)

#define AO_FREE_2DARRAY(nam)                             \
  do{ int ll1=AOL1##nam, pp ;                            \
      if( AO##nam != NULL ){                             \
        for( pp=0 ; pp < ll1 ; pp++ ){                   \
          if( AO##nam[pp] != NULL ) free(AO##nam[pp]) ;  \
        }                                                \
        free(AO##nam) ;                                  \
        AO##nam = NULL ; AOL1##nam = AOL2##nam = 0 ;     \
  } } while(0)

#endif  /* USE_OMP */

/*----------------------------------------------------------------------------*/

#endif /* _AOMP_INCLUDED__ */
