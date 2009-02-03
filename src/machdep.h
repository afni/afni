/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
#ifndef _MCW_MACHDEP_
#define _MCW_MACHDEP_

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

#ifdef  __cplusplus
extern "C" {
#endif

extern void machdep() ;

#ifdef  __cplusplus
}
#endif

/*----------------------------------------------------------------------------
  Flags that can be used to work around bugs on some systems
  (you could also use the -Dname command line switch in the Makefile
   definition of the CC commands to enable these options):

    USE_TRACING = if #define-d, then AFNI and its programs will compile
                   in a set of data/functions that allows debug tracing
                   of execution.

    DONT_USE_MCW_MALLOC = if this is set, then the malloc wrappers
                          defined in mcw_malloc.[ch] will not be
                          be used.  (These functions provide some
                          ability to track and debug the use of
                          malloc-ed memory space.)

    DONT_USE_METER  = if #define-d, won't show progress meter during
                       brick write operations (and other similar places)

    FIX_SCALE_VALUE_PROBLEM = if #defined-d, will work around a bug
                               in Solaris Motif where the threshold scale
                               value is not displayed
                               [Doesn't seem to be needed anymore]

    FIX_SCALE_SIZE_PROBLEM = if #define-d, will work around a bug in
                              some versions of Motif where the
                              threshold scale resizes itself whenever
                              the pbar is touched

    FIX_SCALE_SIZE_LATER   = if this is ALSO defined, then the
                             FIX_SCALE_SIZE_PROBLEM is applied after a
                             time delay

    SCANDIR_WANT_CONST = if #define-d, says that the "scandir" library
                          routine wants "const" arguments -- setting this
                          flag will avoid some stupid compiler warnings

    DONT_USE_SCANDIR   = if #define-d, the Unix routine scandir won't
                          be used.  This seems to help on Solaris, and
                          doesn't hurt on other systems, so it is now
                          the default.

    DONT_INSTALL_ICONS = if #define-d, won't try to install icons for the
                          various windows (Sun's OpenWindows complains when
                          I do this, and I don't know why, and I don't care)

    DONT_CHECK_FOR_MWM = if this is set, then the program won't bother
                          to check for the Motif Window Manager (MWM).
                          This can be useful on Linux systems using FVWM,
                          which won't be detected as MWM, but can be set up
                          to act like it with regards to decorations, etc.

    BOXUP_SCALE = if this is set, then the slider for the functional
                  threshold will have a "box" (Frame widget) drawn
                  around it.  Some people think this looks nicer, some don't.

    NO_FRIVOLITIES = if this is set, then the hidden "fun" parts of
                     AFNI are disabled.  What these are is a secret.

    DONT_USE_HINTS = if this is set, then the popup hints won't be
                     compiled into AFNI.

    NEED_XSETLOCALE = if this is set, then the routine _Xsetlocale
                      must be provided (needed for some Linux systems).

    NEED_NL_LANGINFO = if this is set, then the routine nl_langinfo()
                       must be provided (need on Mac OS X)

    DONT_UNROLL_FFTS = if this is set, then the unrolled FFT routines
                       (for lengths 32, 64, 128, 256) will NOT be used --
                       they are generally faster, but may have trouble
                       compiling on some systems [see file csfft.c].
                       The program fftest.c can be used to test the
                       speed of FFTs.

    SOLARIS_DIRENT_PATCH = if this is set, then a patch for the
                           difference between Sun's "dirent" functions
                           and everbody else's is used in the file
                           mcw_glob.c -- this seems to be necessary
                           on Solaris 2.6 systems in order to get the
                           AFNI file reading software to work.
                           (This patch was supplied by Christoph Losert
                           of the Institut fuer Radiologische Diagnostik,
                           Munchen, and has not been tested at MCW due to
                           the absence of any Sun workstations to play with.)

    SOLARIS_DIRENT_ZERO  = if this is defined, the patch described above
                           is modified slightly -- see mcw_glob.c for
                           the gory details.  The patch above seems to
                           be needed for Solaris 2.5.x and this modification
                           is also needed for Solaris 2.6.x.
                              [God, I hope this is the end of this ]
                              [Solaris nightmare -- I'm sick of it!]

    USE_FLOCK = There are two incompatible ways of 'locking' a file
    USE_LOCKF = on Unix: the flock() and lockf() functions.  Defining
                one of these will enable the use of the corresponding
                function.  If neither is defined, file locking will
                not be used.

    DONT_USE_SHM = Set this to disable use of shared memory.

    DISCARD_EXCESS_EXPOSES = Set this if a ConfigureNotify event is
                             followed by an Expose event on your
                             systems - this will eliminate duplicate
                             image redraws in imseq.c.

    ENFORCE_ASPECT = Set this if you want image aspect ratio enforcement
                     compiled into your system.  Can also be set at runtime
                     using the AFNI_ENFORCE_ASPECT environment variable.

  Exactly one of the following flags must be set for AFNI plugins
  to work:

    DYNAMIC_LOADING_VIA_DL = if this is set, then loading of dynamic
                             libraries is accomplished using the "dl"
                             routines, such as "dlopen".  This is true
                             for all Unixes that I know of, except for
                             HP-UX.

    DYNAMIC_LOADING_VIA_SHL = if this is set, then loading of dynamic
                              libraries is accomplished using the "shl"
                              routines, such as "shl_load".  This is
                              only used on HP-UX, as far as I know.

    NO_DYNAMIC_LOADING = if this is set, then AFNI will load the plugins
                         statically - this means that you can't add plugins
                         without recompiling AFNI;  this option has only
                         been tested on CYGWIN, and requires a special Makefile.

    BAD_BUTTON3_POPUPS = if this is set, then Button-3 popup menus don't work
                         and the program tries something else (i.e., Solaris).

  Flags that MUST be set appropriately for each system:

    THD_MMAP_FLAG = value to set when using "mmap" to map a file to memory
                     (see man mmap to figure this one out)

    THD_MKDIR_MODE = flag to set creation mode for directories that might
                      be created during dataset output (you can probably
                      copy this from any other machine)

    NO_RINT = #define this if the system you are on doesn't have the
              "rint" (round-to-integer) math function -- I don't know
              of any system without this function, but you never can tell.

    DONT_USE_STRPTIME = #define this if your system doesn't have the
                        C function strptime()

    USE_RANDOM = #define this if you want/have to use the srandom/random
                 functions instead of the srand48/drand48 functions for
                 random number generation.

  Some systems need extra header files included.  Some system header
  files don't give a prototype for alphasort.  This is a place to fix
  these things up.
-------------------------------------------------------------------------*/

#define DONT_USE_SCANDIR
#define DONT_UNROLL_FFTS  /* off by default */

#ifdef DT_UNKNOWN                 /** 07 Mar 2006: niftilib problem fix **/
# define QQDT_UNK DT_UNKNOWN
# undef  DT_UNKNOWN
#else
# undef  QQDT_UNK
#endif

/*** HP-UX ***/

#ifdef HP
# include <dirent.h>
# define THD_MMAP_FLAG  (MAP_FILE | MAP_VARIABLE | MAP_SHARED)
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define BOXUP_SCALE
# define FIX_SCALE_SIZE_PROBLEM
# define DYNAMIC_LOADING_VIA_SHL
# undef  DONT_UNROLL_FFTS    /* FFTs are faster, but csfft.c compiles slowly */
# define USE_LOCKF
#endif

/*** SGI IRIX ***/

#if defined(SGI) || defined(OSF1) /*BUG: should have a separate OSF1 ifdef*/
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define BOXUP_SCALE
# define DYNAMIC_LOADING_VIA_DL
# define FIX_SCALE_SIZE_PROBLEM
# ifndef DONT_USE_SCANDIR
    extern int alphasort(struct dirent **, struct dirent **) ;
# endif
# undef  DONT_UNROLL_FFTS         /* helps some */
# define USE_FLOCK
#endif

/*** SunOS or Solaris ***/

#ifdef SPARKY
# include <sys/dirent.h>
# define THD_MMAP_FLAG  (MAP_SHARED | MAP_NORESERVE)
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define DONT_INSTALL_ICONS
# define NO_FRIVOLITIES
# define FIX_SCALE_SIZE_PROBLEM
# ifndef DONT_USE_SCANDIR
    extern int alphasort(struct dirent **, struct dirent **) ;
# endif

extern double strtod() ;
extern long   strtol() ;

/**
#ifndef S_IFREG
#  define S_IFREG _IFREG
# endif
# ifndef S_IFDIR
#  define S_IFDIR _IFDIR
# endif
**/
#endif

#if defined(SOLARIS) || defined(SUN)
# include <sys/types.h>
# ifndef DONT_USE_SCANDIR
#   include <sys/dir.h>
    extern int alphasort(struct dirent **, struct dirent **) ;
# endif
# define THD_MMAP_FLAG  (MAP_SHARED | MAP_NORESERVE)
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define dirent direct
# define FIX_SCALE_SIZE_PROBLEM
# define FIX_SCALE_SIZE_LATER
# define DONT_INSTALL_ICONS
# define DYNAMIC_LOADING_VIA_DL
# define USE_LOCKF
# define DONT_USE_MATRIX_MAT  /* 04 Mar 2005 */

#ifdef SOLARIS_OLD            /* 03 Feb 2009 [rickr] */
# define fabsf  fabs          /* 09 Jul 2007 */
# define sqrtf  sqrt          /* to deal with lameness of Solaris */
# define cbrtf  cbrt
# define logf   log
# define powf   pow
# define sinf   sin
# define cosf   cos
# define asinf  asin
# define acosf  acos
# define floorf floor
# define ceilf  ceil
# define expf   exp
#endif

#endif

/*** IBM RS6000 courtesy Doug Morris of UIUC ***/

#ifdef RS6000
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define FIX_SCALE_SIZE_PROBLEM
# ifndef DONT_USE_SCANDIR
    extern int alphasort(struct dirent **, struct dirent **) ;
# endif
#endif

/*** Linux 1.2.x ***/

#ifdef LINUX2
# define NEED_XSETLOCALE
# ifndef LINUX
#   define LINUX
# endif
#endif

#if defined(LINUX) || defined(FreeBSD) || defined(NetBSD) || defined(OpenBSD)
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define FIX_SCALE_SIZE_PROBLEM   /* Motif 2.0 bug? */
/* # define MMAP_THRESHOLD -1 */       /* no mmap-ing */
# define DONT_CHECK_FOR_MWM       /* assume Motif WM functionality is present */
# define BOXUP_SCALE              /* looks nicer */
# define DYNAMIC_LOADING_VIA_DL
# undef  DONT_UNROLL_FFTS         /* helps a lot */
# define USE_FLOCK
#endif

#ifdef CYGWIN
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define FIX_SCALE_SIZE_PROBLEM   /* Motif 2.0 bug? */
# define MMAP_THRESHOLD -1        /* no mmap-ing */
# define DONT_CHECK_FOR_MWM       /* assume Motif WM functionality is present */
/**# define BOXUP_SCALE**/              /* looks nicer */
# define NO_DYNAMIC_LOADING
# undef  DONT_UNROLL_FFTS         /* helps a lot */
# define DONT_USE_STRPTIME
# define NO_FRIVOLITIES
# define USING_LESSTIF            /* try to avoid some bugs */
# define FIX_SCALE_SIZE_PROBLEM
#endif

/* SCO UDK under Unixware 7 -- contributed by Jason Bacon */
#ifdef SCO
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define FIX_SCALE_SIZE_PROBLEM   /* Motif 2.0 bug? */
/* # define MMAP_THRESHOLD -1 */       /* no mmap-ing */
# define DONT_CHECK_FOR_MWM       /* assume Motif WM functionality is present */
# define BOXUP_SCALE              /* looks nicer */
# define DYNAMIC_LOADING_VIA_DL
# undef  DONT_UNROLL_FFTS         /* helps a lot */
#endif

/* Mac OSX (Darwin) */
#ifdef DARWIN
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define FIX_SCALE_SIZE_PROBLEM   /* Motif 2.0 bug? */
# define MMAP_THRESHOLD -1        /* no mmap-ing */
#if 0
# define DONT_CHECK_FOR_MWM       /* assume Motif WM functionality is present */
#endif
# define BOXUP_SCALE              /* looks nicer */
# define DYNAMIC_LOADING_VIA_DL
# undef  DONT_UNROLL_FFTS
# define USE_FLOCK
# define USE_RANDOM
# define DONT_USE_STRPTIME
/** # define NEED_XSETLOCALE **/  /* removed on 16 May 2005, for Tiger */
# define NEED_NL_LANGINFO
# define ENFORCE_ASPECT           /* 29 Apr 2003 */
#endif

/************************************************************************
   Do NOT change anything below this line (unless your name is Cox)!
*************************************************************************/

#ifdef QQDT_UNK                   /** 07 Mar 2006 **/
# undef  DT_UNKNOWN
# define DT_UNKNOWN QQDT_UNK
# undef  QQDT_UNK
#endif

# define DISCARD_EXCESS_EXPOSES   /* 15 Aug 2002 */

#if defined(DYNAMIC_LOADING_VIA_DL) || defined(DYNAMIC_LOADING_VIA_SHL) || defined(NO_DYNAMIC_LOADING)
#  define ALLOW_PLUGINS
#else
#  define DONT_ALLOW_PLUGINS
#endif

#ifdef NO_RINT
extern double rint(double) ;  /* 12 Feb 2001 */
#endif

#ifdef USE_RANDOM             /* 04 Sep 2001 (cf. machdep.c) */
extern void srand48(long int);
extern double drand48(void);
extern long int lrand48(void);
#endif

#ifdef NO_GAMMA               /* 16 May 2005 (cf. machdep.c) */
extern double lgamma(double) ;
#endif

extern char * Random_Insult(void) ;

#endif /* _MCW_MACHDEP_ */
