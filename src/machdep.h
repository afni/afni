#ifndef _MCW_MACHDEP_
#define _MCW_MACHDEP_

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

/*----------------------------------------------------------------------------
  Flags that can be used to work around bugs on some systems:

    DONT_USE_METER  = if #define-d, won't show progress meter during
                       brick write operations

    FIX_SCALE_VALUE_PROBLEM = if #defined-d, will work around a bug
                               in Solaris Motif where the threshold scale
                               value is not displayed

    FIX_SCALE_SIZE_PROBLEM = if #define-d, will work around a bug in
                              some versions of Motif where the
                              threshold scale resizes itself whenever
                              the pbar is touched

    SCANDIR_WANT_CONST = if #define-d, says that the "scandir" library
                          routine wants "const" arguments -- setting this
                          flag will avoid some stupid compiler warnings

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
                  around it.  some people think this looks nicer, some don't.

    USE_GNU_MALLOC = if this is set, then the AFNI program will use the
                     GNU malloc/free library in place of the system
                     defined library.  GNU's library seems to be more
                     robust against programming flaws (shockingly, there
                     are flaws in AFNI).

    NO_FRIVOLITIES = if this is set, then the hidden "fun" parts of
                     AFNI are disabled.  What these are is a secret.

    DONT_USE_HINTS = if this is set, then the popup hints won't be
                     compiled into AFNI.

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

  Flags that MUST be set appropriately for each system:

    THD_MMAP_FLAG = value to set when using "mmap" to map a file to memory
                     (see man mmap to figure this one out)

    THD_MKDIR_MODE = flag to set creation mode for directories that might
                      be created during dataset output (you can probably
                      copy this from any other machine)


  Some systems need extra header files included.  Some system header
  files don't give a prototype for alphasort.  This is a place to fix
  these things up.
-------------------------------------------------------------------------*/

/*** HP-UX ***/

#ifdef HP
# include <dirent.h>
# define THD_MMAP_FLAG  (MAP_FILE | MAP_VARIABLE | MAP_SHARED)
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define BOXUP_SCALE
# define FIX_SCALE_SIZE_PROBLEM
# define DYNAMIC_LOADING_VIA_SHL
#endif

/*** SGI IRIX ***/

#ifdef SGI
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define BOXUP_SCALE
# define DYNAMIC_LOADING_VIA_DL
# define FIX_SCALE_SIZE_PROBLEM
extern int alphasort(struct dirent **, struct dirent **) ;
#endif

/*** SunOS or Solaris ***/

#ifdef SPARKY
# include <sys/dirent.h>
# define THD_MMAP_FLAG  (MAP_SHARED | MAP_NORESERVE)
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define DONT_INSTALL_ICONS
# define NO_FRIVOLITIES
# define FIX_SCALE_SIZE_PROBLEM
extern int alphasort(struct dirent **, struct dirent **) ;

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
# include <sys/dir.h>
# define THD_MMAP_FLAG  (MAP_SHARED | MAP_NORESERVE)
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define dirent direct
# define FIX_SCALE_SIZE_PROBLEM
# define DONT_INSTALL_ICONS
# define DYNAMIC_LOADING_VIA_DL
extern int alphasort(struct dirent **, struct dirent **) ;
#endif

/*** IBM RS6000 courtesy Doug Morris of UIUC ***/

#ifdef RS6000
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define FIX_SCALE_SIZE_PROBLEM
extern int alphasort(struct dirent **, struct dirent **) ;
#endif

/*** Linux 1.2.x ***/

#ifdef LINUX
# include <dirent.h>
# define THD_MMAP_FLAG  MAP_SHARED
# define THD_MKDIR_MODE (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
# define SCANDIR_WANTS_CONST
# define FIX_SCALE_SIZE_PROBLEM   /* Motif 2.0 bug? */
/* # define MMAP_THRESHOLD -1 */       /* no mmap-ing */
# define DONT_CHECK_FOR_MWM       /* assume Motif WM functionality is present */
# define BOXUP_SCALE              /* looks nicer */
# define DYNAMIC_LOADING_VIA_DL
#endif

/************************************************************************
   Do NOT change anything below this line (unless your name is Cox)!
*************************************************************************/

#if defined(DYNAMIC_LOADING_VIA_DL) || defined(DYNAMIC_LOADING_VIA_SHL)
#  define ALLOW_PLUGINS
#else
#  define DONT_ALLOW_PLUGINS
#endif

#endif /* _MCW_MACHDEP_ */
