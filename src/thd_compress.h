#ifndef _MCW_COMPRESSOR_
#define _MCW_COMPRESSOR_

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

#define COMPRESS_NOFILE    -666
#define COMPRESS_NONE      -1
#define COMPRESS_GZIP       0
#define COMPRESS_BZIP2      1
#define COMPRESS_COMPRESS   2

#define COMPRESS_LASTCODE   2

static char * COMPRESS_suffix[]     = { ".gz" , ".bz2" , ".Z" } ;
static int    COMPRESS_suffix_len[] = { 3    , 4       , 2    } ;

static char * COMPRESS_program[]    = { "gzip -1c > %s"  ,
                                        "bzip2 -1c > %s" ,
                                        "compress > %s"  } ;

static char * COMPRESS_unprogram[]  = { "gzip -dc %s"  ,
                                        "bzip2 -dc %s" ,
                                        "uncompress -c %s" } ;

static char * COMPRESS_enviro[] = { "GZIP" , "BZIP2" , "COMPRESS" } ;

/*---------- prototypes ----------*/

extern int COMPRESS_is_file( char * pathname ) ;
extern int COMPRESS_has_suffix( char * fname , int mode ) ;
extern int COMPRESS_filecode( char * fname ) ;
extern int COMPRESS_fclose( FILE * fp ) ;
extern FILE * COMPRESS_fopen_read( char * fname ) ;
extern FILE * COMPRESS_fopen_write( char * fname , int mm ) ;

extern char * COMPRESS_filename( char * fname ) ; /* Feb 1998 */
extern int COMPRESS_unlink( char * fname ) ;      /* Feb 1998 */

#endif
