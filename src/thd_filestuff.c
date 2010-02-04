/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*-------------------------------------------------------------*/
/*! Return the time at which the file was last modified. */

time_t THD_file_mtime( char *pathname )  /* 05 Dec 2001 */
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   return (time_t)buf.st_mtime ;
}

/*-----------------------------------------------------------*/
/*! Determine if this exists at all (file, directory, ...). */

int THD_is_ondisk( char *pathname )  /* 19 Dec 2002 */
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ) ;
   return (ii == 0) ;
}

/*-----------------------------------------------------------*/
/*! Change working directory. */

int THD_cwd( char *pathname )    /* 19 Dec 2002 */
{
   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   return ( chdir(pathname) == 0 ) ;
}

/*-----------------------------------------------------------*/
/*! Create a directory.  Returns 1 if OK, 0 if not. */

int THD_mkdir( char *pathname )  /* 19 Dec 2002 */
{
   int lp , ii , jj ;
   char *pnam ;

   /* check if input is OK, or if it already exists */

   if( !THD_filename_ok(pathname) ) return 0 ;
   if(  THD_is_ondisk  (pathname) ) return THD_is_directory(pathname) ;

   pnam = strdup(pathname) ;  /* modifiable copy */
   lp = strlen(pnam) ; ii = 0 ;

   /* loop over path segments, creating them if needed */

   while(1){

     /* advance ii to point to end of next path segment,
        at the next '/' character, or at the end of pnam */

     ii += strspn(pnam+ii,"/") ; ii += strcspn(pnam+ii,"/") ;

     /* insert a NUL to replace the '/', temporarily */

     if( ii < lp ) pnam[ii] = '\0' ;

     /* if this segment doesn't already exist, create it */

     if( !THD_is_directory(pnam) ){
       jj = mkdir( pnam , 0755 ) ;
       if( jj != 0 ){ free(pnam); return 0; } /* bad */
     }

     /* if reached end of path string, we're done */

     if( ii == lp ){ free(pnam); return 1; }  /* good */

     /* reinsert '/' if it was excised */

     pnam[ii] = '/' ;
   }

   return 0 ; /* unreachable */
}

/*-----------------------------------------------------------*/
/*! Determine if this is really a regular file or not. */

int THD_is_file( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFREG) != 0 ; return ii ;
}

/*------------------------------------------------------------*/
/*! Determine if this is really a symbolic link or not. */

int THD_is_symlink( char *pathname )  /* 03 Mar 1999 */
{
   char buf[32] ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = readlink( pathname , buf , 32 ) ;
   return (ii > 0) ;
}

/*-------------------------------------------------------*/
/*! Return the file length (0 if file not found). */

long long THD_filesize( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   return (long long)buf.st_size ;
}

/*--------------------------------------------------------*/
/*! Determine if this is really a directory or not. */

int THD_is_directory( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFDIR) != 0 ; return ii ;
}

/*---------------------------------------------------------------*/
/*! Determine if this is really an executable file or not. */

int THD_is_executable( char *pathname )  /* 26 Jun 2001 */
{
   static struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' )  return 0  ;
   ii = stat( pathname , &buf )      ; if( ii ) return 0  ;
   ii = (buf.st_mode & S_IXOTH) != 0 ; if( ii ) return ii ;

   /* 15 Jul 2002: also check if file is owned & executable by user */

   ii = ( getuid() == buf.st_uid       &&
          (buf.st_mode & S_IXUSR) != 0   ) ;
   return ii ;
}

/*--------------------------------------------------------------*/
/*! Determine if two filenames are really the same thing. */

int THD_equiv_files( char *path1 , char *path2 )
{
   static struct stat buf1 , buf2 ; int ii ;

   if( path1 == NULL || path2 == NULL ) return -1 ;
   ii = stat( path1 , &buf1 ) ; if( ii != 0 ) return -1 ;
   ii = stat( path2 , &buf2 ) ; if( ii != 0 ) return -1 ;

   ii = (buf1.st_dev == buf2.st_dev) && (buf1.st_ino == buf2.st_ino) ;
   return ii ;
}

/*-----------------------------------------------------------------*/
/*! Find a 'trailing name in a pathname.

   For example, for fname = "/bob/cox/is/the/author/of/AFNI",
     - the lev=0 trailing name is "AFNI",
     - the lev=1 trailing name is "of/AFNI",
     - the lev=2 trailing name is "author/of/AFNI", and so on.
   That is, "lev" is the number of directory names above the
   last name to keep.  The pointer returned is to some place
   in the middle of fname; that is, this is not a malloc()-ed
   string, so don't try to free() it!.
-------------------------------------------------------------------*/

char * THD_trailname( char *fname , int lev )
{
   int fpos , flen , flev ;

   if( fname == NULL || (flen=strlen(fname)) <= 1 ) return fname ;

   if( lev < 0 ) lev = 0 ;

   flev = 0 ;
   fpos = flen ;
   if( fname[fpos-1] == '/' ) fpos-- ;  /* skip trailing slash */

   /* fpos   = index of latest character I've accepted,
      fpos-1 = index of next character to examine,
      flev   = number of directory levels found so far */

   while( fpos > 0 ){

      if( fname[fpos-1] == '/' ){
         flev++ ; if( flev >  lev ) break ;  /* reached the lev we like */
      }
      fpos-- ;  /* scan backwards */
   }

   return (fname+fpos) ;
}

/*----------------------------------------------------------------------*/

int THD_character_ok( char c )  /* 04 Feb 2010 */
{
   if( iscntrl(c) || isspace(c) ||
       c == ';'   ||
       c == '*'   || c == '?'   ||
       c == '&'   || c == '|'   ||
       c == '"'   || c == '>'   ||
       c == '<'   || c == '\''  ||
       c == '['   || c == ']'   ||
       c == '('   || c == ')'   ||
       c == '{'   || c == '}'   ||
       c == '!'   || (c & 128) != 0 ) return 0 ;
   return 1 ;
}

/*----------------------------------------------------------------------*/

int THD_filename_fix( char *name )  /* 04 Feb 2010 */
{
   int ll , ii , nfix ;

   if( name == NULL ) return -1 ;
   ll = strlen( name ) ; if( ll == 0 ) return -1 ;
   for( nfix=ii=0 ; ii < ll ; ii++ ){
     if( !THD_character_ok(name[ii]) ){ name[ii] = '_' ; nfix++ ; }
   }
   return nfix ;
}

/*----------------------------------------------------------------------*/
/*! Check if a filename is OK - that is, has no crummy characters.

  The filename can have a '/' in it.  To insist that there be not '/',
  use THD_filename_pure().
  The list of crummy characters can be inferred from the source code.
*/

int THD_filename_ok( char *name )  /* 24 Apr 1997 */
{
   int ll , ii ;

   if( name == NULL ) return 0 ;
   ll = strlen( name ) ; if( ll == 0 ) return 0 ;
   if (ll > 6 && strstr(name, "3dcalc") == name) {
      return 1; /* have a 3dcalc command, let it be*/
   }
   if( AFNI_yesenv("AFNI_ALLOW_ARBITRARY_FILENAMES") ) return 1 ; /* 08 Apr 2009 */
   for( ii=0 ; ii < ll ; ii++ )
      if( iscntrl(name[ii]) || isspace(name[ii]) ||
                               name[ii] == ';'   ||
          name[ii] == '*'   || name[ii] == '?'   ||
          name[ii] == '&'   || name[ii] == '|'   ||
          name[ii] == '"'   || name[ii] == '>'   ||
          name[ii] == '<'   || name[ii] == '\''  ||
          name[ii] == '['   || name[ii] == ']'   ||
          name[ii] == '('   || name[ii] == ')'   ||
          name[ii] == '{'   || name[ii] == '}'   ||
          name[ii] == '!'   || (name[ii] & 128) != 0 ) return 0 ;

   return 1 ;
}

/*--------------------------------------------------------------------*/
/*! Check if a filename is pure - no crummy characters, no '/'. */

int THD_filename_pure( char *name )  /* 28 Feb 2001 */
{
   int ii ;

   ii = THD_filename_ok( name ) ;
   if( ii ) ii = (strstr(name,"/") == NULL) ;
   return ii ;
}

/*--------------------------------------------------------------*/

#undef FNAME
#undef FSTYP
#undef BSIZE
#undef BFREE

#if defined(DARWIN) || defined(FreeBSD)  /* Mac or BSD */
#  include <sys/param.h>
#  include <sys/mount.h>
#  define FNAME(a,b) statfs(a,b)
#  define FSTYP      statfs
#  define BSIZE      f_bsize
#  define BFREE      f_bavail
#elif defined(LINUX)                     /* Linux */
#  include <sys/vfs.h>
#  define FNAME(a,b) statfs(a,b)
#  define FSTYP      statfs
#  define BSIZE      f_bsize
#  define BFREE      f_bavail
#elif defined(SOLARIS) || defined(SGI)   /* Sun or SGI */
#  include <sys/types.h>
#  include <sys/statvfs.h>
#  define FNAME(a,b) statvfs64(a,b)
#  define FSTYP      statvfs64
#  define BSIZE      f_bsize
#  define BFREE      f_bavail
#endif

/*--------------------------------------------------------------*/
/*! Get free space (in megabytes) on a disk partition.
    Return value is -1 if can't be determined.
----------------------------------------------------------------*/

int THD_freemegabytes( char *pathname )
{
#ifdef FNAME
   int ii ; struct FSTYP buf ;
   if( pathname == NULL || *pathname == '\0' ) return -1 ;
   ii = FNAME( pathname , &buf ) ;
   if( ii ) return -1 ;
   ii = (int)((double)(buf.BFREE) * (double)(buf.BSIZE) / (1024.0*1024.0)) ;
   return ii ;
#else
   return -1 ;
#endif
}

/*----------------------------------------------------------------------*/
/*! Check a list of dataset names for duplicates. Return value is number
    of duplicates found.
     - ns   = number of strings
     - sar  = string array
     - flag = 1 to print warnings, 0 to be silent
------------------------------------------------------------------------*/

int THD_check_for_duplicates( int ns , char **sar , int flag )
{
   int verb = (flag & 1) != 0 ;
   int ii,jj,li,lj,nw=0 ; char *di, *dj ;

ENTRY("THD_check_for_duplicates") ;

   if( sar == NULL ) RETURN(0) ;

   for( ii=0 ; ii < ns-1 ; ii++ ){

     if( sar[ii] == NULL ) continue ;
     di = strdup(sar[ii]  ) ; li = strlen(di) ;
          if( strcmp(di+li-5,".HEAD"   ) == 0 ) di[li-5] = '\0' ;
     else if( strcmp(di+li-5,".BRIK"   ) == 0 ) di[li-5] = '\0' ;
     else if( strcmp(di+li-8,".BRIK.gz") == 0 ) di[li-8] = '\0' ;
     else if( strcmp(di+li-7,".nii.gz" ) == 0 ) di[li-3] = '\0' ;
     else if( strcmp(di+li-1,"."       ) == 0 ) di[li-1] = '\0' ;

     for( jj=ii+1 ; jj < ns ; jj++ ){

       if( sar[jj] == NULL ) continue ;
       dj = strdup(sar[jj]) ; lj = strlen(dj) ;
            if( strcmp(dj+lj-5,".HEAD"   ) == 0 ) dj[lj-5] = '\0' ;
       else if( strcmp(dj+lj-5,".BRIK"   ) == 0 ) dj[lj-5] = '\0' ;
       else if( strcmp(dj+lj-8,".BRIK.gz") == 0 ) dj[lj-8] = '\0' ;
       else if( strcmp(dj+lj-7,".nii.gz" ) == 0 ) dj[lj-3] = '\0' ;
       else if( strcmp(dj+lj-1,"."       ) == 0 ) dj[lj-1] = '\0' ;

       if( strcmp(di,dj) == 0 ){
         nw++ ;
         if( verb ) WARNING_message("Datasets '%s' and '%s' are the same?!?",
                                     sar[ii] , sar[jj] ) ;
       }
       free(dj) ;
     }
     free(di) ;
   }

   RETURN(nw) ;
}
