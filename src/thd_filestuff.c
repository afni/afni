/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"

int THD_is_file( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFREG) != 0 ; return ii ;
}

int THD_is_symlink( char * pathname )  /* 03 Mar 1999 */
{
   char buf[32] ; int ii ;

   ii = readlink( pathname , buf , 32 ) ;
   return (ii > 0) ;
}

long THD_filesize( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

int THD_is_directory( char * pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFDIR) != 0 ; return ii ;
}

int THD_is_executable( char * pathname )  /* 26 Jun 2001 */
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IXOTH) != 0 ; return ii ;
}

int THD_equiv_files( char * path1 , char * path2 )
{
   static struct stat buf1 , buf2 ; int ii ;

   if( path1 == NULL || path2 == NULL ) return -1 ;
   ii = stat( path1 , &buf1 ) ; if( ii != 0 ) return -1 ;
   ii = stat( path2 , &buf2 ) ; if( ii != 0 ) return -1 ;

   ii = (buf1.st_dev == buf2.st_dev) && (buf1.st_ino == buf2.st_ino) ;
   return ii ;
}

/*-----------------------------------------------------------------
   Routine to find a 'trailing name' in a pathname.  For example,
   for fname = "/bob/cox/is/the/author/of/AFNI",
     the lev=0 trailing name is "AFNI",
     the lev=1 trailing name is "of/AFNI",
     the lev=2 trailing name is "author/of/AFNI", and so on.
   That is, "lev" is the number of directory names above the
   last name to keep.  The pointer returned is to some place
   in the middle of fname.
-------------------------------------------------------------------*/

char * THD_trailname( char * fname , int lev )
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

/*-------------------------------------------------------------------
   Check if a filename is OK -- that is, has no crummy characters.
   28 Feb 2001: removed '/' from the illegal list
---------------------------------------------------------------------*/

int THD_filename_ok( char * name )  /* 24 Apr 1997 */
{
   int ll , ii ;

   if( name == NULL ) return 0 ;
   ll = strlen( name ) ; if( ll == 0 ) return 0 ;

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
          name[ii] == '!'   || name[ii] >= 127     ) return 0 ;

   return 1 ;
}

int THD_filename_pure( char * name )  /* 28 Feb 2001 */
{
   int ii ;

   ii = THD_filename_ok( name ) ;
   if( ii ){
      ii = (strstr(name,"/") == NULL) ;
   }
   return ii ;
}
