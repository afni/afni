/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*--------------------------------------------------------------
   SARR: string array routines (not easily coded as macros)

     SARR_find_string: return index of string in the array
     SARR_find_substring: return index of string that has substring in it
     ci : if 1 then search is case instensitive
   If these return value < 0, string or substring not found!
----------------------------------------------------------------*/

int SARR_find_string( THD_string_array * sar , char * str, byte ci )
{
   return SARR_lookfor_string( sar , str , 0 , ci) ;
}

int SARR_find_substring( THD_string_array * sar , char * str, byte ci )
{
   return SARR_lookfor_substring( sar , str , 0 , ci) ;
}

int SARR_lookfor_string( THD_string_array * sar , char * str ,
                         int nstart , byte ci )
{
   int ii ;

   if( sar == NULL || str == NULL ) return -1 ;

   for( ii=nstart ; ii < sar->num ; ii++ ){
      if( sar->ar[ii] != NULL &&
          ( (ci && !strcasecmp(sar->ar[ii],str)) || !strcmp(sar->ar[ii],str) ) )
         return ii ;
   }
   return -1 ;
}

int SARR_lookfor_substring( THD_string_array * sar , char * sub ,
                            int nstart , byte ci )
{
   int ii ;

   if( sar == NULL || sub == NULL ) return -1 ;

   for( ii=nstart ; ii < sar->num ; ii++ ){
      if( sar->ar[ii] != NULL &&
          ( (ci && strcasestr(sar->ar[ii],sub)) || strstr(sar->ar[ii],sub) ) )
         return ii ;
   }
   return -1 ;
}

/*--------------------------------------------------------------
   routines to return an alphabetized list of all the entries
   in a directory that don't begin with '.'

   01 Feb 1998: modified to avoid use of scandir routine.
----------------------------------------------------------------*/

#ifndef DONT_USE_SCANDIR
#  ifdef SCANDIR_WANTS_CONST
      int THD_select_dirent( const struct dirent * dp )
#  else
      int THD_select_dirent( struct dirent * dp )
#  endif
      {
         if( dp == NULL || dp->d_name[0] == '\0' || dp->d_name[0] == '.' )
            return 0 ;
         return 1 ;
      }
#endif

THD_string_array * THD_get_all_filenames( char * dirname )
{
#ifndef DONT_USE_SCANDIR
   struct dirent ** dplist=NULL ;
#endif
   int nfiles , dlen , ii , n_fname , max_fname ;
   THD_string_array * star ;
   char * total_dirname , * total_fname ;
   char ** gname=NULL ;

ENTRY("THD_get_all_filenames") ;

   if( dirname == NULL || (dlen=strlen(dirname)) == 0 ) RETURN( NULL );
   if( ! THD_is_directory(dirname) )                    RETURN( NULL );

   total_dirname = (char*)RwcMalloc( dlen+4 ) ;
   strcpy( total_dirname , dirname ) ;
   if( total_dirname[dlen-1] != '/' ){
      total_dirname[dlen]   = '/' ;     /* add a slash */
      total_dirname[++dlen] = '\0' ;
   }

#ifdef DONT_USE_SCANDIR
   total_dirname[dlen]   = '*' ;                            /* add wildcard */
   total_dirname[++dlen] = '\0' ;
   MCW_warn_expand(0) ;
if(PRINT_TRACING){
 char str[256]; sprintf(str,"MCW_file_expand(%s)",total_dirname); STATUS(str);
}
   MCW_file_expand( 1, &total_dirname, &nfiles, &gname ) ;  /* find files */
#else
   nfiles = scandir( dirname ,
                     &dplist ,
                     THD_select_dirent ,
                     alphasort ) ;
#endif

   if( nfiles < 1 ){
       myRwcFree( total_dirname ) ;
#ifdef DONT_USE_SCANDIR
       if( gname != NULL ) free(gname) ;
#else
       if( dplist != NULL ) free(dplist) ;
#endif
       RETURN( NULL );
   }

   INIT_SARR( star ) ;

#ifndef DONT_USE_SCANDIR
   max_fname   = dlen+64 ;
   total_fname = (char*)RwcMalloc( max_fname ) ;
#endif

   for( ii=0 ; ii < nfiles ; ii++ ){
#ifdef DONT_USE_SCANDIR
      ADDTO_SARR( star , gname[ii] ) ;
#else
      n_fname = dlen + strlen( dplist[ii]->d_name ) + 4 ;
      if( n_fname > max_fname ){
         total_fname = AFREALL(total_fname, char, n_fname ) ;
         max_fname   = n_fname ;
      }
      strcpy( total_fname , total_dirname ) ;
      strcat( total_fname , dplist[ii]->d_name ) ;
      ADDTO_SARR( star , total_fname ) ;
      free( dplist[ii] ) ;
#endif
   }

   myRwcFree( total_dirname ) ;
#ifdef DONT_USE_SCANDIR
   MCW_free_expand( nfiles , gname ) ;
#else
   myRwcFree( total_fname ) ;
   free( dplist ) ;
#endif
   RETURN( star );
}

/*------------------------------------------------------------------
  Returns a list of all subdirectories under the input,
  recursively to level "lev", including the input directory.
--------------------------------------------------------------------*/

THD_string_array * THD_get_all_subdirs( int lev , char * dirname )
{
   int ii , jj , dlen ;
   THD_string_array * star , * flist , * dlist ;
   char * total_dirname ;

ENTRY("THD_get_all_subdirs") ;

   if( dirname == NULL || (dlen=strlen(dirname)) == 0 ) RETURN( NULL );

   total_dirname = (char*)RwcMalloc( dlen+2 ) ;
   strcpy( total_dirname , dirname ) ;
   if( total_dirname[dlen-1] != '/' ){
      total_dirname[dlen]   = '/' ;
      total_dirname[++dlen] = '\0' ;
   }

   INIT_SARR( star ) ;
   ADDTO_SARR( star , total_dirname ) ;

   /** want only this level? **/

   if( lev <= 0 ) RETURN( star );

   /** must want deeper levels **/

   flist = THD_get_all_filenames( total_dirname ) ;
   myRwcFree(total_dirname) ;

   if( flist == NULL ) RETURN( star );
   if( flist->num == 0 ){ DESTROY_SARR(flist) ; RETURN( star ); }

   dlist = THD_extract_directories( flist ) ;
   DESTROY_SARR(flist) ;
   if( dlist == NULL ) RETURN( star );
   if( dlist->num == 0 ){ DESTROY_SARR(dlist) ; RETURN( star ); }

   for( ii=0 ; ii < dlist->num ; ii++ ){
      flist = THD_get_all_subdirs( lev-1 , dlist->ar[ii] ) ;
      if( flist == NULL ) continue ;
      for( jj=0 ; jj < flist->num ; jj++ ){
         ADDTO_SARR( star , flist->ar[jj] ) ;
      }
      DESTROY_SARR(flist) ;
   }

   DESTROY_SARR(dlist) ;
   RETURN( star );
}

/*-------------------------------------------------------
   take a list of filenames and extract a new list
   consisting only of regular files
---------------------------------------------------------*/

THD_string_array * THD_extract_regular_files( THD_string_array * star_in )
{
   THD_string_array * star_out ;
   int ii ;

ENTRY("THD_extract_regular_files") ;

   if( star_in == NULL || star_in->num <= 0 ) RETURN( NULL );

   INIT_SARR(star_out) ;

   for( ii=0 ; ii < star_in->num ; ii++ ){
      if( THD_is_file(star_in->ar[ii]) )
         ADDTO_SARR( star_out , star_in->ar[ii] ) ;
   }

   if( star_out->num == 0 ) DESTROY_SARR(star_out) ;
   RETURN( star_out );
}

/*-------------------------------------------------------
   take a list of filenames and extract a new list
   consisting only of directories
---------------------------------------------------------*/

THD_string_array * THD_extract_directories( THD_string_array * star_in )
{
   THD_string_array * star_out ;
   int ii ;

ENTRY("THD_extract_directories") ;

   if( star_in == NULL || star_in->num <= 0 ) RETURN( NULL );

   INIT_SARR(star_out) ;

   for( ii=0 ; ii < star_in->num ; ii++ ){
      if( THD_is_directory(star_in->ar[ii]) )
         ADDTO_SARR( star_out , star_in->ar[ii] ) ;
   }

   if( star_out->num == 0 ) DESTROY_SARR(star_out) ;
   RETURN( star_out );
}

/*-------------------------------------------------------------------
   Take a list of filenames, convert them into "real" names
   (excising things like ../ and symbolic links), and then
   cast out duplicates.  09 Sep 1998 -- RWCox.
---------------------------------------------------------------------*/

THD_string_array * THD_normalize_flist( THD_string_array *star_in )
{
   THD_string_array *star_out , *star_qqq ;
   static char rpath[RPMAX] ;  /* 04 Mar 2009 [RWC]: 2048 ==> RPMAX */
   char *rp ;
   int ii , jj , nleft , skip_realpath=0 ;

ENTRY("THD_normalize_flist") ;

   if( star_in == NULL || star_in->num <= 0 ) RETURN( NULL );

   skip_realpath = AFNI_yesenv("AFNI_NOREALPATH") ;

   INIT_SARR(star_out) ;

   for( ii=0 ; ii < star_in->num ; ii++ ){
      if( skip_realpath ) rp = star_in->ar[ii] ;
      else                rp = realpath( star_in->ar[ii] , rpath ) ;

      if( rp == NULL && strchr(star_in->ar[ii],'+') != NULL ){ /* 10 Sep 2013 */
        char *sp ;
        jj = strlen(star_in->ar[ii]) ; sp = malloc(jj+32) ;
        strcpy(sp,star_in->ar[ii]) ;
        if( sp[jj-1] != '.' ) strcat(sp,".") ;
        strcat(sp,"HEAD") ;
        rp = realpath( sp , rpath ) ;
        free(sp) ;
      }

      if( rp != NULL ) ADDTO_SARR( star_out , rp ) ;
   }

   if( star_out->num == 0 ){ DESTROY_SARR(star_out) ; RETURN( NULL ); }

   nleft = 0 ;
   for( ii=0 ; ii < star_out->num ; ii++ ){
      rp = star_out->ar[ii] ;
      if( rp != NULL ){
         nleft++ ; jj = ii ;
         while( jj >= 0 ){
            jj = SARR_lookfor_string( star_out , rp , jj+1 , 0) ;
            if( jj >= 0 ) REMOVEFROM_SARR(star_out,jj) ;
         }

         for( jj=ii+1 ; jj < star_out->num ; jj++ ){
            if( THD_equiv_files(rp,star_out->ar[jj]) )
               REMOVEFROM_SARR(star_out,jj) ;
         }
      }
   }

   if( nleft == 0 ){ DESTROY_SARR(star_out) ; RETURN( NULL ); }

   if( nleft == star_out->num ) RETURN( star_out );

   INIT_SARR(star_qqq) ;
   for( ii=0 ; ii < star_out->num ; ii++ ){
      rp = star_out->ar[ii] ;
      if( rp != NULL ) ADDTO_SARR(star_qqq,rp) ;
   }

#if 0
fprintf(stderr,"\nTHD_normalize_flist: in=%d out=%d qqq=%d\n",
        star_in->num , star_out->num , star_qqq->num ) ;
#endif

   DESTROY_SARR(star_out) ; RETURN( star_qqq );
}

/*---------------------------------------------------------------
  Added 27 Jan 2000
-----------------------------------------------------------------*/

THD_string_array * THD_get_wildcard_filenames( char * pat )
{
   int nfiles , ii ;
   THD_string_array * star ;
   char ** gname=NULL ;

ENTRY("THD_get_wildcard_filenames") ;

   if( pat == NULL || strlen(pat) == 0 ) RETURN( NULL );

   MCW_warn_expand(0) ;
   MCW_file_expand( 1, &pat, &nfiles, &gname ) ;  /* find files */

   if( nfiles < 1 ){
       if( gname != NULL ) free(gname) ;
       RETURN( NULL );
   }

   INIT_SARR( star ) ;

   for( ii=0 ; ii < nfiles ; ii++ ){
      ADDTO_SARR( star , gname[ii] ) ;
   }

   MCW_free_expand( nfiles , gname ) ;
   RETURN( star );
}
