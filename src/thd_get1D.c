/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*=============================================================================*/

/***  Read all *.1D (time series) files from a list of directories ***/

MRI_IMARR * THD_get_many_timeseries( THD_string_array * dlist )
{
   int id , ii , ndir ;
   MRI_IMARR * outar , * tmpar ;
   char * epath , * eee ;
   char   efake[] = "./" ;
   THD_string_array *qlist ; /* 02 Feb 2002 */

ENTRY("THD_get_many_timeseries") ;

   /*----- sanity check and initialize -----*/

                       epath = my_getenv( "AFNI_TSPATH" ) ;
   if( epath == NULL ) epath = my_getenv( "AFNI_TS_PATH" ) ; /* 07 Oct 1996 */
   if( epath == NULL ) epath = efake ;                      /* 07 Oct 1996 */

   ndir = (dlist != NULL) ? dlist->num : 0 ;

   if( ndir == 0 && epath == NULL ) RETURN( outar ) ;

   INIT_IMARR( outar ) ;
   INIT_SARR( qlist ) ;

   /*----- for each input directory, find all *.1D files -----*/

   for( id=0 ; id < ndir ; id++ ){

      ADDTO_SARR(qlist,dlist->ar[id]) ;

      tmpar = THD_get_all_timeseries( dlist->ar[id] ) ;
      if( tmpar == NULL ) continue ;

      for( ii=0 ; ii < tmpar->num ; ii++ )  /* move images to output array */
         ADDTO_IMARR( outar , tmpar->imarr[ii] ) ;

      FREE_IMARR(tmpar) ;  /* don't need this no more */
   }

   /*----- also do directories in environment path, if any -----*/

   if( epath != NULL ){
      int epos =0 , ll = strlen(epath) ;
      char * elocal ;
      char ename[THD_MAX_NAME] ;

      /* copy path list into local memory */

      elocal = (char *) malloc( sizeof(char) * (ll+2) ) ;
      if( elocal == NULL ){
         fprintf(stderr,
                "\n*** THD_get_many_timeseries malloc failure - is memory full? ***\n");
         EXIT(1) ;
      }
      strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

      /* replace colons with blanks */

      for( ii=0 ; ii < ll ; ii++ )
         if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

      /* extract blank delimited strings,
         use as directory names to get timeseries files */

      do{
         ii = sscanf( elocal+epos , "%s%n" , ename , &id ) ;
         if( ii < 1 ) break ;  /* no read --> end of work */
         epos += id ;          /* epos = char after last one scanned */

         ii = strlen(ename) ;                         /* make sure name has */
         if( ename[ii-1] != '/' ){                    /* a trailing '/' on it */
            ename[ii]  = '/' ; ename[ii+1] = '\0' ;
         }

         if( !THD_is_directory(ename) ) continue ;  /* 21 May 2002 - rcr */

         /* 02 Feb 2002: check if scanned this directory before */

         for( ii=0 ; ii < qlist->num ; ii++ )
            if( THD_equiv_files(qlist->ar[ii],ename) ) break ;
         if( ii < qlist->num ) continue ;  /* skip to end of do loop */
         ADDTO_SARR(qlist,ename) ;

         tmpar = THD_get_all_timeseries( ename ) ; /* read this directory */
         if( tmpar != NULL ){
            for( ii=0 ; ii < tmpar->num ; ii++ )   /* move images to output array */
               ADDTO_IMARR( outar , tmpar->imarr[ii] ) ;

            FREE_IMARR(tmpar) ;                    /* don't need this no more */
         }
      } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

      free(elocal) ;
   }

   if( IMARR_COUNT(outar) == 0 ) DESTROY_IMARR(outar) ;

   DESTROY_SARR(qlist) ;
   RETURN( outar ) ;
}

/*---------------------------------------------------*/
/*  Read all *.1D (time series) files from directory */
/*---------------------------------------------------*/

#define NEWWAY

MRI_IMARR * THD_get_all_timeseries( char * dname )
{
   THD_string_array * flist , * rlist ;
   int ir , ll , ii ;
   char * fname , * tname ;
   float * far ;
   MRI_IMARR * outar ;
   MRI_IMAGE * outim , * flim ;

#ifdef NEWWAY
   char * pat ;
#endif

   unsigned long max_fsize ;  /* 20 Jul 2004: max 1D file size to load */

   max_fsize = (unsigned long) AFNI_numenv( "AFNI_MAX_1DSIZE" ) ;
   if( max_fsize == 0 ) max_fsize = 123*1024 ;

   /*----- sanity check and initialize -----*/

   if( dname == NULL || strlen(dname) == 0 ) return NULL ;
   INIT_IMARR( outar ) ;

   /*----- find all *.1D files -----*/

#ifdef NEWWAY
   ii  = strlen(dname) ;
   pat = (char *) malloc(sizeof(char)*(ii+8)) ;
   strcpy(pat,dname) ;
   if( pat[ii-1] != '/' ) strcat(pat,"/") ;
   strcat(pat,"*.1D*") ;
   flist = THD_get_wildcard_filenames( pat ) ;
   free(pat) ;
#else
   flist = THD_get_all_filenames( dname ) ;
#endif

   if( flist == NULL || flist->num <= 0 ){
      DESTROY_SARR(flist) ;
      DESTROY_IMARR(outar) ;
      return NULL ;
   }

   rlist = THD_extract_regular_files( flist ) ;
   DESTROY_SARR(flist) ;
   if( rlist == NULL || rlist->num <= 0 ){
      DESTROY_SARR(rlist) ;
      DESTROY_IMARR(outar) ;
      return NULL ;
   }

   for( ir=0 ; ir < rlist->num ; ir++ ){
      fname = rlist->ar[ir] ; if( fname == NULL ) continue ;

      ll = strlen(fname) - 3 ; if( ll < 1 ) continue ;

      if( strcmp(fname+ll,".1D")==0 ||
          strcmp(fname+ll,"1Dx")==0 ||
          strcmp(fname+ll,"1Dv")==0   ){

         if( THD_filesize(fname) > max_fsize ) continue ;  /* 20 Jul 2004 */

         flim = mri_read_1D( fname ) ;
         if( flim != NULL ){
            far = MRI_FLOAT_PTR(flim) ;
            for( ii=0 ; ii < flim->nvox ; ii++ )
               if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;

            tname = THD_trailname(fname,1) ;
            mri_add_name( tname , flim ) ;
            ADDTO_IMARR( outar , flim ) ;
         }
      }
   }

   DESTROY_SARR(rlist) ;

   if( IMARR_COUNT(outar) == 0 ) DESTROY_IMARR(outar) ;

   return outar ;
}
