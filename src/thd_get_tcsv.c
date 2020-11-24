/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*=============================================================================*/
/*** Adapted from THD_get_many_timeseries() for *.1D files ***/

/***  Read all *.csv and *.tsv files from a list of directories ***/

NI_ELARR * THD_get_many_tcsv( THD_string_array *dlist )
{
   int id , ii , ndir ;
   NI_ELARR *outar=NULL, *tmpar=NULL ;
   char *epath , *eee ;
   char  efake[] = "./" ;
   THD_string_array *qlist ; /* 02 Feb 2002 */

ENTRY("THD_get_many_tcsv") ;

   /*----- sanity check and initialize -----*/

                       epath = my_getenv( "AFNI_TSPATH" ) ;
   if( epath == NULL ) epath = my_getenv( "AFNI_TS_PATH" ) ;
   if( epath == NULL ) epath = efake ;

   ndir = (dlist != NULL) ? dlist->num : 0 ;

   if( ndir == 0 && epath == NULL ) RETURN( outar ) ;

   INIT_ELARR( outar ) ; /* output array */
   INIT_SARR( qlist ) ;  /* list of directories */

   /*----- for each input directory, find all *.1D files -----*/

   for( id=0 ; id < ndir ; id++ ){

      if( THD_forbidden_directory(dlist->ar[id]) ) continue ;

      ADDTO_SARR(qlist,dlist->ar[id]) ; /* save this name for later use */

      tmpar = THD_get_all_tcsv( dlist->ar[id] ) ; /* read files from */
      if( tmpar == NULL ) continue ;              /* this directory */

      for( ii=0 ; ii < tmpar->num ; ii++ )  /* move data to output array */
        ADDTO_ELARR( outar , tmpar->elarr[ii] ) ;

      FREE_ELARR(tmpar) ;  /* don't need this no more */
   }

   /*----- also do directories in environment path, if any -----*/

   if( epath != NULL ){
      int epos = 0 , ll = strlen(epath) ;
      char *elocal ;
      char  ename[THD_MAX_NAME] ;

      /* copy path list into local memory */

      elocal = (char *) malloc( sizeof(char) * (ll+2) ) ;
      if( elocal == NULL ){
         fprintf(stderr,
                "\n*** THD_get_many_tcsv malloc failure - is memory full? ***\n");
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
         if( THD_forbidden_directory(ename) ) continue ; /* 18 Sep 2020 */

         /* 02 Feb 2002: check if scanned this directory before */

         for( ii=0 ; ii < qlist->num ; ii++ )
            if( THD_equiv_files(qlist->ar[ii],ename) ) break ;
         if( ii < qlist->num ) continue ;  /* skip to end of do loop */
         ADDTO_SARR(qlist,ename) ;

         tmpar = THD_get_all_tcsv( ename ) ; /* read all files from this directory */
         if( tmpar != NULL ){
           for( ii=0 ; ii < tmpar->num ; ii++ )   /* move data to output array */
             ADDTO_ELARR( outar , tmpar->elarr[ii] ) ;

           FREE_ELARR(tmpar) ;                    /* don't need this no more */
         }
      } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

      free(elocal) ;
   }

   if( ELARR_COUNT(outar) == 0 ) DESTROY_ELARR(outar) ;

   DESTROY_SARR(qlist) ;
   RETURN( outar ) ;
}

/*---------------------------------------------------*/
/*  Read all *.tsv and *.csv files from directory   */
/*---------------------------------------------------*/

NI_ELARR * THD_get_all_tcsv( char *dname )
{
   int ir , ii ;
   char *fname , * tname ;
   float *far ;
   char *cmd , *flist ;
   NI_str_array *qsar=NULL ;
   NI_element *outim , *flim=NULL ;
   NI_ELARR   *outar ;

   unsigned long max_fsize ;  /* 20 Jul 2004: max 1D file size to load */

ENTRY("THD_get_all_tcsv") ;

   max_fsize = (unsigned long) AFNI_numenv( "AFNI_MAX_1DSIZE" ) ;
   if( max_fsize == 0 ) max_fsize = 123*1024 ;
   if( max_fsize == 1 ) RETURN(NULL) ;

   /*----- sanity check and initialize -----*/

   if( dname == NULL || strlen(dname) == 0 ) RETURN(NULL) ;

   /*----- find all *.csv and *.tsv files -----*/

   ii  = strlen(dname) ;
   cmd = (char *) malloc(sizeof(char)*3*(ii+64)) ;
   sprintf( cmd , "find %s -maxdepth 4 -type f -name '*.[ctCT][sS][vV]'" , dname ) ;

   flist = THD_suck_pipe( cmd ) ;
   free(cmd) ;
   if( flist == NULL || strlen(flist) < 4 ){
     if( flist != NULL ) free(flist) ;
     RETURN(NULL) ;
   }

   /* break output into discrete strings */

   qsar = NI_decode_string_list( flist , ";" ) ;
   if( qsar == NULL || qsar->num == 0 ){    /* should never happen */
     free(flist) ; RETURN(NULL) ;
   }

   INIT_ELARR( outar ) ;

   /* try to read each file */

   for( ir=0 ; ir < qsar->num ; ir++ ){
     fname = qsar->str[ir] ; if( fname == NULL ) continue ;

     /* too big? */
     if( THD_filesize(fname) > max_fsize ) continue ;

     if( STRING_HAS_SUFFIX_CASE(fname,".csv") ){        /* .csv */
       flim = THD_read_csv( fname ) ;
     } else if( STRING_HAS_SUFFIX_CASE(fname,".tsv") ){ /* .tsv */
       flim = THD_read_tsv( fname ) ;
     } else {
       flim = NULL ;
     }

     /* if it worked, and it's OK, add it to the output */

     if( flim != NULL && NI_element_type(flim) == NI_ELEMENT_TYPE ){
       if( flim->vec_num <= 0 || flim->vec_len <= 0 ){
         NI_free_element(flim) ; flim = NULL ; /* nothing inside?! */
       } else {
         tname = THD_trailname(fname,1) ;
         flim->filename = strdup(tname) ;
         ADDTO_ELARR( outar , flim ) ;
       }
     } else {
       NI_free_element(flim) ; flim = NULL ;   /* should not happen */
     }
   }

   /* return to the caller */

   NI_delete_str_array(qsar) ;
   if( ELARR_COUNT(outar) == 0 ) DESTROY_ELARR(outar) ;
   RETURN(outar) ;
}
