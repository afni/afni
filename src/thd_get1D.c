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

   /*----- sanity check and initialize -----*/

ENTRY("THD_get_many_timeseries") ;

                       epath = getenv( "AFNI_TSPATH" ) ;
   if( epath == NULL ) epath = getenv( "AFNI_TS_PATH" ) ; /* 07 Oct 1996 */
   if( epath == NULL ) epath = efake ;                    /* 07 Oct 1996 */

   ndir = (dlist != NULL) ? dlist->num : 0 ;

   if( ndir == 0 && epath == NULL ) return NULL ;

   INIT_IMARR( outar ) ;

   /*----- for each input directory, find all *.1D files -----*/

   for( id=0 ; id < ndir ; id++ ){

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
         fprintf(stderr,"\n*** THD_get_many_timeseries malloc failure ***\n") ;
         exit(1) ;
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

         ii = SARR_find_string( dlist , ename ) ;  /* skip this directory */
         if( ii >= 0 ) continue ;                  /* if already was scanned */

         /* 09 Sep 1998: check for file equivalence as well */

         if( dlist != NULL ){
            for( ii=0 ; ii < dlist->num ; ii++ )
               if( THD_equiv_files(dlist->ar[ii],ename) ) break ;

            if( ii < dlist->num ) continue ;  /* skip to end of do loop */
         }

         eee = strstr( elocal , ename ) ;
         if( eee != NULL && (eee-elocal) < epos-id ) continue ;

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

   return outar ;
}

/*---------------------------------------------------*/
/*  Read all *.1D (time series) files from directory */
/*---------------------------------------------------*/

MRI_IMARR * THD_get_all_timeseries( char * dname )
{
   THD_string_array * flist , * rlist ;
   int ir , ll , ii ;
   char * fname , * tname ;
   float * far ;
   MRI_IMARR * outar ;
   MRI_IMAGE * outim , * flim ;

   /*----- sanity check and initialize -----*/

ENTRY("THD_get_all_timeseries") ;

   if( dname == NULL || strlen(dname) == 0 ) return NULL ;
   INIT_IMARR( outar ) ;

   /*----- find all *.1D files -----*/

   flist = THD_get_all_filenames( dname ) ;
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

         outim = mri_read_ascii( fname ) ;
         if( outim != NULL ){
            if( outim->kind != MRI_float ){
               flim = mri_to_float(outim) ;
               mri_free(outim) ; outim = flim ;
            }
            flim = mri_transpose(outim) ; mri_free(outim) ;
            far = MRI_FLOAT_PTR(flim) ;
            for( ii=0 ; ii < flim->nvox ; ii++ )
               if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;

            tname = THD_trailname(fname,1) ;
            mri_add_name( tname , flim ) ;
            ADDTO_IMARR( outar , flim ) ;

#ifdef THD_DEBUG
{ char str[256] ;
  sprintf(str,"added %s: nx=%d ny=%d", flim->name,flim->nx,flim->ny ) ;
  STATUS(str) ; }
#endif

         }
      }
   }

   DESTROY_SARR(rlist) ;

   if( IMARR_COUNT(outar) == 0 ) DESTROY_IMARR(outar) ;

   return outar ;
}
