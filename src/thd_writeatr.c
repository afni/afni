/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------*/

/* Help for retrying to open a file for writing.  If it seems busy, give
   the system time before catastrophic failure.                         */
#undef  LOCAL_MAX_LOCK_TRIES
#define LOCAL_MAX_LOCK_TRIES    6
static int has_locking_errno(void) {
   return( errno == EDEADLK     ||
           errno == EAGAIN      ||
           errno == EWOULDBLOCK ||
           errno == EBUSY );
}


Boolean THD_write_atr( THD_datablock *blk )
{
   THD_diskptr *dkptr ;
   int ia , code , ii ;
   ATR_any *atr_any ;
   Boolean good = True ;
   FILE *header_file ;

ENTRY("THD_write_atr") ;

   /*--- sanity checks ---*/

   if( ! ISVALID_DATABLOCK(blk) ) RETURN( False );

   dkptr = blk->diskptr ;
   if( ! ISVALID_DISKPTR(dkptr) || strlen(dkptr->header_name) == 0 ) RETURN( False );

   if( DBLK_IS_MINC(blk)    ) RETURN( False ); /* 29 Oct 2001 */
   if( DBLK_IS_ANALYZE(blk) ) RETURN( False ); /* 27 Aug 2002 */
   if( DBLK_IS_NIFTI(blk)   ) RETURN( False ); /* 28 Aug 2003 */
   if( DBLK_IS_CTFMRI(blk)  ) RETURN( False );
   if( DBLK_IS_CTFSAM(blk)  ) RETURN( False );
   if( DBLK_IS_MPEG(blk)    ) RETURN( False );

   /** 01 Jun 2005: perhaps write in the new NIML format? **/

   if( AFNI_yesenv("AFNI_WRITE_NIML") ){
     return THD_write_nimlatr( blk ) ;
   }

   /** Write the old AFNI format **/

   header_file = fopen( dkptr->header_name , "w" ) ;

   /* allow more chances for those with large file systems */
   /* nap and try again          [16 Aug 2019 rickr/dglen] */
   if( header_file == NULL && has_locking_errno() ) {
      int tries, nap_time = 1;
      for( tries = 0; tries < LOCAL_MAX_LOCK_TRIES; tries++ ) {
         nap_time *= 2; /* sleep 2^(n+1) s, i.e. up to 1 minute, total of 2 */
         if( tries == 0 ) {
            fprintf(stderr,"** E-xxx open failure for file %s, "
                           "will nap and try %d more times\n",
                           dkptr->header_name, LOCAL_MAX_LOCK_TRIES);
            fprintf(stderr,"Kris K, is that you?\n");
         }
         fprintf(stderr,"-- napping for %d s...\n", nap_time);
         NI_sleep(nap_time);   /* nap time! */
         header_file = fopen( dkptr->header_name , "w" ) ;
         if( header_file || ! has_locking_errno() )
            break;
      }
   }

   if( header_file == NULL ){
      fprintf(stderr,
              "*** ERROR: failed (%d) to open file %s for attribute writing;\n"
              "         - Do you have permission to write to this disk?\n"
              "         - Is the disk full?\n" ,
              errno, dkptr->header_name) ;
      if (errno == ENAMETOOLONG) {
         fprintf(stderr,
           "    Well Phil, a closer look at your filename reveals it is \n"
           "    %d characters long, too long for your C library's taste.\n",
           (int)strlen(dkptr->header_name));
      }
      RETURN( False );
   }

   for( ia=0 ; ia < blk->natr ; ia++ ){

      atr_any = &(blk->atr[ia]) ;
      if( ATR_COUNT(atr_any) <= 0 ) continue ;

      code = SUCCESS ;
      switch( atr_any->type ){

         case ATR_FLOAT_TYPE:{
            ATR_float * atr_flo = (ATR_float *) atr_any ;

            code = fprintf( header_file ,
                            "\ntype  = %s\nname  = %s\ncount = %d\n" ,
                            ATR_typestr[ATR_FLOAT_TYPE] ,
                            atr_flo->name , atr_flo->nfl ) ;

            if( code < 1 ){ code = FAIL ; break ; }

            for( ii=0 ; ii < atr_flo->nfl ; ii++ ){
               code = fprintf( header_file , " %14.7g" , atr_flo->fl[ii] ) ;
               if( ii % 5 == 4 && ii < atr_flo->nfl-1 )
                  fprintf( header_file , "\n" ) ;
            }

            code = (code < 1 ) ? FAIL : SUCCESS ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int * atr_int = (ATR_int *) atr_any ;

            code = fprintf( header_file ,
                            "\ntype = %s\nname = %s\ncount = %d\n" ,
                            ATR_typestr[ATR_INT_TYPE] ,
                            atr_int->name , atr_int->nin ) ;

            if( code < 1 ){ code = FAIL ; break ; }

            for( ii=0 ; ii < atr_int->nin ; ii++ ){
               code = fprintf( header_file , " %d" , atr_int->in[ii] ) ;
               if( ii % 5 == 4 && ii < atr_int->nin-1 )
                  fprintf( header_file , "\n" ) ;
            }

            code = (code < 1 ) ? FAIL : SUCCESS ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string * atr_str = (ATR_string *) atr_any ;

            code = fprintf( header_file ,
                            "\ntype = %s\nname = %s\ncount = %d\n" ,
                            ATR_typestr[ATR_STRING_TYPE] ,
                            atr_str->name , atr_str->nch ) ;


            if( code < 1 ){ code = FAIL ; break ; }

            THD_zblock( atr_str->nch , atr_str->ch ) ;

            fprintf( header_file , "'" ) ;
            for( ii=0 ; ii < atr_str->nch ; ii++ ){
               code = fprintf( header_file , "%c" , atr_str->ch[ii] ) ;
            }

            code = (code < 1) ? FAIL : SUCCESS ;

            THD_unzblock( atr_str->nch , atr_str->ch ) ;
         }
         break ;

      } /* end of switch on atr type */

      good = good && (code != FAIL) ;  /* all must not FAIL */
      fprintf(header_file,"\n") ;

   } /* end of loop over all atr's */

   if( good == False ){
      fprintf(stderr,
              "*** WARNING: error in output to attribute file %s;\n"
              "           - Is the disk full?\n" ,
              dkptr->header_name) ;
   }

   /*--- close it down ---*/

   fclose( header_file ) ;
   RETURN( True );
}

/*----------------------------------------------------------------------*/

Boolean THD_write_nimlatr( THD_datablock *blk )  /* 01 Jun 2005 */
{
   NI_stream ns ;
   NI_group  *ngr ;
   char sname[2048] ;
   THD_3dim_dataset *dset ;

ENTRY("THD_write_nimlatr") ;

   /* get dataset that contains this datablock */

   dset = (THD_3dim_dataset *)blk->parent ;
   if( !ISVALID_DSET(dset) ){
     STATUS("parent is not valid dataset!"); RETURN(False);      /* bad */
   }

   /* convert dataset struct AFNI attributes into a NIML element */

   ngr = THD_nimlize_dsetatr( dset ) ;
   if( ngr == NULL ){
     STATUS("can't create NIML header element!"); RETURN(False); /* bad */
   }
   NI_set_attribute( ngr , "self_prefix" , blk->diskptr->prefix ) ;

   /* open output NIML stream (to file) */

   sprintf(sname,"file:%s",blk->diskptr->header_name) ;
   ns = NI_stream_open( sname , "w" ) ;
   if( ns == (NI_stream)NULL ){
     STATUS("can't open output NIML stream!"); RETURN(False);    /* bad */
   }

   /* write XML header and then the AFNI header element */

   STATUS("writing NIML header") ;
   NI_stream_writestring( ns , "<?xml version='1.0' ?>\n" ) ;
   NI_write_element( ns , ngr , NI_TEXT_MODE ) ;
   NI_stream_close( ns ) ;
   RETURN(True) ;
}
