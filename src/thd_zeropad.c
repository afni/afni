#include "mrilib.h"

/*-------------------------------------------------------------------
  Create a new dataset from an old one, with zero padding around
  the edges.  For example,
    add_I = number of zero planes to add at inferior edge
            (if < 0, number of data planes to cut off inferior edge)

  09 Feb 2001 - added "flag" input, which is the OR of
    ZPAD_EMPTY = produce only an padded "empty copy" of the input
    ZPAD_PURGE = purge input dataset bricks after they are copied
    ZPAD_MM    = increments are mm instead of slice counts
                 (at least 'add_?' mm will be added/subtracted)

  14 May 2002: if inputs crops are all zero, return something anyway
---------------------------------------------------------------------*/

THD_3dim_dataset * THD_zeropad( THD_3dim_dataset * inset ,
                                int add_I , int add_S , int add_A ,
                                int add_P , int add_L , int add_R ,
                                char * prefix , int flag )
{
   THD_3dim_dataset *outset ;
   int nxold,nyold,nzold , nxnew,nynew,nznew , nxyold,nxynew ,
       nxbot=0,nxtop=0 , nybot=0,nytop=0 , nzbot=0,nztop=0    ;
   int ii,jj,kk , iv , iibot,iitop , jjbot,jjtop , kkbot,kktop ;

   int empty_flag = (flag & ZPAD_EMPTY) ;  /* 09 Feb 2001 */
   int purge_flag = (flag & ZPAD_PURGE) ;  /* 09 Feb 2001 */
   int mm_flag    = (flag & ZPAD_MM   ) ;  /* 13 Feb 2001 */

   THD_ivec3 iv_nxyz ;
   THD_fvec3 fv_xyzorg ;

   MRI_IMAGE * oldim ;
   void * vnew ;

ENTRY("THD_zeropad") ;

   /*-- check inputs --*/

   if( !ISVALID_DSET(inset) ) RETURN( NULL ) ;

   if( add_I==0 && add_S==0 && add_P==0 &&
       add_A==0 && add_L==0 && add_R==0    ){

      fprintf(stderr,"++ THD_zeropad: all pad values are zero!\n") ;

      outset = EDIT_full_copy( inset , prefix ) ;  /* 14 May 2002 */
      RETURN( outset );
   }

   if( !THD_filename_ok(prefix) ) prefix = "zeropad" ;

   /*-- map add_? values into dataset xyz coordinate directions --*/

   nxold = DSET_NX(inset) ;
   nyold = DSET_NY(inset) ;
   nzold = DSET_NZ(inset) ;

   /* comput n?top and n?bot, the number of planes to add at
      the top and bottom of the ? direction, for ? = x, y, or z */

   switch( inset->daxes->xxorient ){
      default:
        fprintf(stderr,"*** THD_zeropad: Unknown orientation codes!\n") ;
        RETURN( NULL );

      case ORI_R2L_TYPE: nxtop = add_L ; nxbot = add_R ; break ;
      case ORI_L2R_TYPE: nxtop = add_R ; nxbot = add_L ; break ;
      case ORI_P2A_TYPE: nxtop = add_A ; nxbot = add_P ; break ;
      case ORI_A2P_TYPE: nxtop = add_P ; nxbot = add_A ; break ;
      case ORI_I2S_TYPE: nxtop = add_S ; nxbot = add_I ; break ;
      case ORI_S2I_TYPE: nxtop = add_I ; nxbot = add_S ; break ;
   }

   switch( inset->daxes->yyorient ){
      default:
        fprintf(stderr,"*** THD_zeropad: Unknown orientation codes!\n") ;
        RETURN( NULL );

      case ORI_R2L_TYPE: nytop = add_L ; nybot = add_R ; break ;
      case ORI_L2R_TYPE: nytop = add_R ; nybot = add_L ; break ;
      case ORI_P2A_TYPE: nytop = add_A ; nybot = add_P ; break ;
      case ORI_A2P_TYPE: nytop = add_P ; nybot = add_A ; break ;
      case ORI_I2S_TYPE: nytop = add_S ; nybot = add_I ; break ;
      case ORI_S2I_TYPE: nytop = add_I ; nybot = add_S ; break ;
   }

   switch( inset->daxes->zzorient ){
      default:
        fprintf(stderr,"*** THD_zeropad: Unknown orientation codes!\n") ;
        RETURN( NULL );

      case ORI_R2L_TYPE: nztop = add_L ; nzbot = add_R ; break ;
      case ORI_L2R_TYPE: nztop = add_R ; nzbot = add_L ; break ;
      case ORI_P2A_TYPE: nztop = add_A ; nzbot = add_P ; break ;
      case ORI_A2P_TYPE: nztop = add_P ; nzbot = add_A ; break ;
      case ORI_I2S_TYPE: nztop = add_S ; nzbot = add_I ; break ;
      case ORI_S2I_TYPE: nztop = add_I ; nzbot = add_S ; break ;
   }

   /* 13 Feb 2001: round to millimeters? */

#undef  RMM
#define RMM(n,d)                                                           \
  do{      if( (n) > 0 ) (n) = (int)( (n)/fabs(d) + 0.999 ) ;              \
      else if( (n) < 0 ) (n) = (int)( (n)/fabs(d) - 0.999 ) ; } while(0) ;

   if( mm_flag ){
      RMM(nxtop,inset->daxes->xxdel) ; RMM(nxbot,inset->daxes->xxdel) ;
      RMM(nytop,inset->daxes->yydel) ; RMM(nybot,inset->daxes->yydel) ;
      RMM(nztop,inset->daxes->zzdel) ; RMM(nzbot,inset->daxes->zzdel) ;
   }

   nxnew = nxold + nxbot + nxtop ;  /* dimensions of new bricks */
   nynew = nyold + nybot + nytop ;
   nznew = nzold + nzbot + nztop ;

   nxyold = nxold * nyold ;         /* for computing subscripts */
   nxynew = nxnew * nynew ;

   iibot = MAX(0,-nxbot) ; iitop = MIN(nxold,nxold+nxtop) ;  /* range of data */
   jjbot = MAX(0,-nybot) ; jjtop = MIN(nyold,nyold+nytop) ;  /* in old dataset */
   kkbot = MAX(0,-nzbot) ; kktop = MIN(nzold,nzold+nztop) ;

   if( nxnew < 1 || iibot > iitop ||   /* check for reasonable sizes */
       nynew < 1 || jjbot > jjtop ||   /* and ranges of dataset     */
       nznew < 1 || kkbot > kktop   ){

      fprintf(stderr,"*** THD_zeropad: Can't cut dataset down too much!\n") ;
      RETURN( NULL );
   }

   if( nxnew < 2 || iibot >= iitop ||   /* check for reasonable sizes */
       nynew < 2 || jjbot >= jjtop ||   /* and ranges of dataset     */
       nznew < 2 || kkbot >= kktop   ){

      fprintf(stderr,"*** WARNING - THD_zeropad: dataset cut down to %dx%dx%d\n",
                      nxnew,nynew,nznew) ;
   }

   /*-- create the shell of the new dataset --*/

   outset = EDIT_empty_copy( inset ) ;

   LOAD_IVEC3( iv_nxyz , nxnew,nynew,nznew ) ;

   LOAD_FVEC3( fv_xyzorg, inset->daxes->xxorg - nxbot * inset->daxes->xxdel,
                          inset->daxes->yyorg - nybot * inset->daxes->yydel,
                          inset->daxes->zzorg - nzbot * inset->daxes->zzdel );

STATUS("setting new dimensions") ;

   EDIT_dset_items( outset ,
                       ADN_prefix , prefix    ,
                       ADN_nxyz   , iv_nxyz   ,
                       ADN_xyzorg , fv_xyzorg ,
                    ADN_none ) ;

   /* Changing dimensions means old anat parent is no longer valid! */

   EDIT_ZERO_ANATOMY_PARENT_ID( outset ) ;
   outset->anat_parent_name[0] = '\0' ;

#if 0
   /* if changing number of slices, can't keep slice-dependent time shifts! */

   if( (nzbot!=0 || nztop!=0) && outset->taxis != NULL && outset->taxis->nsl > 0 ){
      EDIT_dset_items( outset , ADN_nsl , 0 , ADN_none ) ;
      fprintf(stderr,
              "*** THD_zeropad: warning - slice-dependent time shifts have been removed!\n") ;
   }
#else
   /* 31 Jan 2001: OK, lets keeps the slice-dependent time shifts
                   (but we'll have to mangle them) -- RWCox      */

   if( (nzbot!=0 || nztop!=0) && outset->taxis != NULL && outset->taxis->nsl > 0 ){
      int old_nsl , new_nsl , ii, nkeep,kbot,ibot ;
      float old_zorg_sl , *old_toff_sl , new_zorg_sl , *new_toff_sl ;

      /* copy current conditions to local variables */

      old_nsl     = outset->taxis->nsl ;
      old_zorg_sl = outset->taxis->zorg_sl ;
      old_toff_sl = (float *) malloc(sizeof(float)*old_nsl) ;
      memcpy( old_toff_sl , outset->taxis->toff_sl , sizeof(float)*old_nsl ) ;

      /* compute new values */

      new_nsl     = nznew ;
      new_zorg_sl = outset->daxes->zzorg ;                      /* cf. to3d.c */
      new_toff_sl = (float *) malloc(sizeof(float)*new_nsl) ;
      for( ii=0 ; ii < new_nsl ; ii++ ) new_toff_sl[ii] = 0.0 ; /* extras are 0 */

      nkeep = old_nsl ;                 /* how many to keep from the old list */
      if( nzbot < 0 ) nkeep += nzbot ;  /* lost this many at the bottom */
      if( nztop < 0 ) nkeep += nztop ;  /* lost this many at the top */

      if( nzbot < 0 ){
         kbot = -nzbot ;   /* which old one to start with */
         ibot = 0 ;        /* and where it goes in new list */
      } else {
         kbot = 0 ;
         ibot = nzbot ;
      }

      memcpy( new_toff_sl+ibot , old_toff_sl+kbot , sizeof(float)*nkeep ) ;

      /* set new values in dataset */

STATUS("setting new time-offsets") ;

      EDIT_dset_items( outset ,
                         ADN_nsl     , new_nsl     ,
                         ADN_toff_sl , new_toff_sl ,
                         ADN_zorg_sl , new_zorg_sl ,
                       ADN_none ) ;

      free(new_toff_sl) ; free(old_toff_sl) ;
   }
#endif

   if( empty_flag ) RETURN(outset) ;  /* 09 Feb 2001 */

   /*-- now read the old dataset in, and make bricks for the new dataset --*/

STATUS("reading dataset in") ;

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"*** THD_zeropad: Can't load input dataset BRIK!\n");
      DSET_delete(outset) ;
      RETURN( NULL );
   }

STATUS("padding") ;

   for( iv=0 ; iv < DSET_NVALS(inset) ; iv++ ){

      /* create a brick of zeros */

      oldim = DSET_BRICK(inset,iv) ;  /* image structure of old brick */

      vnew  = calloc( nxnew*nynew*nznew , oldim->pixel_size ) ; /* new brick */
      if( vnew == NULL ){
         fprintf(stderr,
                 "*** THD_zeropad: Can't malloc space for new sub-brick %d\n",
                 iv) ;
         DSET_delete(outset) ; RETURN( NULL );
      }

      /* macros for computing 1D subscripts from 3D indices */

#undef  SNEW  /* in case was defined in some stupid .h file */
#undef  SOLD
#define SNEW(i,j,k) ((i+nxbot)+(j+nybot)*nxnew+(k+nzbot)*nxynew)
#define SOLD(i,j,k) (i+j*nxold+k*nxyold)

      switch( oldim->kind ){  /* copy rows of old into new */

         case MRI_byte:{
            byte * bnew = (byte *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_short:{
            short * bnew = (short *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_float:{
            float * bnew = (float *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_complex:{
            complex * bnew = (complex *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_int:{
            int * bnew = (int *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_rgb:{
            rgbyte * bnew = (rgbyte *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

      } /* end of switch on sub-brick type */

      if( purge_flag) DSET_unload_one(inset,iv) ; /* 09 Feb 2001 */

      EDIT_substitute_brick( outset , iv , oldim->kind , vnew ) ;

   } /* end of loop on sub-brick index */

#if 0
   if( purge_flag ) DSET_unload(inset) ; /* 09 Feb 2001 */
#endif

   RETURN( outset );
}
