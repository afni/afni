#include "mrilib.h"

/*-------------------------------------------------------------------
  Create a new dataset from an old one, with zero padding around
  the edges.  For example,
    add_I = number of zero planes to add at inferior edge
            (if < 0, number of data planes to cut off inferior edge)
---------------------------------------------------------------------*/

THD_3dim_dataset * THD_zeropad( THD_3dim_dataset * inset ,
                                int add_I , int add_S , int add_A ,
                                int add_P , int add_L , int add_R ,
                                char * prefix )
{
   THD_3dim_dataset *outset ;
   int nxold,nyold,nzold , nxnew,nynew,nznew , nxyold,nxynew ,
       nxbot=0,nxtop=0 , nybot=0,nytop=0 , nzbot=0,nztop=0    ;
   int ii,jj,kk , iv , iibot,iitop , jjbot,jjtop , kkbot,kktop ;

   THD_ivec3 iv_nxyz ;
   THD_fvec3 fv_xyzorg ;

   MRI_IMAGE * oldim ;
   void * vnew ;

   /*-- check inputs --*/

   if( !ISVALID_DSET(inset)                ||
       (add_I==0 && add_S==0 && add_P==0 &&
        add_A==0 && add_L==0 && add_R==0   ) ){

      fprintf(stderr,"*** THD_zeropad: illegal inputs!\n") ;
      return NULL ;
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
        return NULL ;

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
        return NULL ;

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
        return NULL ;

      case ORI_R2L_TYPE: nztop = add_L ; nzbot = add_R ; break ;
      case ORI_L2R_TYPE: nztop = add_R ; nzbot = add_L ; break ;
      case ORI_P2A_TYPE: nztop = add_A ; nzbot = add_P ; break ;
      case ORI_A2P_TYPE: nztop = add_P ; nzbot = add_A ; break ;
      case ORI_I2S_TYPE: nztop = add_S ; nzbot = add_I ; break ;
      case ORI_S2I_TYPE: nztop = add_I ; nzbot = add_S ; break ;
   }

   nxnew = nxold + nxbot + nxtop ;  /* dimensions of new bricks */
   nynew = nyold + nybot + nytop ;
   nznew = nzold + nzbot + nztop ;

   nxyold = nxold * nyold ;         /* for computing subscripts */
   nxynew = nxnew * nynew ;

   iibot = MAX(0,-nxbot) ; iitop = MIN(nxold,nxold+nxtop) ;  /* range of data */
   jjbot = MAX(0,-nybot) ; jjtop = MIN(nyold,nyold+nytop) ;  /* in old dataset */
   kkbot = MAX(0,-nzbot) ; kktop = MIN(nzold,nzold+nztop) ;

   if( nxnew < 2 || iibot >= iitop ||   /* check for reasonable sizes */
       nynew < 2 || jjbot >= jjtop ||   /* and ranges of dataset     */
       nznew < 2 || kkbot >= kktop   ){

      fprintf(stderr,"*** THD_zeropad: Can't cut dataset down too much!\n") ;
      return NULL ;
   }

   /*-- create the shell of the new dataset --*/

   outset = EDIT_empty_copy( inset ) ;

   LOAD_IVEC3( iv_nxyz , nxnew,nynew,nznew ) ;

   LOAD_FVEC3( fv_xyzorg, inset->daxes->xxorg - nxbot * inset->daxes->xxdel,
                          inset->daxes->yyorg - nybot * inset->daxes->yydel,
                          inset->daxes->zzorg - nzbot * inset->daxes->zzdel );

   EDIT_dset_items( outset ,
                       ADN_prefix , prefix    ,
                       ADN_nxyz   , iv_nxyz   ,
                       ADN_xyzorg , fv_xyzorg ,
                    ADN_none ) ;

   /* Changing dimensions means old anat parent is no longer valid! */

   EDIT_ZERO_ANATOMY_PARENT_ID( outset ) ;
   outset->anat_parent_name[0] = '\0' ;

   /* if changing number of slices, can't keep slice-dependent time shifts! */

   if( nznew != nzold && outset->taxis != NULL && outset->taxis->nsl > 0 )
      EDIT_dset_items( outset , ADN_nsl , 0 , ADN_none ) ;

   /*-- now read the old dataset in, and make bricks for the new dataset --*/

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"*** THD_zeropad: Can't load input dataset BRIK!\n");
      DSET_delete(outset) ;
      return NULL ;
   }

   for( iv=0 ; iv < DSET_NVALS(inset) ; iv++ ){

      /* create a brick of zeros */

      oldim = DSET_BRICK(inset,iv) ;  /* image structure of old brick */

      vnew  = calloc( nxnew*nynew*nznew , oldim->pixel_size ) ; /* new brick */
      if( vnew == NULL ){
         fprintf(stderr,
                 "*** THD_zeropad: Can't malloc space for new sub-brick %d\n",
                 iv) ;
         DSET_delete(outset) ; return NULL ;
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

      } /* end of switch on sub-brick type */

      DSET_unload_one(inset,iv) ; /* don't need this no more */

      EDIT_substitute_brick( outset , iv , oldim->kind , vnew ) ;

   } /* end of loop on sub-brick index */

   /*-- finished --*/

   return outset ;
}
