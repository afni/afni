/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Routine to attach sub-bricks to the end of an already existing 3D dataset.
    - nbr = number of extra bricks
    - tbr = array of data types in each brick (defaults to MRI_short)
    - fbr = array of new brick_fac factors in each brick
             if NULL, new factors will be set to zero
    - sbr = array of pointers to data in each brick
             if sbr[i]==NULL, then the i'th extra brick will not have data
             if sbr==NULL, then all new extra bricks will not have data
               (in which case you will have to use mri_fix_data_pointer to
                attach data at a later time, if you plan to use the data)

  Note that this can only be done on a brick that is malloc-ed, not mmap-ed!
-----------------------------------------------------------------------------*/

void EDIT_add_bricklist( THD_3dim_dataset *dset ,
                         int nbr, int *tbr, float *fbr , void *sbr[] )
{
   int ibr , typ , nx,ny,nz , nvals,new_nvals ;
   THD_datablock *dblk ;
   MRI_IMAGE *qim ;
   char str[32] ;

ENTRY("EDIT_add_bricklist") ;

   /**-- Sanity Checks --**/

   if( ! ISVALID_3DIM_DATASET(dset) || nbr <= 0 )       EXRETURN; /* error! */
   if( dset->dblk->brick == NULL )                      EXRETURN; /* error! */
   if( dset->dblk->malloc_type != DATABLOCK_MEM_MALLOC )EXRETURN; /* error! */

   dblk  = dset->dblk ;
   nvals = dblk->nvals ;
   nx    = dblk->diskptr->dimsizes[0] ;
   ny    = dblk->diskptr->dimsizes[1] ;
   nz    = dblk->diskptr->dimsizes[2] ;

   /**-- reallocate the brick control information --**/

   new_nvals = nvals + nbr ;
   dblk->brick_bytes = (int64_t *) RwcRealloc( (char *) dblk->brick_bytes ,
                                          sizeof(int64_t) * new_nvals ) ;

   dblk->brick_fac = (float *) RwcRealloc( (char *) dblk->brick_fac ,
                                          sizeof(float) * new_nvals ) ;

   dblk->nvals = dblk->diskptr->nvals = new_nvals ;

   /** allocate new sub-brick images **/

   for( ibr=0 ; ibr < nbr ; ibr++ ){
      typ = (tbr != NULL ) ? tbr[ibr] : MRI_short ;
      qim = mri_new_vol_empty( nx,ny,nz , typ ) ;  /* image with no data */

      if( sbr != NULL && sbr[ibr] != NULL )        /* attach data to image */
         mri_fix_data_pointer( sbr[ibr] , qim ) ;

      ADDTO_IMARR( dblk->brick , qim ) ;           /* attach image to dset */

      dblk->brick_fac[nvals+ibr]   = (fbr != NULL) ? fbr[ibr] : 0.0 ;
      dblk->brick_bytes[nvals+ibr] = (int64_t)qim->pixel_size * (int64_t)qim->nvox ;
      dblk->total_bytes           += dblk->brick_bytes[ibr] ;
   }

   /** allocate new sub-brick auxiliary data: labels **/

   if( dblk->brick_lab == NULL )
      THD_init_datablock_labels( dblk ) ;
   else
      dblk->brick_lab = (char **) RwcRealloc( (char *) dblk->brick_lab ,
                                             sizeof(char *) * new_nvals ) ;
   for( ibr=0 ; ibr < nbr ; ibr++ ){
      sprintf( str , "#%d" , nvals+ibr ) ;
      dblk->brick_lab[nvals+ibr] = NULL ;
      THD_store_datablock_label( dblk , nvals+ibr , str ) ;
   }

   /** keywords **/

   if( dblk->brick_keywords == NULL )
      THD_init_datablock_keywords( dblk ) ;
   else
      dblk->brick_keywords = (char **) RwcRealloc( (char *) dblk->brick_keywords ,
                                                  sizeof(char *) * new_nvals ) ;
   for( ibr=0 ; ibr < nbr ; ibr++ ){
      dblk->brick_keywords[nvals+ibr] = NULL ;
      THD_store_datablock_keywords( dblk , nvals+ibr , NULL ) ;
   }

   /** stataux **/

   if( dblk->brick_statcode != NULL ){
      dblk->brick_statcode = (int *) RwcRealloc( (char *) dblk->brick_statcode ,
                                                sizeof(int) * new_nvals        ) ;
      dblk->brick_stataux  = (float **) RwcRealloc( (char *) dblk->brick_stataux ,
                                                   sizeof(float *) * new_nvals ) ;

      for( ibr=0 ; ibr < nbr ; ibr++ ){
         dblk->brick_statcode[nvals+ibr] = 0 ;
         dblk->brick_stataux[nvals+ibr]  = NULL ;
      }
   }

   /** fdrcurve **/

   if( dblk->brick_fdrcurve != NULL ){
     dblk->brick_fdrcurve = (floatvec **) realloc( (void *)dblk->brick_fdrcurve ,
                                                  sizeof(floatvec *) * new_nvals ) ;
     for( ibr=0 ; ibr < nbr ; ibr++ ) dblk->brick_fdrcurve[nvals+ibr] = NULL ;
   }

   if( dblk->brick_mdfcurve != NULL ){  /* 22 Oct 2008 */
     dblk->brick_mdfcurve = (floatvec **) realloc( (void *)dblk->brick_mdfcurve ,
                                                  sizeof(floatvec *) * new_nvals ) ;
     for( ibr=0 ; ibr < nbr ; ibr++ ) dblk->brick_mdfcurve[nvals+ibr] = NULL ;
   }

   EXRETURN;
}

/*---------------------------------------------------------------------------*/
/*! Add one brick to the end of a dataset. */

void EDIT_add_brick( THD_3dim_dataset *dset, int typ , float fac , void *br )
{
   int   ttt = typ ;
   float fff = fac ;
   void *bbb = br ;

   EDIT_add_bricklist( dset , 1 , &ttt , &fff , &bbb ) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/*!
   Turn float arrays into sub-bricks of a preset type
         (based on code in 3dMean)

   dset (THD_3dim_dataset *) new dset to which arrays will be added
   far (float **) each far[i] is to become one sub-brick in dset
   nval (int) the number of arrays in far
   otype (int) the sub-brick type. Supported options are:
               MRI_float (so far
   scaleopt (char) scaling options:
            'A' scale if needed
            'F' do scale each sub-brick
            'G' scale all sub-bricks with the same factor
            'N' Do not scale
   verb (int) loquaciousness
   returns 1 if all is well
           0 all hell broke loose

*/
int EDIT_add_bricks_from_far(THD_3dim_dataset *dset,
                    float **far, int nval,
                    int otype, char scaleopt,
                    int verb)
{
   int ii=0, kk=0, nxyz;

   ENTRY("EDIT_add_bricks_from_far");

   if (scaleopt != 'A' && scaleopt != 'F' && scaleopt != 'G' && scaleopt != 'N'){
      ERROR_message("Bad scaleopt value of %c", scaleopt);
      RETURN(0);
   }

   if (!dset) {
      ERROR_message("NULL input");
      RETURN(0);
   }

   nxyz = DSET_NVOX(dset);

   switch( otype ){

      default:
         ERROR_message("Somehow ended up with otype = %d\n",otype) ;
         RETURN(0) ;

      case MRI_float:{
         for( kk=0 ; kk < nval ; kk++ ){
             EDIT_substitute_brick(dset, kk, MRI_float, far[kk]);
             DSET_BRICK_FACTOR(dset, kk) = 0.0;
             far[kk] = NULL;
         }
      }
      break ;

      case MRI_byte:
      case MRI_short:{
         void ** dfim ;
         float gtop=0.0 , fimfac , gtemp ;

         if( verb )
            fprintf(stderr,"  ++ Scaling output to type %s brick(s)\n",
                    MRI_TYPE_name[otype] ) ;

         dfim = (void **) malloc(sizeof(void *)*nval) ;

         if( scaleopt == 'G' ){   /* allow global scaling */
            gtop = 0.0 ;
            for( kk=0 ; kk < nval ; kk++ ){
               gtemp = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, far[kk] ) ;
               gtop  = MAX( gtop , gtemp ) ;
               if( gtemp == 0.0 )
                  WARNING_message("output sub-brick %d is all zeros!\n",kk) ;
            }
         }

         for (kk = 0 ; kk < nval ; kk ++ ) {

            if( scaleopt != 'G' && scaleopt != 'N'){
                           /* compute max value in this sub-brick */
               gtop = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, far[kk] ) ;
               if( gtop == 0.0 )
                  WARNING_message("output sub-brick %d is all zeros!\n",kk) ;

            }

            if( scaleopt == 'F' || scaleopt == 'G'){ /* scaling needed */

               fimfac = (gtop > 0.0) ? MRI_TYPE_maxval[otype] / gtop : 0.0 ;

            } else if( scaleopt == 'A' ){  /* only if needed */

               fimfac = (  gtop > MRI_TYPE_maxval[otype] ||
                           (gtop > 0.0 && gtop < 1.0)       )
                        ? MRI_TYPE_maxval[otype]/ gtop : 0.0 ;

               if( fimfac == 0.0 && gtop > 0.0 ){  /* 14 May 2010 */
                 float fv,iv ;                     /* force scaling if */
                 for( ii=0 ; ii < nxyz ; ii++ ){   /* non-integers are inside */
                   fv = far[kk][ii] ; iv = rint(fv) ;
                   if( fabsf(fv-iv) >= 0.01 ){
                     fimfac = MRI_TYPE_maxval[otype] / gtop ; break ;
                   }
                 }
               }

            } else if( scaleopt == 'N') {          /* no scaling allowed */
               fimfac = 0.0 ;
            } else {
               ERROR_message("Should not see this one");
               RETURN(0);
            }


            if( verb ){
               if( fimfac != 0.0 )
                  INFO_message("Sub-brick %d scale factor = %f\n",kk,fimfac) ;
               else
                  INFO_message("Sub-brick %d: no scale factor\n" ,kk) ;
            }

            dfim[kk] = (void *) malloc( mri_datum_size(otype) * nxyz ) ;
            if( dfim[kk] == NULL ){
               ERROR_message("malloc fails at output\n");
               exit(1);
            }

            EDIT_coerce_scale_type( nxyz , fimfac ,
                                    MRI_float, far[kk] , otype,dfim[kk] ) ;
            if( otype == MRI_short )
              EDIT_misfit_report( DSET_FILECODE(dset) , kk ,
                                  nxyz , (fimfac != 0.0f) ? 1.0f/fimfac : 0.0f ,
                                  dfim[kk] , far[kk] ) ;
            free( far[kk] ) ; far[kk] = NULL;
            EDIT_substitute_brick(dset, kk, otype, dfim[kk] );

            DSET_BRICK_FACTOR(dset,kk) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;

            dfim[kk]=NULL;
          }
          free(dfim); dfim = NULL;
      }
      break ;
   }


   RETURN(1);
}
