/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*-----------------------------------------------------------------------------*/
/*! Time shift a dataset to a common origin.  Stores the data on top of the
   input dataset!  Return value is 0 for good processing, nonzero for errors.
   This routine is like doing the default actions of 3dTshift (from which
   the code is adapted).  -- RWCox -- 15 Feb 2001
-------------------------------------------------------------------------------*/

 /*-- At present, these static variables are not alterable from the
      outside; they correspond to the command line options of 3dTshift.
      In the future, there may be a way to changes these to affect things. --*/

static float   TS_TR      = 0.0 ;
static int     TS_tunits  = UNITS_SEC_TYPE ;
static float * TS_tpat    = NULL ;
static float   TS_tzero   = -1.0 ;
static int     TS_slice   = -1 ;
static int     TS_rlt     = 0 ;   /* 0=add both in; 1=add neither; 2=add mean */
static int     TS_mode    = MRI_FOURIER ; /* one of MRI_FOURIER MRI_CUBIC
                                                    MRI_QUINTIC MRI_HEPTIC
                                                    MRI_LINEAR             */

int THD_dataset_tshift( THD_3dim_dataset * TS_dset , int ignore )
{
   int nzz, ii,jj,kk , ntt,nxx,nyy,nxy , nup , freepat=0 ;
   float tomax,tomin , tshift , fmin,fmax , gmin,gmax , f0,f1 , g0,g1 ;
   float ffmin,ffmax , ggmin,ggmax ;
   MRI_IMAGE * flim , * glim ;
   float * far , * gar ;

ENTRY("THD_dataset_tshift") ;

   /*- extract dataset values, check for errors -*/

   if( !ISVALID_DSET(TS_dset) || ignore < 0 ) RETURN(1) ;

   nxx = DSET_NX(TS_dset) ;                      /* get dimensions */
   nyy = DSET_NY(TS_dset) ; nxy = nxx * nyy ;
   nzz = DSET_NZ(TS_dset) ;
   ntt = DSET_NVALS(TS_dset) ;
   if( ignore > ntt-4 ) RETURN(1) ;

   if( DSET_NVALS(TS_dset) < 2 ) RETURN(1) ;
   if( TS_slice >= nzz ) RETURN(1) ;

   if( TS_dset->taxis == NULL ){
      if( TS_TR == 0.0 || TS_tpat == NULL ) RETURN(1) ;
   } else if( TS_tpat == NULL && TS_dset->taxis->toff_sl == NULL ){
      RETURN(1) ;
   }

   if( TS_TR == 0.0 ){                                    /* set TR from dataset */
      TS_TR     = DSET_TIMESTEP(TS_dset) ;
      TS_tunits = TS_dset->taxis->units_type ;
   }

   if( TS_tpat == NULL ){
      if( TS_dset->taxis->nsl < nzz ) RETURN(1) ;
      TS_tpat = (float *) malloc( sizeof(float) * nzz ) ;
      memcpy( TS_tpat , TS_dset->taxis->toff_sl , sizeof(float)*nzz ) ;
      freepat = 1 ;
   }

   tomin = WAY_BIG ; tomax = -WAY_BIG ;                      /* check pattern */
   for( ii=0 ; ii < nzz ; ii++ ){
      if( TS_tpat[ii] > tomax ) tomax = TS_tpat[ii] ;
      if( TS_tpat[ii] < tomin ) tomin = TS_tpat[ii] ;
   }
   if( tomin < 0.0 || tomax > TS_TR ) RETURN(1) ;
   else if( tomin >= tomax )          RETURN(1) ;

   if( TS_slice >= 0 && TS_slice < nzz ){                   /* set common time point */
      TS_tzero = TS_tpat[TS_slice] ;
   } else if( TS_tzero < 0.0 ){
      TS_tzero = 0.0 ;
      for( ii=0 ; ii < nzz ; ii++ ) TS_tzero += TS_tpat[ii] ;
      TS_tzero /= nzz ;
   }

   /*- reconfigure the time axis -*/

   DSET_mallocize( TS_dset) ;
   DSET_load( TS_dset ) ; if( !DSET_LOADED(TS_dset) ) RETURN(1) ;

   EDIT_dset_items( TS_dset ,
                       ADN_ntt    , ntt       ,  /* in case not already set */
                       ADN_ttdel  , TS_TR     ,  /* may have changed */
                       ADN_tunits , TS_tunits ,  /* may have changed */
                       ADN_nsl    , 0         ,  /* will have no offsets when done */
                       ADN_ttorg  , 0.0       ,  /* in case not already set */
                       ADN_ttdur  , 0.0       ,  /* in case not already set */
                    ADN_none ) ;

   /*---- do the temporal shifting! ----*/

   SHIFT_set_method( TS_mode ) ;

   nup = csfft_nextup_one35( ntt+4 ) ;

   for( kk=0 ; kk < nzz ; kk++ ){                            /* loop over slices */

      tshift = (TS_tzero - TS_tpat[kk]) / TS_TR ;  /* rightward fractional shift */
#if 1
      tshift = -tshift ;  /* 24 Apr 2003 -- OOG */
#endif

      if( fabs(tshift) < 0.001 ) continue ;                   /* skip this slice */

      for( ii=0 ; ii < nxy ; ii+=2 ){          /* loop over voxel pairs in slice */

         flim = THD_extract_series( ii+kk*nxy , TS_dset , 0 ); /* get this voxel */
         far  = MRI_FLOAT_PTR(flim) ;

         if( TS_rlt == 0 ){                             /* range of data: before */
            for( ffmin=ffmax=far[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                    if( far[jj] < ffmin ) ffmin = far[jj] ;
               else if( far[jj] > ffmax ) ffmax = far[jj] ;
            }
         }

         THD_linear_detrend( ntt-ignore , far+ignore , &f0,&f1 ) ;             /* remove trend */

         for( fmin=fmax=far[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                 if( far[jj] < fmin ) fmin = far[jj] ;   /* range of data: after */
            else if( far[jj] > fmax ) fmax = far[jj] ;
         }

         if( ii < nxy-1 ){                                       /* get next voxel */
            glim = THD_extract_series( ii+kk*nxy+1 , TS_dset , 0 ) ;
            gar  = MRI_FLOAT_PTR(glim) ;
            if( TS_rlt == 0 ){
               for( ggmin=ggmax=gar[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                       if( gar[jj] < ggmin ) ggmin = gar[jj] ;
                  else if( gar[jj] > ggmax ) ggmax = gar[jj] ;
               }
            }

            THD_linear_detrend( ntt-ignore , gar+ignore , &g0,&g1 ) ;

            for( gmin=gmax=gar[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                    if( gar[jj] < gmin ) gmin = gar[jj] ;
               else if( gar[jj] > gmax ) gmax = gar[jj] ;
            }

         } else {
            gar  = NULL ;
         }

         if( gar != NULL )
            SHIFT_two_rows( ntt-ignore,nup, tshift,far+ignore , tshift, gar+ignore ) ;
         else
            SHIFT_two_rows( ntt-ignore,nup, tshift,far+ignore , tshift, NULL ) ;

         for( jj=ignore ; jj < ntt ; jj++ ){
                 if( far[jj] < fmin ) far[jj] = fmin ;           /* clip to input range */
            else if( far[jj] > fmax ) far[jj] = fmax ;
            switch( TS_rlt ){                                    /* restore trend? */
               case 0:
                  far[jj] += (f0 + (jj-ignore)*f1) ;
                       if( far[jj] < ffmin ) far[jj] = ffmin ;
                  else if( far[jj] > ffmax ) far[jj] = ffmax ;
               break ;

               case 2:
                  far[jj] += f0 ;
               break ;
            }
         }

         if( gar != NULL ){
            for( jj=ignore ; jj < ntt ; jj++ ){
                    if( gar[jj] < gmin ) gar[jj] = gmin ;
               else if( gar[jj] > gmax ) gar[jj] = gmax ;
               switch( TS_rlt ){
                  case 0:
                     gar[jj] += (g0 + (jj-ignore)*g1) ;
                          if( gar[jj] < ggmin ) gar[jj] = ggmin ;
                     else if( gar[jj] > ggmax ) gar[jj] = ggmax ;
                  break ;

                  case 2:
                     gar[jj] += g0 ;
                  break ;
               }
            }
         }

         /* put back into dataset */

         THD_insert_series( ii+kk*nxy , TS_dset , ntt , MRI_float , far , 0 ) ;
         if( gar != NULL )
            THD_insert_series( ii+kk*nxy+1 , TS_dset , ntt , MRI_float , gar , 0 ) ;

         /* throw out the trash */

         mri_free(flim) ; if( gar != NULL ) mri_free(glim) ;

      } /* end of loop over voxel pairs */

   } /* end of loop over slices */

   if( freepat ){ free(TS_tpat) ; TS_tpat = NULL ; }  /* resets */
   TS_tzero = -1.0 ; TS_TR = 0.0 ; TS_tunits = UNITS_SEC_TYPE ;

   RETURN(0) ;
}
