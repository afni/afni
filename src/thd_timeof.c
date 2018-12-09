/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*****************************************************************
  Time at the z-coordinate "z", at the it-th time step
******************************************************************/

float THD_timeof( int it, float z, THD_timeaxis * tax )
{
   float sl , tof ;
   int  isl ;

   if( ! ISVALID_TIMEAXIS(tax) ) return 0.0 ;

   tof = tax->ttorg + it * tax->ttdel ;

   if( tax->nsl <= 0 || tax->toff_sl == NULL ) return tof ;

   isl = (z - tax->zorg_sl) / tax->dz_sl + 0.5 ;

   if( isl < 0 || isl >= tax->nsl ) return tof ;

   return tof + tax->toff_sl[isl] ;
}

/*--------------------------------------------------------------
   Get the time at voxel # nvox, at the it-th time step.
   22 July 1998 -- RWCox
----------------------------------------------------------------*/

float THD_timeof_vox( int it , int nvox , THD_3dim_dataset * dset )
{
   float sl , tof ;
   int isl ;

   if( !ISVALID_DSET(dset) || !ISVALID_TIMEAXIS(dset->taxis) ) return 0.0 ;

   tof = dset->taxis->ttorg + it * dset->taxis->ttdel ;

   if( dset->taxis->nsl <= 0 || dset->taxis->toff_sl == NULL ) return tof ;

   isl = nvox / ( DSET_NX(dset) * DSET_NY(dset) ) ;

   if( isl < 0 || isl >= dset->taxis->nsl ) return tof ;

   return tof + dset->taxis->toff_sl[isl] ;
}


/*---------------------------------------------------------------------------*/
/*
   Get the time at slice # isl, at the it-th time step.
   21 October 1999 -- B.D.Ward
*/

float THD_timeof_slice( int it , int isl , THD_3dim_dataset * dset )
{
   float tof ;

   if( !ISVALID_DSET(dset) || !ISVALID_TIMEAXIS(dset->taxis) ) return 0.0 ;

   tof = dset->taxis->ttorg + it * dset->taxis->ttdel ;

   if( dset->taxis->nsl <= 0 || dset->taxis->toff_sl == NULL ) return tof ;

   if( isl < 0 || isl >= dset->taxis->nsl ) return tof ;

   return tof + dset->taxis->toff_sl[isl] ;
}

/*--------------------------------------------------------------------------*/
/* moved here (and slightly edited) from 3dTshift.c [11 Dec 2007] */

float * TS_parse_tpattern( int nzz , float TR , char *tpattern )
{
   int ii ;
   float tframe , tsl ;
   float *tpat ;

   if( nzz < 1 ) return NULL ;
   tpat = (float *)malloc( sizeof(float) * nzz ) ;
   for( ii=0 ; ii < nzz ; ii++ ) tpat[ii] = 0.0 ;

   if( TR  < 0.0f ) TR = 1.0f ;
   tframe = TR / nzz ;  /* time per slice */

   if( nzz == 1 ||
       (tpattern == NULL || *tpattern == '\0' ||
        strcasecmp(tpattern,"zero")==0 || strcasecmp(tpattern,"simult")==0) ){

      /*--- do nothing [leave it all zeros] ---*/

   } else if( tpattern[0] == '@' ){
      MRI_IMAGE *tim ; float *tar ;

      /*--- read pattern file ---*/

      tim = mri_read_1D( tpattern+1 ) ;
      if( tim == NULL ){
         ERROR_exit("Can't read tpattern file %s",tpattern+1) ;
      /* allow arbitrary line wraps (can ny==0 happen?)  02 Sep 2014 [rickr] */
      } else if( tim->nx < nzz && tim->ny < nzz && (tim->nx*tim->ny < nzz) ){
         int nv = tim->nx * tim->ny;
         if( nv == 0 ) nv = (tim->nx > tim->ny) ? tim->nx : tim->ny;
         ERROR_exit("tpattern file %s has %d values but have %d slices",
                    tpattern+1 , nv , nzz ) ;
      } else {
         tar = MRI_FLOAT_PTR(tim) ;
         for( ii=0 ; ii < nzz ; ii++ ){
            tpat[ii] = tar[ii] ;
            if( tpat[ii] < 0.0f || tpat[ii] > TR )
               ERROR_exit("Illegal value %g in tpattern file %s",
                          tpat[ii] , tpattern+1 ) ;
         }
         mri_free(tim) ;
      }

   } else if( (strcmp(tpattern,"alt+z")==0 || strcmp(tpattern,"altplus")==0) ){

      /*--- set up alternating in the +z direction ---*/

      tsl = 0.0f ;
      for( ii=0 ; ii < nzz ; ii+=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }
      for( ii=1 ; ii < nzz ; ii+=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }

   } else if( strcmp(tpattern,"alt+z2")==0 ){  /* 22 Feb 2005 */
      /*--- set up alternating in the +z direction ---*/

      tsl = 0.0f ;
      for( ii=1 ; ii < nzz ; ii+=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }
      for( ii=0 ; ii < nzz ; ii+=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }

   } else if( (strcmp(tpattern,"alt-z")==0 || strcmp(tpattern,"altminus")==0) ){

      /*--- set up alternating in the -z direction ---*/

      tsl = 0.0f ;
      for( ii=nzz-1 ; ii >=0 ; ii-=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }
      for( ii=nzz-2 ; ii >=0 ; ii-=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }

   } else if( strcmp(tpattern,"alt-z2") == 0 ){  /* 22 Feb 2005 */

      /*--- set up alternating in the -z direction ---*/

      tsl = 0.0f ;
      for( ii=nzz-2 ; ii >=0 ; ii-=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }
      for( ii=nzz-1 ; ii >=0 ; ii-=2 ){
        tpat[ii] = tsl ; tsl += tframe ;
      }

   } else if( (strcmp(tpattern,"seq+z")==0 || strcmp(tpattern,"seqplus")==0) ){

      /*--- set up sequential in the +z direction ---*/

      tsl = 0.0f ;
      for( ii=0 ; ii < nzz ; ii++ ){
        tpat[ii] = tsl ; tsl += tframe ;
      }

   } else if( (strcmp(tpattern,"seq-z")==0 || strcmp(tpattern,"seqminus")==0) ){
      /*--- set up sequential in the -z direction ---*/

      tsl = 0.0f ;
      for( ii=nzz-1 ; ii >=0 ; ii-- ){
        tpat[ii] = tsl ; tsl += tframe ;
      }

   } else {
      ERROR_message("Unknown tpattern = %s",tpattern) ;
      return NULL ;
   }

   return tpat ;
}
