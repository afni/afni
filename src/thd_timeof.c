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
