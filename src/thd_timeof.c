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
