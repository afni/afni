/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/*-----------------------------------------------------------------------------
   Routine to make a cluster that is a mask of points closer than max_dist.

      nx, ny, nz   = number of voxels along each axis
      dx, dy, dz   = voxel dimensions
      max_dist     = maximum distance for a point to be included in the mask

   Date   :  11 September 1996

   To correct error due to abiguity in identification of clusters,
   voxel coordinates are now stored as 3 separate short integers.
   BDW  06 March 1997
-----------------------------------------------------------------------------*/

MCW_cluster * MCW_build_mask (int nx, int ny, int nz,
                              float dx, float dy, float dz,
                              float max_dist)
{
   int ii, jj, kk, idx, jdy, kdz, nxy, nxyz, ijkma, mnum;
   float xq, yq, zq, dist_q;
   MCW_cluster * mask;

   idx = max_dist / dx ; jdy = max_dist / dy ; kdz = max_dist / dz ;

#ifdef CLUST_DEBUG
printf("MCW_find_clusters: idx=%d jdy=%d kdz=%d\n",idx,jdy,kdz) ;
#endif

   if( (idx < 1 && jdy < 1 && kdz < 1) || (idx < 0 || jdy < 0 || kdz < 0 ) ){
      fprintf(stderr,"*** Illegal dimensions input to MCW_build_mask:\n"
                     "*** dx=%g dy=%g dz=%g max_dist=%g\n",
                     dx,dy,dz,max_dist ) ;
      return NULL ;
   }

   INIT_CLUSTER(mask) ;

   dist_q = max_dist * max_dist ;
   nxy = nx*ny ; nxyz = nxy * nz ;

   for( kk=-kdz ; kk <= kdz ; kk++ ){
      zq = (kk*dz) * (kk*dz) ;
      for( jj=-jdy ; jj <= jdy ; jj++ ){
         yq = zq + (jj*dy) * (jj*dy) ;
         for( ii=-idx ; ii <= idx ; ii++ ){
            xq = yq + (ii*dx)*(ii*dx) ;
            if( xq <= dist_q && xq > 0.0 ){
	      ADDTO_CLUSTER( mask , ii, jj, kk, 0 ) ;
            }
         }
      }
   }

#ifdef CLUST_DEBUG
printf("  mask size = %d\n",mask->num_pt ) ;
#endif

   mnum = mask->num_pt ;
   if( mnum < 2 ){
      KILL_CLUSTER(mask) ;
      fprintf(stderr,
              "*** MCW_build_mask error: mask has only %d elements!\n",mnum);
      return NULL ;
   }

   return (mask);
}
