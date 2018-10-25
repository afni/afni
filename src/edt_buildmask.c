/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*-----------------------------------------------------------------------------*/
/*! Routine to make a cluster that is a mask of points closer than max_dist.
     - dx, dy, dz = voxel dimensions
     - max_dist   = maximum distance for a point to be included in the mask
   Date   :  11 September 1996

   To correct error due to ambiguity in identification of clusters,
   voxel coordinates are now stored as 3 separate short integers.
   BDW  06 March 1997

   30 Apr 2002: max_dist input as <= 0 now gives NN connectivity

   N.B.: The cluster does NOT contain the (0,0,0) point!
   N.B.: The cluster is not sorted by radius from the (0,0,0) point!
         To do this, see MCW_radsort_cluster().
-----------------------------------------------------------------------------*/

MCW_cluster * MCW_build_mask( float dx, float dy, float dz, float max_dist )
{
   int ii, jj, kk, idx, jdy, kdz, mnum;
   float xq, yq, zq, dist_q;
   MCW_cluster *mask;

ENTRY("MCW_build_mask") ;

   if( max_dist <= 0.0 ){                   /* 30 Apr 2002 */
     dx = dy = dz = 1.0f ; max_dist = 1.01f ;
   } else {
     if( dx <= 0.0f ) dx = 1.0f ;           /* something sensible */
     if( dy <= 0.0f ) dy = 1.0f ;
     if( dz <= 0.0f ) dz = 1.0f ;
   }

   idx = max_dist/dx ; jdy = max_dist/dy ; kdz = max_dist/dz ;

   if( idx < 1 && jdy < 1 && kdz < 1 ){
     WARNING_message("Illegal input to MCW_build_mask:"
                    " dx=%g dy=%g dz=%g max_dist=%g"   ,
                    dx,dy,dz,max_dist ) ;
     RETURN( NULL );
   }

   INIT_CLUSTER(mask) ;

   dist_q = max_dist * max_dist ;

   for( kk=-kdz ; kk <= kdz ; kk++ ){
     zq = (kk*dz) * (kk*dz) ;
     for( jj=-jdy ; jj <= jdy ; jj++ ){
       yq = zq + (jj*dy) * (jj*dy) ;
       for( ii=-idx ; ii <= idx ; ii++ ){
         xq = yq + (ii*dx)*(ii*dx) ;
         if( xq <= dist_q && xq > 0.0f ){
           ADDTO_CLUSTER( mask , ii, jj, kk, 0 ) ;
         }
       }
     }
   }

   mnum = mask->num_pt ;
   if( mnum < 1 ){
     KILL_CLUSTER(mask) ;
     WARNING_message("MCW_build_mask error: mask has only %d elements!",mnum);
     RETURN( NULL );
   }

   RETURN (mask);
}

/*----------------------------------------------------------------------*/
/*! Like MCW_build_mask(), but adds the (0,0,0) point to the cluster,
    and then sorts by radius (so the points closest to the origin
    are first in the array).  The radius from the origin is stored
    in the 'mag' element of the cluster struct.
------------------------------------------------------------------------*/

MCW_cluster * MCW_spheremask( float dx, float dy, float dz, float radius )
{
   MCW_cluster *mask=NULL;
   int ii , nn ;
   float x,y,z ;

   if( radius != 0.0f )
     mask = MCW_build_mask(dx,dy,dz,radius) ;

   if( mask == NULL ){ INIT_CLUSTER(mask) ; }
   ADDTO_CLUSTER(mask,0,0,0,0) ;

   /** sorting stuff added 20 Oct 2006 **/

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   nn = mask->num_pt ;
   for( ii=0 ; ii < nn ; ii++ ){
     x = mask->i[ii]*dx; y = mask->j[ii]*dy; z = mask->k[ii]*dz;

/*     if((x==0.0)&&(y==0.0)&&(z==0.0)) 
         mask->mag[ii] = 1.0;
     else
*/

         mask->mag[ii] = sqrt(x*x+y*y+z*z) ;
   }
   MCW_sort_cluster( mask ) ;
   return mask ;
}

/*----------------------------------------------------------------------*/
/*! Like MCW_spheremask(), but builds a rectangular parallelopiped. */

MCW_cluster * MCW_rectmask( float dx, float dy, float dz,
                            float xh, float yh, float zh )
{
   int ii, jj, kk, idx, jdy, kdz ;
   MCW_cluster *mask;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   if( xh <  0.0f ) xh = 0.0f ;
   if( yh <  0.0f ) yh = 0.0f ;
   if( zh <  0.0f ) zh = 0.0f ;

   idx = (int)(xh/dx) ;
   jdy = (int)(yh/dy) ;
   kdz = (int)(zh/dz) ;

   INIT_CLUSTER(mask) ;

   ADDTO_CLUSTER(mask,0,0,0,0) ; /* always keep central point first */
   for( kk=-kdz ; kk <= kdz ; kk++ ){
    for( jj=-jdy ; jj <= jdy ; jj++ ){
     for( ii=-idx ; ii <= idx ; ii++ ){
       if (ii || jj || kk)
       ADDTO_CLUSTER( mask , ii,jj,kk , 0 ) ;
   }}}

   return mask ;
}

/*----------------------------------------------------------------------*/
/*! Like MCW_spheremask(), but builds a rhombic dodecahedron.
    Volume = 2 * radius**3.
*//*--------------------------------------------------------------------*/

MCW_cluster * MCW_rhddmask( float dx, float dy, float dz, float radius )
{
   int ii, jj, kk, idx, jdy, kdz ;
   float a,b,c ;
   MCW_cluster *mask;

   if( radius <= 0.0 ){                   /* 30 Apr 2002 */
     dx = dy = dz = 1.0f ; radius = 1.01f ;
   } else {
     if( dx <= 0.0f ) dx = 1.0f ;
     if( dy <= 0.0f ) dy = 1.0f ;
     if( dz <= 0.0f ) dz = 1.0f ;
   }

   idx = (int)(radius/dx) ;
   jdy = (int)(radius/dy) ;
   kdz = (int)(radius/dz) ;

   INIT_CLUSTER(mask) ;

   ADDTO_CLUSTER(mask,0,0,0,0) ; /* always keep central point first */
   for( kk=-kdz ; kk <= kdz ; kk++ ){
    c = kk*dz ;
    for( jj=-jdy ; jj <= jdy ; jj++ ){
     b = jj*dy ;
     for( ii=-idx ; ii <= idx ; ii++ ){
       if (ii || jj || kk) {
          a = ii*dx ;
          if( fabsf(a+b) <= radius &&
              fabsf(a-b) <= radius &&
              fabsf(a+c) <= radius &&
              fabsf(a-c) <= radius &&
              fabsf(b+c) <= radius &&
              fabsf(b-c) <= radius   ) ADDTO_CLUSTER( mask , ii,jj,kk , 0 ) ;
       }
   }}}

   return mask ;
}

/*----------------------------------------------------------------------*/
/*! Like MCW_spheremask(), but builds a truncated octahedron.
    Volume = 4 * radius**3.
*//*--------------------------------------------------------------------*/

#undef  FAS
#undef  TOHD_inside
#define FAS(a,b) (fabsf(a) <= (b))
#define TOHD_inside(a,b,c,siz)                                      \
  ( FAS((a),(siz)) && FAS((b),(siz)) && FAS((c),(siz))         &&   \
    FAS((a)+(b)+(c),1.5f*(siz)) && FAS((a)-(b)+(c),1.5f*(siz)) &&   \
    FAS((a)+(b)-(c),1.5f*(siz)) && FAS((a)-(b)-(c),1.5f*(siz))   )

MCW_cluster * MCW_tohdmask( float dx, float dy, float dz, float radius )
{
   int ii, jj, kk, idx, jdy, kdz ;
   float a,b,c ;
   MCW_cluster *mask;

   if( radius <= 0.0 ){                   /* 30 Apr 2002 */
     dx = dy = dz = 1.0f ; radius = 1.01f ;
   } else {
     if( dx <= 0.0f ) dx = 1.0f ;
     if( dy <= 0.0f ) dy = 1.0f ;
     if( dz <= 0.0f ) dz = 1.0f ;
   }

   idx = (int)(radius/dx) ;
   jdy = (int)(radius/dy) ;
   kdz = (int)(radius/dz) ;

   INIT_CLUSTER(mask) ;

   ADDTO_CLUSTER(mask,0,0,0,0) ; /* always keep central point first */
   for( kk=-kdz ; kk <= kdz ; kk++ ){
    c = kk*dz ;
    for( jj=-jdy ; jj <= jdy ; jj++ ){
     b = jj*dy ;
     for( ii=-idx ; ii <= idx ; ii++ ){
       a = ii*dx ;
       if( (ii || jj || kk) && TOHD_inside(a,b,c,radius) ) 
                              ADDTO_CLUSTER( mask , ii,jj,kk , 0 ) ;
   }}}

   return mask ;
}

void MCW_showmask (MCW_cluster *nbhd, char *opening, char *closing, FILE *fout)
{
   int ii;
   if (!fout) fout = stdout;
   if (opening) fprintf(fout, "%s", opening);
   if (!nbhd) {
      fprintf(fout, "NULL nbhd\n");
   } else {
      fprintf(fout, "Neighborhood of %d voxels (%d allocated), %s mag.\n", 
                    nbhd->num_pt, nbhd->num_all, nbhd->mag?"with":"without");
      if (nbhd->mag) {
         for (ii=0; ii<nbhd->num_pt; ++ii) {
            fprintf (fout, "Offset[I J K]: %+03d %+03d %+03d, Mag: %f\n", 
                           nbhd->i[ii], nbhd->j[ii], nbhd->k[ii], nbhd->mag[ii]);
         }
      } else {
         for (ii=0; ii<nbhd->num_pt; ++ii) {
            fprintf (fout, "Offset[I J K]: %+03d %+03d %+03d\n", 
                        nbhd->i[ii], nbhd->j[ii], nbhd->k[ii]);
         }
      }
   }
   if (closing) fprintf(fout, "%s", closing);
   return;
}
