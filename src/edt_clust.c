#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)

/*----------------------------------------------------------------
   find clusters of points !=0; return an array of clusters
   [the input array fim is destroyed in this
    computation; it may be recreated by MCW_cluster_to_vol]

   Nov 1995: fim array may now be short, byte, or float,
             signified by new ftype argument.

   Coordinates of voxels in clusters are now stored as 3 separate
   short integers, to correct error due to abiguity in
   identification of clusters.
   BDW  06 March 1997
------------------------------------------------------------------*/

MCW_cluster_array * MCW_find_clusters(
                       int nx, int ny, int nz,
                       float dx, float dy, float dz,
                       int ftype , void * fim ,
                       float max_dist )
{
   MCW_cluster_array * clust_arr ;
   MCW_cluster       * clust , * mask ;
   int ii,jj,kk ,  nxy,nxyz , ijk , ijk_last , mnum ;
   int icl , jma , ijkcl , ijkma , did_one ;
   float fimv ;
   short * sfar ;
   float * ffar ;
   byte  * bfar ;
   short ic, jc, kc;
   short im, jm, km;

   if( fim == NULL || max_dist <= 0.0 ) return NULL ;

   switch( ftype ){
      default: return NULL ;
      case MRI_short:  sfar = (short *) fim ; break ;
      case MRI_byte :  bfar = (byte  *) fim ; break ;
      case MRI_float:  ffar = (float *) fim ; break ;
   }

   /*--- make a cluster that is a mask of points closer than max_dist ---*/
   mask = MCW_build_mask (nx, ny, nz, dx, dy, dz, max_dist);
   if (mask == NULL)
   {
      fprintf (stderr, "Unable to build mask in MCW_find_clusters");
      return NULL;
   }

   nxy = nx*ny ; nxyz = nxy * nz ;

   mnum = mask->num_pt ;


   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

   INIT_CLARR(clust_arr) ;

   ijk_last = 0 ;
   do {
      switch( ftype ){
         case MRI_short:
            for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( sfar[ijk] != 0 ) break ;
            if( ijk < nxyz ){
               fimv = sfar[ijk] ; sfar[ijk] = 0 ;  /* save found point */
            }
         break ;

         case MRI_byte:
            for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( bfar[ijk] != 0 ) break ;
            if( ijk < nxyz ){
               fimv = bfar[ijk] ; bfar[ijk] = 0 ;  /* save found point */
            }
         break ;

         case MRI_float:
            for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( ffar[ijk] != 0.0 ) break ;
            if( ijk < nxyz ){
               fimv = ffar[ijk] ; ffar[ijk] = 0.0 ;  /* save found point */
            }
         break ;
      }
      if( ijk == nxyz ) break ;  /* didn't find any! */

#ifdef CLUST_DEBUG
printf("  starting cluster at ijk=%d\n",ijk) ;
#endif

      ijk_last = ijk+1 ;         /* start here next time */

      INIT_CLUSTER(clust) ;                  /* make a new cluster */
      IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;
      ADDTO_CLUSTER( clust , ic, jc, kc, fimv ) ;  /* start it off */

      /*--
        for each point in cluster:
           check points offset by the mask for nonzero entries in fim
           enter those into cluster
           continue until end of cluster is reached
             (note that cluster is expanding as we progress)
      --*/

      switch( ftype ){
         case MRI_short:
            for( icl=0 ; icl < clust->num_pt ; icl++ ){
     	       ic = clust->i[icl];
	       jc = clust->j[icl];
	       kc = clust->k[icl];

               for( jma=0 ; jma < mnum ; jma++ ){
                  im = ic + mask->i[jma];
		  jm = jc + mask->j[jma];
		  km = kc + mask->k[jma];
                  if( im < 0 || im >= nx ||
                      jm < 0 || jm >= ny || km < 0 || km >= nz ) continue ;

		  ijkma = THREE_TO_IJK (im, jm, km, nx, nxy);
                  if( ijkma < ijk_last || ijkma >= nxyz || sfar[ijkma] == 0 ) continue ;

                  ADDTO_CLUSTER( clust , im, jm, km, sfar[ijkma] ) ;
                  sfar[ijkma] = 0 ;
               }
            }
         break ;

         case MRI_byte:
            for( icl=0 ; icl < clust->num_pt ; icl++ ){
 	       ic = clust->i[icl];
	       jc = clust->j[icl];
	       kc = clust->k[icl];

               for( jma=0 ; jma < mnum ; jma++ ){
		  im = ic + mask->i[jma];
		  jm = jc + mask->j[jma];
		  km = kc + mask->k[jma];
                  if( im < 0 || im >= nx ||
                      jm < 0 || jm >= ny || km < 0 || km >= nz ) continue ;

		  ijkma = THREE_TO_IJK (im, jm, km, nx, nxy);
                  if( ijkma < ijk_last || ijkma >= nxyz || bfar[ijkma] == 0 ) continue ;

                  ADDTO_CLUSTER( clust , im, jm, km, bfar[ijkma] ) ;
                  bfar[ijkma] = 0 ;
               }
            }
         break ;

         case MRI_float:
            for( icl=0 ; icl < clust->num_pt ; icl++ ){
	       ic = clust->i[icl];
	       jc = clust->j[icl];
	       kc = clust->k[icl];

               for( jma=0 ; jma < mnum ; jma++ ){
		  im = ic + mask->i[jma];
		  jm = jc + mask->j[jma];
		  km = kc + mask->k[jma];
		  if( im < 0 || im >= nx ||
		      jm < 0 || jm >= ny || km < 0 || km >= nz ) continue ;

		  ijkma = THREE_TO_IJK (im, jm, km, nx, nxy);
                  if( ijkma < ijk_last || ijkma >= nxyz || ffar[ijkma] == 0.0 ) continue ;

		  ADDTO_CLUSTER( clust , im, jm, km, ffar[ijkma] ) ;
                  ffar[ijkma] = 0.0 ;
               }
            }
         break ;
      }

      ADDTO_CLARR(clust_arr,clust) ;
   } while( 1 ) ;

   KILL_CLUSTER(mask) ;

   if( clust_arr->num_clu <= 0 ){ DESTROY_CLARR(clust_arr) ; }

   return clust_arr ;
}

/*---------------------------------------------------------------
  Write the points stored in a cluster back into a volume

   Coordinates of voxels in clusters are now stored as 3 separate
   short integers, to correct error due to abiguity in
   identification of clusters.
   BDW  06 March 1997
-----------------------------------------------------------------*/

void MCW_cluster_to_vol( int nx , int ny , int nz ,
                         int ftype , void * fim , MCW_cluster * clust )
{
   int icl, ijk ;
   int nxy ;
   short * sfar ;
   float * ffar ;
   byte  * bfar ;

   if( fim == NULL || clust == NULL ) return ;

   nxy = nx * ny;

   switch( ftype ){
      case MRI_short:
         sfar = (short *) fim ;
         for( icl=0 ; icl < clust->num_pt ; icl++ )
	   {
	     ijk = THREE_TO_IJK (clust->i[icl], clust->j[icl], clust->k[icl],
				 nx, nxy);
	     sfar[ijk] = clust->mag[icl] ;
	   }
      return ;

      case MRI_byte:
         bfar = (byte *) fim ;
         for( icl=0 ; icl < clust->num_pt ; icl++ )
	   {
	     ijk = THREE_TO_IJK (clust->i[icl], clust->j[icl], clust->k[icl],
				 nx, nxy);
	     bfar[ijk] = clust->mag[icl] ;
	   }
      return ;

      case MRI_float:
         ffar = (float *) fim ;
         for( icl=0 ; icl < clust->num_pt ; icl++ )
	   {
	     ijk = THREE_TO_IJK (clust->i[icl], clust->j[icl], clust->k[icl],
				 nx, nxy);
	     ffar[ijk] = clust->mag[icl] ;
	   }
      return ;
   }

   return ;  /* should not be reached */
}
