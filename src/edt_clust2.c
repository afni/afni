#include "mrilib.h"

/*----------------------------------------------------------------*/
/*! Modified MCW_find_clusters():
    - added mode variable
    - ISOVALUE_MODE => make clusters that are both contiguous
                       and have the same numerical value
    - ISOMERGE_MODE => make clusters that have the same numerical
                       value, regardless of contiguity
------------------------------------------------------------------*/

MCW_cluster_array * NIH_find_clusters(
                       int nx, int ny, int nz,
                       float dx, float dy, float dz,
                       int ftype , void * fim ,
                       float max_dist , int mode )
{
   MCW_cluster_array *clust_arr ;
   MCW_cluster       *clust , *mask=NULL ;
   int ii,jj,kk ,  nxy,nxyz , ijk , ijk_last , mnum ;
   int icl , jma , ijkcl , ijkma , did_one ;
   float fimv ;
   short *sfar ;
   float *ffar ;
   byte  *bfar ;
   short ic, jc, kc;
   short im, jm, km;

ENTRY("NIH_find_clusters") ;

   if( fim == NULL ) RETURN(NULL) ;

   switch( ftype ){
      default: RETURN(NULL) ;
      case MRI_short:  sfar = (short *) fim ; break ;
      case MRI_byte :  bfar = (byte  *) fim ; break ;
      case MRI_float:  ffar = (float *) fim ; break ;
   }

   /* default => use older code (in edt_clust.c) */

   if( mode <= 0 || mode > ISOMERGE_MODE ){
     RETURN( MCW_find_clusters( nx,ny,nz , dx,dy,dz ,
                               ftype,fim , max_dist ) ) ;
   }

   /*--- make a cluster that is a mask of points closer than max_dist ---*/

   if( mode == ISOVALUE_MODE ){
     mask = MCW_build_mask (nx, ny, nz, dx, dy, dz, max_dist);
     if (mask == NULL)
     {
        fprintf(stderr, "Unable to build mask in NIH_find_clusters");
        RETURN(NULL);
     }
     mnum = mask->num_pt ;
   }

   nxy = nx*ny ; nxyz = nxy * nz ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

   INIT_CLARR(clust_arr) ;

   ijk_last = 0 ;
   do {

      /* find nonzero point in 3D array, starting at ijk_last */

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
      if( ijk == nxyz ) break ;  /* didn't find any nonzero point! */

      ijk_last = ijk+1 ;         /* start here next time */

      INIT_CLUSTER(clust) ;                        /* make a new cluster */
      IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;          /* find 3D index */
      ADDTO_CLUSTER( clust , ic, jc, kc, fimv ) ;  /* start cluster off */

      switch( mode ){

        case ISOVALUE_MODE:{ /* for each point in cluster:
                                check points offset by mask for nonzero entries in fim
                                enter those into cluster
                                continue until end of cluster is reached
                                (note that cluster is expanding as we progress) */

         switch( ftype ){
            case MRI_short:
               for( icl=0 ; icl < clust->num_pt ; icl++ ){
     	          ic = clust->i[icl];  /* check around this point */
	          jc = clust->j[icl];
	          kc = clust->k[icl];

                  for( jma=0 ; jma < mnum ; jma++ ){
                     im = ic + mask->i[jma];  /* offset by mask */
		     jm = jc + mask->j[jma];
		     km = kc + mask->k[jma];
                     if( im < 0 || im >= nx ||
                         jm < 0 || jm >= ny || km < 0 || km >= nz ) continue ;

		     ijkma = THREE_TO_IJK (im, jm, km, nx, nxy);
                     if( ijkma < ijk_last || ijkma >= nxyz || sfar[ijkma] != fimv ) continue ;

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
                     if( ijkma < ijk_last || ijkma >= nxyz || bfar[ijkma] != fimv ) continue ;

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
                     if( ijkma < ijk_last || ijkma >= nxyz || ffar[ijkma] != fimv ) continue ;

		     ADDTO_CLUSTER( clust , im, jm, km, ffar[ijkma] ) ;
                     ffar[ijkma] = 0.0 ;
                  }
               }
            break ;
         } /* end of switch on array type */
        }
        break ; /* end of ISOVALUE_MODE */

        /*.................................*/

        case ISOMERGE_MODE:{  /* find other points with the same value */

         switch( ftype ){
            case MRI_short:
              for( ijk=ijk_last ; ijk < nxyz ; ijk++ )
                if( sfar[ijk] == fimv ){
                  IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;          /* find 3D index */
                  ADDTO_CLUSTER( clust , ic, jc, kc, fimv ) ;  /* start cluster off */
                  sfar[ijk] = 0 ;
                }
            break ;

            case MRI_byte:
              for( ijk=ijk_last ; ijk < nxyz ; ijk++ )
                if( bfar[ijk] == fimv ){
                  IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;          /* find 3D index */
                  ADDTO_CLUSTER( clust , ic, jc, kc, fimv ) ;  /* start cluster off */
                  bfar[ijk] = 0 ;
                }
            break ;

            case MRI_float:
              for( ijk=ijk_last ; ijk < nxyz ; ijk++ )
                if( ffar[ijk] == fimv ){
                  IJK_TO_THREE(ijk,ic,jc,kc,nx,nxy) ;          /* find 3D index */
                  ADDTO_CLUSTER( clust , ic, jc, kc, fimv ) ;  /* start cluster off */
                  ffar[ijk] = 0 ;
                }
            break ;
         }
        }
        break ; /* end of ISOMERGE_MODE */

      }

      ADDTO_CLARR(clust_arr,clust) ;
   } while( 1 ) ;

   if( mask != NULL ) KILL_CLUSTER(mask) ;

   if( clust_arr->num_clu <= 0 ){ DESTROY_CLARR(clust_arr) ; }

   RETURN(clust_arr) ;
}
