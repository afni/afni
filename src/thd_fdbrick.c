#include "3ddata.h"
#include "thd.h"


/*-----------------------------------------------------------------------
  Create FD_bricks for viewing purposes.

  FD_bricks are a relic of the very earliest code that evolved into
  AFNI.  By the time I invented THD_3dim_datasets, I had enough code
  in place to keep this data structure alive.  Basically, it exists
  only to provide a structure that enables fast copying of data out
  of a 3D array into 2D arrays.
-------------------------------------------------------------------------*/

FD_brick ** THD_setup_bricks( THD_3dim_dataset * dset )
{
   int r2l=0 , a2p=0 , i2s=0 ;
   THD_dataxes * daxes ;
   FD_brick ** br ;

ENTRY("THD_setup_bricks") ;

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   daxes = CURRENT_DAXES(dset) ;
   if( ! ISVALID_DATAXES(daxes) ) return NULL ;

#ifdef THD_DEBUG
  printf(" -- input orientation codes for dataset %s\n"
         "     #1 = %d = %s\n"
         "     #2 = %d = %s\n"
         "     #3 = %d = %s\n" ,
         dset->dblk->diskptr->filecode ,
         daxes->xxorient , ORIENT_typestr[daxes->xxorient] ,
         daxes->yyorient , ORIENT_typestr[daxes->yyorient] ,
         daxes->zzorient , ORIENT_typestr[daxes->zzorient]  ) ; fflush(stdout) ;
#endif

   /*----- create FD_bricks for viewing purposes -----*/

   switch( daxes->xxorient ){
      case ORI_R2L_TYPE: r2l =  1 ; break ;
      case ORI_L2R_TYPE: r2l = -1 ; break ;
      case ORI_P2A_TYPE: a2p = -1 ; break ;
      case ORI_A2P_TYPE: a2p =  1 ; break ;
      case ORI_I2S_TYPE: i2s =  1 ; break ;
      case ORI_S2I_TYPE: i2s = -1 ; break ;
   }

   switch( daxes->yyorient ){
      case ORI_R2L_TYPE: r2l =  2 ; break ;
      case ORI_L2R_TYPE: r2l = -2 ; break ;
      case ORI_P2A_TYPE: a2p = -2 ; break ;
      case ORI_A2P_TYPE: a2p =  2 ; break ;
      case ORI_I2S_TYPE: i2s =  2 ; break ;
      case ORI_S2I_TYPE: i2s = -2 ; break ;
   }

   switch( daxes->zzorient ){
      case ORI_R2L_TYPE: r2l =  3 ; break ;
      case ORI_L2R_TYPE: r2l = -3 ; break ;
      case ORI_P2A_TYPE: a2p = -3 ; break ;
      case ORI_A2P_TYPE: a2p =  3 ; break ;
      case ORI_I2S_TYPE: i2s =  3 ; break ;
      case ORI_S2I_TYPE: i2s = -3 ; break ;
   }

   if( r2l==0 || a2p==0 || i2s==0 ){
      char buf[256] ;
      sprintf(buf,"Illegal orientation codes: %d %d %d",
                  daxes->xxorient,daxes->yyorient,daxes->zzorient ) ;
      THD_FATAL_ERROR(buf) ;
   }

   /* now we can set up views: axial, sagittal, coronal;
      the top option is the way I think it ought to be, so that
      in the axial and coronal views, left is left and right is right;
      the bottom options are the radiologists conventions, so sue me! */

   br = (FD_brick **) XtMalloc( sizeof(FD_brick *) * 3 ) ;

#ifdef LEFT_IS_LEFT_AND_RIGHT_IS_RIGHT
   br[0] = THD_3dim_dataset_to_brick(dset,-r2l, a2p,-i2s); /* axi */
   br[1] = THD_3dim_dataset_to_brick(dset, a2p,-i2s,-r2l); /* sag */
   br[2] = THD_3dim_dataset_to_brick(dset,-r2l,-i2s,-a2p); /* cor */
#else
   br[0] = THD_3dim_dataset_to_brick(dset, r2l, a2p, i2s); /* axi */
   br[1] = THD_3dim_dataset_to_brick(dset, a2p,-i2s,-r2l); /* sag */
   br[2] = THD_3dim_dataset_to_brick(dset, r2l,-i2s, a2p); /* cor */
#endif

   strcpy( br[0]->namecode , "Axial" ) ;
   strcpy( br[1]->namecode , "Sagittal" ) ;
   strcpy( br[2]->namecode , "Coronal" ) ;

   return br ;
}

/*======================================================================
    routines adapted from original afni code for display of a 3D dataset
========================================================================*/

/*----------------------------------------------------------------------
  prepare a "display brick" for a 3D dataset;
    ax_n = 1,2,3 for displaying the x,y,z axis along the n-th display
              direction (n=1 --> across, n=2 --> down, n=3 --> slices )
           negative value means reverse

   a return of NULL means something bad happened
------------------------------------------------------------------------*/

FD_brick * THD_3dim_dataset_to_brick( THD_3dim_dataset * dset ,
                                      int ax_1, int ax_2, int ax_3 )
{
   FD_brick      * br ;    /* will be output */
   THD_dataxes   * daxes ; /* connection to actual axes */

   int   xyz_dim[4] , xyz_stp[4] , xyz_dir[4] ;
   float xyz_del[4] , xyz_org[4] ;

   int x_dir,y_dir,z_dir , sx,sy,sz , aax_1,aax_2,aax_3 , nx,ny,nz ;

ENTRY("THD_3dim_dataset_to_brick") ;

   /*-- sanity check --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   daxes = CURRENT_DAXES(dset) ;

   aax_1 = abs(ax_1) ;  /* which axes, regardless of + or - */
   aax_2 = abs(ax_2) ;
   aax_3 = abs(ax_3) ;

   if( aax_1 < 1 || aax_1 > 3 ||   /* range checks */
       aax_2 < 1 || aax_2 > 3 ||
       aax_3 < 1 || aax_3 > 3   ) return NULL ;

   xyz_dir[1] = xyz_dir[2] = xyz_dir[3] = 0 ;

   xyz_dir[aax_1] = ax_1 ;  /* assign to original directions */
   xyz_dir[aax_2] = ax_2 ;
   xyz_dir[aax_3] = ax_3 ;

   x_dir = xyz_dir[1] ;  /* if any |ax_n| is duplicated */
   y_dir = xyz_dir[2] ;  /* then one of these will end  */
   z_dir = xyz_dir[3] ;  /* up as zero --> bad inputs!  */

   if( x_dir == 0 || y_dir == 0 || z_dir == 0 ) return NULL ;

   /*-- the inputs are good, so create a brick: --*/

   br             = myXtNew(FD_brick) ;  /* new brick */
   br->dset       = dset ;             /* dataset */
   br->resam_code = RESAM_NN_TYPE ;    /* crudest type */
   br->parent     = NULL ;

   br->thr_resam_code = RESAM_NN_TYPE ;    /* 09 Dec 1997 */

   /*-- at this point, x_dir is +1 or -1, y_dir is +2 or -2, etc. --*/

   nx = daxes->nxx ; ny = daxes->nyy ; nz = daxes->nzz ;

   sx = (x_dir > 0) ? (0) : (nx-1) ;  /* starting voxel indices */
   sy = (y_dir > 0) ? (0) : (ny-1) ;  /* for each original dimension */
   sz = (z_dir > 0) ? (0) : (nz-1) ;

   br->start = sx + sy*nx + sz*nx*ny ; /* overall starting voxel index */

   /*-- assign original dimensions to arrays,
        then pick out the permuted dimensions --*/

   xyz_dim[1] = nx ;  /* dimensions */
   xyz_dim[2] = ny ;
   xyz_dim[3] = nz ;

   LOAD_IVEC3( br->nxyz , nx,ny,nz ) ;       /* save stuff in br */
   LOAD_IVEC3( br->sxyz , sx,sy,sz ) ;
   LOAD_IVEC3( br->a123 , ax_1,ax_2,ax_3 ) ;

   xyz_stp[1] = 1 ;            /* index step sizes */
   xyz_stp[2] = nx ;
   xyz_stp[3] = nx * ny ;

   xyz_del[1] = daxes->xxdel ; /* voxel physical step sizes (mm) */
   xyz_del[2] = daxes->yydel ;
   xyz_del[3] = daxes->zzdel ;

   xyz_org[1] = daxes->xxorg ; /* voxel origins (mm) */
   xyz_org[2] = daxes->yyorg ;
   xyz_org[3] = daxes->zzorg ;

   br->n1 = xyz_dim[aax_1] ;  /* permute dimensions, etc. */
   br->n2 = xyz_dim[aax_2] ;
   br->n3 = xyz_dim[aax_3] ;

   br->d1 = (ax_1 > 0) ? (xyz_stp[aax_1]) : (-xyz_stp[aax_1]) ;
   br->d2 = (ax_2 > 0) ? (xyz_stp[aax_2]) : (-xyz_stp[aax_2]) ;
   br->d3 = (ax_3 > 0) ? (xyz_stp[aax_3]) : (-xyz_stp[aax_3]) ;

   br->e1 = br->n1 * br->d1 ;  /* last indices for readout */
   br->e2 = br->n2 * br->d2 ;

   br->del1 = fabs(xyz_del[aax_1]) ;  /* dimensions */
   br->del2 = fabs(xyz_del[aax_2]) ;
   br->del3 = fabs(xyz_del[aax_3]) ;

#ifdef THD_DEBUG
  printf(" -- new brick has:\n"
         "     ax_1 ax_2 ax_3 = %3d %3d %3d\n"
         "     n1   d1   e1   = %3d %3d %3d\n"
         "     n2   d2   e2   = %3d %3d %3d\n"
         "     n3   d3 start  = %3d %3d %3d\n" ,
         ax_1,ax_2,ax_3 ,
         br->n1,br->d1,br->e1,br->n2,br->d2,br->e2,br->n3,br->d3,br->start ) ;
#endif

   br->namecode[0] = '\0' ;

   return br ;
}
