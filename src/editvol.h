#ifndef _MCW_EDITVOL_
#define _MCW_EDITVOL_

#undef CLUST_DEBUG

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>      /* to fix a bug in gcc */
#include <stddef.h>
#include <X11/Intrinsic.h>  /* only for XtFree, etc */
#include <stdarg.h>         /* for variable number of arguments processing */

#include "3ddata.h"
#include "afni_warp.h"

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

#ifndef myXtNew
#define myXtNew(type) ((type *) XtCalloc(1,(unsigned) sizeof(type)))
#endif

#define INC_CLUSTER 8

/** In a cluster struct, the (i,j,k) indexes for each voxel
    are stored in a single integer ijk (to save space).
    The macros below translate between (i,j,k) [THREE] and ijk. **/

#define IJK_TO_THREE(ijk,i,j,k,nx,nxy) \
  ( (k) = (ijk)/(nxy) , (j)=((ijk)%(nxy))/(nx) , (i)=(ijk)%(nx) )

#define THREE_TO_IJK(i,j,k,nx,nxy) ((i)+(j)*(nx)+(k)*(nxy))

/*
   The cluster structure was modified to store the individual coordinate
   indices for each voxel.  This avoids ambiguity in voxel identification.
   BDW, 06 March 1997
*/

typedef struct {
   int num_pt , num_all ;
   short * i;       /* store indices separately */
   short * j;
   short * k;
   float * mag ;    /* stores value at each voxel */
} MCW_cluster ;

#define INIT_CLUSTER(cc)               \
  ( (cc) = XtNew(MCW_cluster) ,        \
    (cc)->num_pt = (cc)->num_all = 0 , \
    (cc)->i = NULL , (cc)->j = NULL , (cc)->k = NULL ,(cc)->mag = NULL )

#define KILL_CLUSTER(cc)             \
  do{ if( cc != NULL ){              \
         myXtFree((cc)->i) ;   \
         myXtFree((cc)->j) ;   \
         myXtFree((cc)->k) ;   \
         myXtFree((cc)->mag) ; \
         myXtFree((cc)) ;      \
         (cc) = NULL ;               \
      } break ; } while(0)

#ifdef CLUST_DEBUG
#  define DBMALL(n) printf(" -- Realloc-ing cluster: %d\n",(n))
#else
#  define DBMALL(n)
#endif

#define ADDTO_CLUSTER(cc,ii,jj,kk,m) \
  do{ int nn ;                                              \
      if( (cc)->num_pt == (cc)->num_all ){                  \
         (cc)->num_all = 1.25*(cc)->num_all + INC_CLUSTER ; \
         nn = (cc)->num_all ;                               \
         (cc)->i=(short *)   XtRealloc((char *)(cc)->i,sizeof(short)*nn  );\
         (cc)->j=(short *)   XtRealloc((char *)(cc)->j,sizeof(short)*nn  );\
         (cc)->k=(short *)   XtRealloc((char *)(cc)->k,sizeof(short)*nn  );\
         (cc)->mag=(float *) XtRealloc((char *)(cc)->mag,sizeof(float)*nn);\
         DBMALL(nn) ; }                      \
      nn = (cc)->num_pt ; ((cc)->num_pt)++ ; \
      (cc)->i[nn] = (ii) ; (cc)->j[nn] = (jj) ; (cc)->k[nn] = (kk) ; \
      (cc)->mag[nn] = (m) ; break ; } while(0)

/*----------------------------------------------------------------------------*/

typedef struct {
   int num_clu , num_all ;
   MCW_cluster ** clar ;
} MCW_cluster_array ;

#define INIT_CLARR(cl)                \
  ( (cl) = XtNew(MCW_cluster_array) , \
    (cl)->num_clu = (cl)->num_all = 0 , (cl)->clar = NULL )

#define ADDTO_CLARR(cl,cc)                  \
  do{ int nn ;                              \
      if( (cl)->num_clu == (cl)->num_all ){ \
         (cl)->num_all += INC_CLUSTER ; nn = (cl)->num_all ;           \
         (cl)->clar = (MCW_cluster **) XtRealloc( (char *)(cl)->clar , \
                                                 sizeof(MCW_cluster *) * nn ) ;\
      } \
      (cl)->clar[((cl)->num_clu)++] = (cc) ; break ; } while(0)

#define DESTROY_CLARR(cl) \
  do{ int ii ; if( cl != NULL ){                    \
         for( ii=0 ; ii < (cl)->num_clu ; ii++ )    \
            KILL_CLUSTER( (cl)->clar[ii] ) ;        \
         myXtFree((cl)->clar) ; (cl) = NULL ; \
      } break ; } while(0)

#define CLUST_ORDERED(c1,c2) ( (c1)->num_pt >= (c2)->num_pt )

#define CLUST_SWAP(c1,c2) (ct=(c1),(c1)=(c2),(c2)=ct,sss=1)

#define SORT_CLARR(name) \
   if( (name) != NULL && (name)->num_clu > 1 ){                             \
      int iic , jjc , sss ; MCW_cluster * ct ;                              \
      for( iic=0 ; iic < (name)->num_clu ; iic++ ){                         \
         sss = 0 ;                                                          \
         for( jjc=1 ; jjc < (name)->num_clu ; jjc++ ){                      \
            if( !CLUST_ORDERED( (name)->clar[jjc-1] , (name)->clar[jjc] ) ) \
               CLUST_SWAP( (name)->clar[jjc-1] , (name)->clar[jjc] ) ;      \
   } if( !sss ) break ; }}

/*----------------------------------------------------------------------------*/

extern MCW_cluster_array * MCW_find_clusters( int,int,int , float,float,float ,
                                              int , void * , float ) ;

extern void MCW_cluster_to_vol( int,int,int, int,void *, MCW_cluster * ) ;

extern void MCW_scale_to_max( int,int,int, int,void * );

extern float MCW_vol_amax( int,int,int , int,void *) ;

/* 11 Sept 1996 */
extern MCW_cluster * MCW_build_mask (int, int, int,
                                     float, float, float, float);

/*----------------------------------------------------------------------------*/

#undef ALLOW_SCALE_TO_MAX

#ifdef NEED_EDIT_HELP
static char * EDIT_options_help =
    "EDITING OPTIONS APPLIED TO EACH INPUT DATASET:\n"
    "  -1thtoin         = Copy threshold data over intensity data.\n"
    "                       This is only valid for datasets with some\n"
    "                       thresholding statistic attached.  All\n"
    "                       subsequent operations apply to this\n"
    "                       substituted data.\n"
    "  -2thtoin         = The same as -1thtoin, but do NOT scale the\n"
    "                       threshold values from shorts to floats when\n"
    "                       processing.  This option is only provided\n"
    "                       for compatibility with the earlier versions\n"
    "                       of the AFNI package '3d*' programs.\n"
    "  -1noneg          = Zero out voxels with negative intensities\n"
    "  -1abs            = Take absolute values of intensities\n"
    "  -1clip val       = Clip intensities in range (-val,val) to zero\n"
    "  -2clip v1 v2     = Clip intensities in range (v1,v2) to zero\n"
    "  -1uclip val      = These options are like the above, but do not apply\n"
    "  -2uclip v1 v2        any automatic scaling factor that may be attached\n"
    "                       to the data.  These are for use only in special\n"
    "                       circumstances.  (The 'u' means 'unscaled'.  Program\n"
    "                       '3dinfo' can be used to find the scaling factors.)\n"
    "               N.B.: Only one of these 'clip' options can be used; you cannot\n"
    "                       combine them to have multiple clipping executed.\n"
    "  -1thresh thr     = Use the threshold data to censor the intensities\n"
    "                       (only valid for 'fith', 'fico', or 'fitt' datasets).\n"
    "               N.B.: The value 'thr' is floating point, in the range\n"
    "                           0.0 < thr < 1.0  for 'fith' and 'fico' datasets,\n"
    "                       and 0.0 < thr < 32.7 for 'fitt' datasets.\n"
    "  -1blur_sigma bmm = Gaussian blur with sigma = bmm (in mm)\n"
    "  -1blur_rms bmm   = Gaussian blur with rms deviation = bmm\n"
    "  -1blur_fwhm bmm  = Gaussian blur with FWHM = bmm\n"
    "  -t1blur_sigma bmm= Gaussian blur of threshold with sigma = bmm(in mm)\n"
    "  -t1blur_rms bmm  = Gaussian blur of threshold with rms deviation = bmm\n"
    "  -t1blur_fwhm bmm = Gaussian blur of threshold with FWHM = bmm\n"
    "  -1zvol x1 x2 y1 y2 z1 z2\n"
    "                   = Zero out entries inside the 3D volume defined\n"
    "                       by x1 <= x <= x2, y1 <= y <= y2, z1 <= z <= z2 ;\n"
    "               N.B.: The ranges of x,y,z in a dataset can be found\n"
    "                       using the '3dinfo' program. Dimensions are in mm.\n"
    "               N.B.: This option may not work correctly at this time, but\n"
    "                       I've not figured out why!\n"
    " \n"
    "  The following cluster options are mutually exclusive: \n"
    "  -1clust rmm vmul = Form clusters with connection distance rmm\n"
    "                       and clip off data not in clusters of\n"
    "                       volume at least vmul microliters\n"
    "  -1clust_mean rmm vmul = Same as -1clust, but all voxel intensities \n"
    "                            within a cluster are replaced by the average\n"
    "                            intensity of the cluster. \n"
    "  -1clust_max rmm vmul  = Same as -1clust, but all voxel intensities \n"
    "                            within a cluster are replaced by the maximum\n"
    "                            intensity of the cluster. \n"
    "  -1clust_amax rmm vmul = Same as -1clust, but all voxel intensities \n"
    "                            within a cluster are replaced by the maximum\n"
    "                            absolute intensity of the cluster. \n"
    "  -1clust_smax rmm vmul = Same as -1clust, but all voxel intensities \n"
    "                            within a cluster are replaced by the maximum\n"
    "                            signed intensity of the cluster. \n"
    "  -1clust_size rmm vmul = Same as -1clust, but all voxel intensities \n"
    "                            within a cluster are replaced by the size \n"
    "                            of the cluster (in multiples of vmul ).   \n"
    " \n"
    "  The following filter options are mutually exclusive: \n"
    "  -1filter_mean rmm   = Set each voxel to the average intensity of the \n"
    "                          voxels within a radius of rmm. \n"
    "  -1filter_nzmean rmm = Set each voxel to the average intensity of the \n"
    "                          non-zero voxels within a radius of rmm. \n"
    "  -1filter_max rmm    = Set each voxel to the maximum intensity of the \n"
    "                          voxels within a radius of rmm. \n"
    "  -1filter_amax rmm   = Set each voxel to the maximum absolute intensity\n"
    "                          of the voxels within a radius of rmm. \n"
    "  -1filter_smax rmm   = Set each voxel to the maximum signed intensity \n"
    "                          of the voxels within a radius of rmm. \n"
    " \n"
    "  The following threshold filter options are mutually exclusive: \n"
    "  -t1filter_mean rmm   = Set each correlation or threshold voxel to the \n"
    "                          average of the voxels within a radius of rmm. \n"
    "  -t1filter_nzmean rmm = Set each correlation or threshold voxel to the \n"
    "                          average of the non-zero voxels within \n"
    "                          a radius of rmm. \n"
    "  -t1filter_max rmm    = Set each correlation or threshold voxel to the \n"
    "                          maximum of the voxels within a radius of rmm. \n"
    "  -t1filter_amax rmm   = Set each correlation or threshold voxel to the \n"
    "                          maximum absolute intensity of the voxels \n"
    "                          within a radius of rmm. \n"
    "  -t1filter_smax rmm   = Set each correlation or threshold voxel to the \n"
    "                          maximum signed intensity of the voxels \n"
    "                          within a radius of rmm. \n"
    " \n"


#ifdef ALLOW_SCALE_TO_MAX
    "  -1scale          = Linearly scale intensities so that max is 10000\n"
#endif
    "  -1mult factor    = Multiply intensities by the given factor\n"
    "\n"
    "The above '-1' options are carried out in the order given above,\n"
    "regardless of the order in which they are entered on the command line.\n"
    "\n"
    "N.B.: The 3 '-1blur' options just provide different ways of\n"
    "      specifying the radius used for the blurring function.\n"
    "      The relationships among these specifications are\n"
    "         sigma = 0.57735027 * rms = 0.42466090 * fwhm\n"
    "      The requisite convolutions are done using FFTs; this is by\n"
    "      far the slowest operation among the editing options.\n"
;
#endif

/** data structure filled in EDIT_check_argv,
    and used to control EDIT_one_dataset (options applied in order given) **/

typedef struct EDIT_options {
   int thtoin ,                   /* --> copy thresh data over intensity data */
       noneg ,                    /* --> throw away negative intensities      */
       abss ;                     /* --> take absolute values of intensities  */

   float clip_bot , clip_top ;    /* --> zero out voxels with v in (bot,top)  */
   int   clip_unscaled ;          /* --> clip without scaling? [09 Aug 1996]  */

   float thresh ,                 /* --> zero out if threshold < thresh       */
         clust_rmm , clust_vmul , /* --> cluster data and edit by vmul        */
         blur ,                   /* --> Gaussian blur data with sigma = blur */
         thrblur ;                /* --> Gaussian blur threshold data,
                                         with sigma = thrblur (4 Oct 1996)    */

   int edit_clust;                /* --> edit cluster option  (10 Sept 1996)  */

   float filter_rmm;              /* --> filter radius        (11 Sept 1996)  */
   int   filter_opt;              /* --> filter option        (11 Sept 1996)  */

   float thrfilter_rmm;           /* --> threshold filter radius (1 Oct 1996) */
   int   thrfilter_opt;           /* --> threshold filter option (1 Oct 1996) */

   int   scale ;                  /* --> linearly scale data so max = 10000   */

   float mult ;                   /* --> multiply all voxels by this          */

   int   do_zvol ;                /* --> zero out a 3D sub-volume             */
   float zv_x1 , zv_x2 ,          /* --> dimensions of sub-volume to massacre */
         zv_y1 , zv_y2 ,
         zv_z1 , zv_z2  ;

   int   iv_fim , iv_thr ;        /* 30 Nov 1997 --> use these sub-bricks     */

} EDIT_options ;

/*--- cluster editing options ---*/   /* 10 Sept 1996 */
#define ECFLAG_NONE   0
#define ECFLAG_SAME   1
#define ECFLAG_MEAN   2
#define ECFLAG_MAX    3
#define ECFLAG_AMAX   4
#define ECFLAG_SMAX   5
#define ECFLAG_SIZE   6

/*--- filtering options ---*/   /* 11 Sept 1996 */
#define FCFLAG_NONE   0
#define FCFLAG_MEAN   1
#define FCFLAG_NZMEAN 2
#define FCFLAG_MAX    3
#define FCFLAG_AMAX   4
#define FCFLAG_SMAX   5

#define INIT_EDOPT(edopt)              \
      ( (edopt)->thtoin        = 0   , \
        (edopt)->noneg         = 0   , \
        (edopt)->abss          = 0   , \
        (edopt)->clip_bot      = 0.0 , \
        (edopt)->clip_top      = 0.0 , \
        (edopt)->thresh        = 0.0 , \
        (edopt)->clust_rmm     = 0.0 , \
        (edopt)->clust_vmul    = 0.0 , \
        (edopt)->edit_clust    = 0   , \
        (edopt)->filter_rmm    = 0.0 , \
        (edopt)->filter_opt    = 0   , \
        (edopt)->thrfilter_rmm = 0.0 , \
        (edopt)->thrfilter_opt = 0   , \
        (edopt)->blur          = 0.0 , \
        (edopt)->thrblur       = 0.0 , \
        (edopt)->scale         = 0   , \
        (edopt)->mult          = 0.0 , \
        (edopt)->do_zvol       = 0   , \
        (edopt)->clip_unscaled = 0   , \
        (edopt)->iv_fim        = -1  , \
        (edopt)->iv_thr        = -1    \
      )

extern void EDIT_one_dataset( THD_3dim_dataset * dset , EDIT_options * edopt ) ;

extern void EDIT_blur_volume( int,int,int , float,float,float , int,void * , float ) ;
extern void EDIT_blur_volume_3d( int,int,int , float,float,float , int,void * , float, float, float ) ;

#define RMS_TO_SIGMA(rms) (0.57735027*(rms))  /* 1/sqrt(3) */
#define FWHM_TO_SIGMA(fh) (0.42466090*(fh))   /* 1/sqrt(log(2)*8) */

extern int EDIT_check_argv( int , char * argv[] , int , EDIT_options * ) ;

extern void EDIT_coerce_type      ( int , int,void * , int,void * ) ;
extern void EDIT_coerce_scale_type( int , float , int,void * , int,void * ) ;
extern float EDIT_coerce_autoscale( int , int,void * , int,void * ) ;

/********************* New routines for AFNI-96 ****************************/

/**----------------------- prototypes -----------------------**/

extern THD_3dim_dataset * EDIT_empty_copy( THD_3dim_dataset * ) ;
extern int                EDIT_dset_items( THD_3dim_dataset * , ... ) ;

extern void EDIT_add_bricklist( THD_3dim_dataset *,int,int *,float *,void *sbr[] ) ;

extern void EDIT_add_brick( THD_3dim_dataset * , int , float , void * ) ;

extern void EDIT_substitute_brick( THD_3dim_dataset * ,  int,int , void * ) ;

/* 10 Sept 1996 */
extern void EDIT_cluster_array (MCW_cluster_array * , int, float, float);

/* 11 Sept 1996 */
extern void EDIT_filter_volume (int, int, int, float, float, float,
                                int, void *, int, float);

/**---------------- AFNI Dataset item Names ----------------**/

#define ADN_none                 0

/** values in the diskptr **/

#define ADN_prefix               6001     /*=  char *  =*/
#define ADN_directory_name       6002     /*=  char *  =*/

/** values in the datablock **/

#define ADN_brick_fac            6011     /*=  float *  =*/
#define ADN_malloc_type          6012     /*=  int      =*/
#define ADN_datum_all            6013     /*=  int      =*/
#define ADN_datum_array          6014     /*=  int *    =*/
#define ADN_nvals                6016     /*=  int      =*/

/** values in the dataxes **/

#define ADN_nxyz                 6020     /*=  THD_ivec3  =*/
#define ADN_xyzdel               6021     /*=  THD_fvec3  =*/
#define ADN_xyzorg               6022     /*=  THD_fvec3  =*/
#define ADN_xyzorient            6023     /*=  THD_ivec3  =*/
#define ADN_to_dicomm            6024     /*=  THD_mat33  =*/

/** values in the timeaxis **/

#define ADN_ntt                  6031     /*=  int    =*/
#define ADN_ttorg                6032     /*=  float  =*/
#define ADN_ttdel                6033     /*=  float  =*/
#define ADN_ttdur                6034     /*=  float  =*/

#define ADN_nsl                  6035     /*=  int      =*/
#define ADN_zorg_sl              6036     /*   float    =*/
#define ADN_dz_sl                6037     /*   float    =*/
#define ADN_toff_sl              6039     /*=  float *  =*/
#define ADN_tunits               6040     /*=  int      =*/  /* 21 Oct 1996 */

/** values in the 3dim_dataset itself **/

#define ADN_type                 6051     /*=  int     =*/
#define ADN_view_type            6052     /*=  int     =*/
#define ADN_func_type            6053     /*=  int     =*/
#define ADN_label1               6054     /*=  char *  =*/
#define ADN_label2               6055     /*=  char *  =*/
#define ADN_self_name            6056     /*=  char *  =*/
#define ADN_keywords_replace     6057     /*=  char *  =*/
#define ADN_keywords_append      6058     /*=  char *  =*/

#define ADN_warp_parent          6061     /*=  THD_3dim_dataset *  =*/
#define ADN_anat_parent          6062     /*=  THD_3dim_dataset *  =*/
#define ADN_stat_aux             6063     /*=  float *             =*/
#define ADN_warp                 6064     /*=  THD_warp *          =*/

/* 30 Nov 1997 */
#define ADN_ONE_STEP            100000
#define ADN_brick_label_one             (2*ADN_ONE_STEP)  /*=  char *   =*/
#define ADN_brick_fac_one               (3*ADN_ONE_STEP)  /*=  float    =*/
#define ADN_brick_stataux_one           (4*ADN_ONE_STEP)  /*=  float *  =*/
#define ADN_brick_keywords_replace_one  (5*ADN_ONE_STEP)  /*=  char *   =*/
#define ADN_brick_keywords_append_one   (6*ADN_ONE_STEP)  /*=  char *   =*/

/*------------------------------------------------------------------*/

#endif /* _MCW_EDITVOL_ */
