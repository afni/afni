/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

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

#include "mrilib.h"
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

#define KILL_CLUSTER(cc)       \
  do{ if( cc != NULL ){        \
         myXtFree((cc)->i) ;   \
         myXtFree((cc)->j) ;   \
         myXtFree((cc)->k) ;   \
         myXtFree((cc)->mag) ; \
         myXtFree((cc)) ;      \
         (cc) = NULL ;         \
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

/* 16 June 1998 */
extern void * MCW_erode_clusters (int, int, int, float, float, float, int,
				  void *, float, float, int);

/*----------------------------------------------------------------------------*/
#undef ALLOW_SCALE_TO_MAX
char * EDIT_options_help(void) ;  /* was a string, now a prototype */
/*----------------------------------------------------------------------------*/

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

   float erode_pv;                /* --> erosion percentage   (16 June 1998)  */
   int dilate;                    /* --> dilation option      (16 June 1998)  */


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

   int   zscore ;                 /* 17 Sep 1998 --> convert statistic to Z   */

   int   verbose ;                /* 01 Nov 1999 --> verbose output during editing */

   int   nfmask ;                 /* 09 Aug 2000 --> filter mask */
   byte * fmask ;
   char * fexpr ;                 /* 09 Aug 2000 --> filter expression */

   int fake_dxyz ;                /* 11 Sep 2000 -> use dx=dy=dz=1.0? */

} EDIT_options ;

/*--- cluster editing options ---*/   /* 10 Sept 1996 */
#define ECFLAG_NONE   0
#define ECFLAG_SAME   1
#define ECFLAG_MEAN   2
#define ECFLAG_MAX    3
#define ECFLAG_AMAX   4
#define ECFLAG_SMAX   5
#define ECFLAG_SIZE   6
#define ECFLAG_ORDER  7         /* 09 June 1998 */

/*--- filtering options ---*/   /* 11 Sept 1996 */
#define FCFLAG_NONE   0
#define FCFLAG_MEAN   1
#define FCFLAG_NZMEAN 2
#define FCFLAG_MAX    3
#define FCFLAG_AMAX   4
#define FCFLAG_SMAX   5

#define FCFLAG_AVER   66        /*  7 Jan 1998 */
#define FCFLAG_EXPR   77        /* 09 Aug 2000 */

#define FCFLAG_ONE_STEP 100000
#define FCFLAG_WINSOR   (2*FCFLAG_ONE_STEP)  /* 11 Sep 2000 */

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
	(edopt)->erode_pv      = 0.0 , \
	(edopt)->dilate        = 0   , \
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
        (edopt)->iv_thr        = -1  , \
        (edopt)->zscore        = 0   , \
        (edopt)->verbose       = 0   , \
        (edopt)->nfmask        = 0   , \
        (edopt)->fmask         = NULL, \
        (edopt)->fexpr         = NULL, \
        (edopt)->fake_dxyz     = 0   , \
       0 )

extern void EDIT_one_dataset( THD_3dim_dataset * dset , EDIT_options * edopt ) ;

extern void EDIT_blur_volume( int,int,int , float,float,float , int,void * , float ) ;
extern void EDIT_blur_volume_3d( int,int,int , float,float,float , int,void * , float, float, float ) ;

#define RMS_TO_SIGMA(rms) (0.57735027*(rms))  /* 1/sqrt(3) */
#define FWHM_TO_SIGMA(fh) (0.42466090*(fh))   /* 1/sqrt(log(2)*8) */

extern int EDIT_check_argv( int , char * argv[] , int , EDIT_options * ) ;

extern void EDIT_coerce_type      ( int , int,void * , int,void * ) ;
extern void EDIT_coerce_scale_type( int , float , int,void * , int,void * ) ;
extern float EDIT_coerce_autoscale( int , int,void * , int,void * ) ;

extern void EDIT_aver_fvol( int, int, int,
                            float, float, float, float *, float) ;

extern void EDIT_zscore_vol( int,int,float,void *,int,float * ) ;

extern void EDIT_clip_float( float , int , float * ) ;

extern byte * EDT_calcmask( char * , int * ) ;  /* 16 Mar 2000 */

extern void * EDIT_volpad( int,int,int,int,int,int ,
                           int,int,int , int,void * ) ; /* 09 Feb 2001 */

#define EDIT_zeropad EDIT_volpad                        /* 14 Feb 2001 */

#define EDIT_volpad_even(px,py,pz,nx,ny,nz,ft,vv) \
   EDIT_volpad( (px),(px), (py),(py), (pz),(pz), (nx),(ny),(nz), (ft),(vv) )

/********************* New routines for AFNI-96 ****************************/

/**----------------------- prototypes -----------------------**/

extern THD_3dim_dataset * EDIT_empty_copy( THD_3dim_dataset * ) ;
extern THD_3dim_dataset * EDIT_full_copy ( THD_3dim_dataset * , char * ) ;
extern int                EDIT_dset_items( THD_3dim_dataset * , ... ) ;

extern void EDIT_add_bricklist( THD_3dim_dataset *,int,int *,float *,void *sbr[] ) ;

extern void EDIT_add_brick( THD_3dim_dataset * , int , float , void * ) ;

extern void EDIT_substitute_brick( THD_3dim_dataset * ,  int,int , void * ) ;

/* 10 Sept 1996 */
extern void EDIT_cluster_array (MCW_cluster_array * , int, float, float);

/* 11 Sept 1996 */
extern void EDIT_filter_volume (int, int, int, float, float, float,
                                int, void *, int, float , byte * , char * );

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
#define ADN_anatpar_idcode       6065     /*=  MCW_idcode * [13 Dec 1999] =*/

/* 30 Nov 1997 */
#define ADN_ONE_STEP            100000
#define ADN_brick_label_one             (2*ADN_ONE_STEP)  /*=  char *   =*/
#define ADN_brick_fac_one               (3*ADN_ONE_STEP)  /*=  float    =*/
#define ADN_brick_stataux_one           (4*ADN_ONE_STEP)  /*=  float *  =*/
#define ADN_brick_keywords_replace_one  (5*ADN_ONE_STEP)  /*=  char *   =*/
#define ADN_brick_keywords_append_one   (6*ADN_ONE_STEP)  /*=  char *   =*/

/*-----------------------------------------------------------------
   These 2 macros added 14 Dec 1999
-------------------------------------------------------------------*/

#define EDIT_COPY_ANATOMY_PARENT_ID(nds,ods)                   \
  do{ if( ISVALID_DSET(nds) && ISVALID_DSET(ods) )              \
         (nds)->anat_parent_idcode = (ods)->anat_parent_idcode ; \
    } while(0)

#define EDIT_ZERO_ANATOMY_PARENT_ID(nds)                 \
  do{ if( ISVALID_DSET(nds) )                             \
         ZERO_IDCODE((nds)->anat_parent_idcode); } while(0)

/*------------------------------------------------------------------*/

#define EDIT_STATAUX4(ds,iv,ft,a,b,c,d)                     \
 do{ float sqq[6] ;                                           \
     if( ISVALID_DSET(ds) &&                                    \
         (iv) >= 0 && (iv) < DSET_NVALS(ds) &&                    \
         (ft) >= 0 && (ft) <= LAST_FUNC_TYPE   ){                   \
        sqq[0] = (ft) ; sqq[1] = FUNC_need_stat_aux[ft] ;             \
        sqq[2] = (a) ; sqq[3] = (b) ; sqq[4] = (c) ; sqq[5] = (d) ;     \
        EDIT_dset_items( (ds),ADN_brick_stataux_one+(iv),sqq,ADN_none ) ; \
     } } while(0)

#define EDIT_BRICK_TO_FICO(ds,iv,nsam,nfit,nort) \
  EDIT_STATAUX4(ds,iv,FUNC_COR_TYPE,nsam,nfit,nort,0)

#define EDIT_BRICK_TO_FITT(ds,iv,ndof) \
  EDIT_STATAUX4(ds,iv,FUNC_TT_TYPE,ndof,0,0,0)

#define EDIT_BRICK_TO_FIFT(ds,iv,ndof,ddof) \
  EDIT_STATAUX4(ds,iv,FUNC_FT_TYPE,ndof,ddof,0,0)

#define EDIT_BRICK_TO_FIZT(ds,iv) \
  EDIT_STATAUX4(ds,iv,FUNC_ZT_TYPE,0,0,0,0)

#define EDIT_BRICK_TO_FICT(ds,iv,ndof) \
  EDIT_STATAUX4(ds,iv,FUNC_CT_TYPE,ndof,0,0,0)

#define EDIT_BRICK_TO_FIBT(ds,iv,a,b) \
    EDIT_STATAUX4(ds,iv,FUNC_BT_TYPE,a,b,0,0)

#define EDIT_BRICK_TO_FIBN(ds,iv,ntrial,prob) \
    EDIT_STATAUX4(ds,iv,FUNC_BN_TYPE,ntrial,prob,0,0)

#define EDIT_BRICK_TO_FIGT(ds,iv,shape,scale) \
    EDIT_STATAUX4(ds,iv,FUNC_GT_TYPE,shape,scale,0,0)

#define EDIT_BRICK_TO_FIPT(ds,iv,mean) \
    EDIT_STATAUX4(ds,iv,FUNC_PT_TYPE,mean,0,0,0)

/*------------------------------------------------------------------*/

#define EDIT_BRICK_LABEL(ds,iv,str) \
     EDIT_dset_items( (ds), ADN_brick_label_one+(iv), (str), ADN_none )

#define EDIT_BRICK_FACTOR(ds,iv,fac) \
     EDIT_dset_items( (ds), ADN_brick_fac_one+(iv), (fac), ADN_none )

#define EDIT_BRICK_ADDKEY(ds,iv,str) \
     EDIT_dset_items( (ds), ADN_brick_keywords_append_one+(iv), (str), ADN_none )

/*------------------------------------------------------------------*/

#endif /* _MCW_EDITVOL_ */
