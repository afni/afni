/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <string.h>

#include "thd_shear3d.h"  /* 06 Feb 2001 */

#define ERREX(str) (fprintf(stderr,"** %s\n",str),exit(1))

/******* global data *******/

/** define results of scanning the command line **/

static int     Iarg = 1 ;
static int     Argc ;
static char ** Argv ;

static int         VL_nbase  = 0 ;
static int         VL_intern = 1 ;
static int         VL_resam  = MRI_HEPTIC ;  /* from MRI_FOURIER 08 Mar 2016 */
static int         VL_final  = -1 ;   /* 20 Nov 1998 */
static int         VL_clipit = 1 ;    /* 23 Oct 1998 and 16 Apr 2002 */
static MRI_IMAGE * VL_imbase = NULL ;
static MRI_IMAGE * VL_imwt   = NULL ;
static int         VL_wtinp  = 0 ;    /* 06 Jun 2002 */

static int         VL_zpad   = 0 ;    /* 05 Feb 2001 */

static int         VL_twopass= 0 ;    /* 11 Sep 2000 */
static float       VL_twoblur= 2.0 ;
static int         VL_twosum = 1 ;    /* 08 Dec 2000 */
static int         VL_twodup = 0 ;
static float       VL_bxorg, VL_byorg, VL_bzorg ;

static float       VL_edging ;        /* 10 Dec 2000 */
static int         VL_edperc=-1 ;

static int         VL_coarse_del=10 ; /* 11 Dec 2000 */
static int         VL_coarse_num=2  ;
static int         VL_coarse_rot=1  ; /* 01 Dec 2005 */

static THD_fvec3 VL_cen_bas , VL_cen_inp ; /* 11 Mar 2008 */
static float     VL_cen_dist = 0.0f ;

static THD_3dim_dataset *VL_dset = NULL ;
static THD_3dim_dataset *VL_bset = NULL ;  /* 06 Feb 2001 */

static char VL_prefix[256] = "volreg" ;
static int  VL_verbose     = 0 ;
static char VL_dfile[256]  = "\0" ;
static char VL_1Dfile[256] = "\0" ;  /* 14 Apr 2000 */

static int VL_maxite = 19 ;
static float VL_dxy  = 0.02;  /* voxels */
static float VL_dph  = 0.03 ;  /* degrees */
static float VL_del  = 0.70 ;  /* voxels */

static int VL_rotcom = 0 ;     /* 04 Sep 2000: print out 3drotate commands? */

static int VL_maxdisp= 1 ;     /* 03 Aug 2006: print out max displacment? */
static THD_fvec3 *VL_dispvec=NULL ;
static char *VL_commandline = NULL ;

static float  VL_dmax          = 0.0f ;
static int    VL_dmaxi         = 0    ;
static char   VL_dmaxfile[999] = "\0" ;
static float *VL_dmaxar        = NULL ;

static float  VL_emax          = 0.0f ;   /* 22 Jun 2015 */
static int    VL_emaxi         = 0    ;
static char   VL_emaxfile[999] = "\0" ;
static float *VL_emaxar        = NULL ;
static THD_dfvec3 *VL_emaxvec  = NULL ;

static char *VL_savedisp          = NULL ;  /* 04 Apr 2012: save all displacements */
static int   VL_savedisp19        = 0    ;
static char *VL_savedisp_prA      = NULL ;
static char *VL_savedisp_prB      = NULL ;
static THD_3dim_dataset *VL_xdset = NULL ;
static THD_3dim_dataset *VL_ydset = NULL ;
static THD_3dim_dataset *VL_zdset = NULL ;

static THD_3dim_dataset *VL_rotpar_dset =NULL ,  /* 14 Feb 2001 */
                        *VL_gridpar_dset=NULL ;

static int VL_tshift        = 0 ,                /* 15 Feb 2001 */
           VL_tshift_ignore = 0  ;

static int VL_sinit = 1 ;                        /* 22 Mar 2004 */

static char *VL_matrix_save_1D = NULL ;          /* 24 Jul 2007 */
static FILE *VL_msfp = NULL ;

static int VL_floatize = 0 ;                     /* 07 Jan 2008 */
static int VL_floatize_forced = 0 ;

/******* prototypes *******/

void VL_syntax(void) ;
void VL_command_line(void) ;

float voldif( int nx, int ny, int nz, float *b,
              int dx, int dy, int dz, float *v, int edge ) ;

float get_best_shift( int nx, int ny, int nz,
                      float *b, float *v, int *dxp,int *dyp,int *dzp ) ;

float new_get_best_shiftrot( THD_3dim_dataset *dset ,
                             MRI_IMAGE *base , MRI_IMAGE *vol ,
                             float *roll , float *pitch , float *yaw ,
                             int   *dxp  , int   *dyp   , int   *dzp  ) ;

float ** VL_get_displacments( THD_3dim_dataset *, THD_dmat33, THD_dfvec3 ) ;

void VL_normalize_timeseries( THD_3dim_dataset *dset ) ;

void * VL_create_disprod( THD_3dim_dataset *tdset ,
                          THD_3dim_dataset *xdset , int xp ,
                          THD_3dim_dataset *ydset , int yp ,
                          THD_3dim_dataset *zdset , int zp  ) ;

/**********************************************************************/
/***************************** the program! ***************************/

int main( int argc , char *argv[] )
{
   MRI_3dalign_basis *albase ;
   THD_3dim_dataset *new_dset ;
   MRI_IMAGE *qim , *tim , *fim ;
   double cputim=0.0 ;
   float *dx, *dy, *dz, *roll, *yaw, *pitch, *rmsnew, *rmsold, *imb, *tar ;
   float ddx,ddy,ddz , sum , fac ;
   float dxtop=0.0,dytop=0.0,dztop=0.0 , rolltop=0.0,yawtop=0.0,pitchtop=0.0 ;
   float dxbot=0.0,dybot=0.0,dzbot=0.0 , rollbot=0.0,yawbot=0.0,pitchbot=0.0 ;
   float dxbar,dybar,dzbar , rollbar,yawbar,pitchbar ;
   int kim,ii , imcount , iha , ax1,ax2,ax3 , hax1,hax2,hax3 ;

   float *dx_1=NULL,*dy_1=NULL,*dz_1=NULL, *roll_1=NULL,*yaw_1=NULL,*pitch_1=NULL ;  /* 11 Sep 2000 */
   int   nx,ny,nz ;
   int   null_output=0 ;

   static char * modes[] = {
        "-NN" , "-linear" , "-cubic" , "-Fourier" , "-quintic" , "-heptic" } ;

#define MATVEC_DICOM 1
#define MATVEC_ORDER 2

   int matvec=0 ;              /* 14 Feb 2001 */
   THD_dmat33 rmat , pp,ppt ;  /* rmat = "extra" rotation matrix at end */
   THD_dfvec3 tvec ;           /* tvec = "extra" translation vector at end */
   int npad_neg=0 ,            /* zero-padding needed, -z and +z axes */
       npad_pos=0 , npadd=0 ;

   /*-- handle command line options --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){ VL_syntax() ; exit(0); }

   mainENTRY("3dvolreg main") ; machdep() ; AFNI_logger("3dvolreg",argc,argv) ;
   PRINT_VERSION("3dvolreg") ; AUTHOR("RW Cox") ; THD_check_AFNI_version("3dvolreg") ;
   memset(&tvec, 0, sizeof(THD_dfvec3)); memset(&rmat, 0, sizeof(THD_dmat33));
   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   Argc = argc ; Argv = argv ; Iarg = 1 ;
   VL_command_line() ;

   mri_3dalign_wtrimming(1) ;  /* 22 Mar 2004: always turn this on */

   /*-- setup the registration algorithm parameters --*/

   imcount = DSET_NVALS( VL_dset ) ;
   dx      = (float *) malloc( sizeof(float) * imcount ) ;
   dy      = (float *) malloc( sizeof(float) * imcount ) ;
   dz      = (float *) malloc( sizeof(float) * imcount ) ;
   roll    = (float *) malloc( sizeof(float) * imcount ) ;
   pitch   = (float *) malloc( sizeof(float) * imcount ) ;
   yaw     = (float *) malloc( sizeof(float) * imcount ) ;
   rmsnew  = (float *) malloc( sizeof(float) * imcount ) ;
   rmsold  = (float *) malloc( sizeof(float) * imcount ) ;

   iha = THD_handedness( VL_dset )   ;                     /* LH or RH? */
   ax1 = THD_axcode( VL_dset , 'I' ) ; hax1 = ax1 * iha ;  /* roll */
   ax2 = THD_axcode( VL_dset , 'R' ) ; hax2 = ax2 * iha ;  /* pitch */
   ax3 = THD_axcode( VL_dset , 'A' ) ; hax3 = ax3 * iha ;  /* yaw */

   /*-- create the output dataset --*/

   new_dset = EDIT_empty_copy( VL_dset ) ;  /* not much here yet */

   /*-- 14 Feb 2001: if have -gridparent, might need to zeropad output --*/

   if( VL_gridpar_dset != NULL ){
      int mm , nz_gp , nz_ds ;

      /* first, check for compatibility! */

      mm = THD_dataset_mismatch( VL_gridpar_dset , VL_dset ) ;
      if( mm & (MISMATCH_DELTA | MISMATCH_ORIENT) ){
        fprintf(stderr,"** Fatal Error:\n"
                       "** -gridparent dataset and input dataset don't\n"
                       "** match in grid spacing and/or orientation!\n"  ) ;
        exit(1) ;
      }

      if( DSET_NX(VL_gridpar_dset) != DSET_NX(VL_dset) ||
          DSET_NY(VL_gridpar_dset) != DSET_NY(VL_dset)   ){

        fprintf(stderr,"** Fatal Error:\n"
                       "** -gridparent and input datasets\n"
                       "** don't match in x,y dimensions!\n" ) ;
        exit(1) ;
      }

      /* check for zero padding requirment */

      nz_gp = DSET_NZ(VL_gridpar_dset) ; nz_ds = DSET_NZ(VL_dset) ;

      if( nz_gp < nz_ds ){
        fprintf(stderr,"** Fatal Error:\n"
                       "** -gridparent has fewer slices than input dataset!\n") ;
        exit(1) ;
      }
      if( nz_gp > nz_ds ){                    /* must zeropad */
        int npad1 = (nz_gp - nz_ds) / 2 ;     /* negative z padding */
        int npad2 = (nz_gp - nz_ds) - npad1 ; /* positive z padding */
        int add_I=0, add_S=0, add_A=0, add_P=0, add_L=0, add_R=0 ;
        THD_3dim_dataset * pset ;
        char *sp1=NULL,*sp2=NULL ;

        /* where to add slices? and how many? */

        switch( VL_dset->daxes->zzorient ){
          case ORI_R2L_TYPE:
          case ORI_L2R_TYPE: add_R=npad1; add_L=npad2; sp1="R"; sp2="L"; break;

          case ORI_P2A_TYPE:
          case ORI_A2P_TYPE: add_A=npad1; add_P=npad2; sp1="A"; sp2="P"; break;

          case ORI_I2S_TYPE:
          case ORI_S2I_TYPE: add_I=npad1; add_S=npad2; sp1="I"; sp2="S"; break;
        }

        /* set padding globals */

        switch( ORIENT_sign[VL_dset->daxes->zzorient] ){
          default:
          case '+': npad_neg = npad1 ; npad_pos = npad2 ; break ;
          case '-': npad_neg = npad2 ; npad_pos = npad1 ; break ;
        }
        npadd = (npad_neg > 0 || npad_pos > 0 ) ;  /* flag for later padding */

        /* add them to output, in a virtual (empty dataset) sense */

        if( VL_verbose )
          fprintf(stderr,"++ Zero padding to match -gridparent: -%s %d  -%s %d\n",
                  sp1,npad1,sp2,npad2 ) ;

        pset = THD_zeropad( new_dset,
                            add_I,add_S,add_A,add_P,add_L,add_R,
                            NULL , ZPAD_EMPTY ) ;

        if( pset == NULL ){
          fprintf(stderr,"** Fatal Error:\n"
                         "** Can't properly zeropad output dataset!\n" ) ;
          exit(1) ;
        }

        /* replace output dataset with padded dataset */

        DSET_delete(new_dset); new_dset = pset;
      }
   }

   /*-- set some information into the new dataset's header --*/

   if( strcmp(VL_prefix,"NULL") == 0 ){  /* 24 Jul 2007 */
     WARNING_message("No output dataset will be calculated") ;
     null_output = 1 ;
   } else {
     EDIT_dset_items( new_dset , ADN_prefix , VL_prefix , ADN_none ) ;
     if( THD_deathcon() && THD_is_file( DSET_HEADNAME(new_dset) ) ){
       ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(new_dset) ) ;
     }
     EDIT_dset_items( new_dset, ADN_brick_fac,NULL, ADN_none ); /* 07 Jan 2008 */
   }

   tross_Copy_History( VL_dset , new_dset ) ;
   tross_Make_History( "3dvolreg" , argc,argv , new_dset ) ;
   VL_commandline = tross_commandline( "3dvolreg" , argc,argv ) ;

   /*-- 14 Feb 2001: compute -rotparent/-gridparent transformation --*/

   if( VL_rotpar_dset != NULL ){
      ATR_float *atr ;
      float *matar , sum ;
      THD_fvec3 fv ;
      THD_dfvec3 dv,ev,qv , cv_e2, cv_e1, cv_s1, cv_s2 ;

      /* load (Dicom-order) transformation from rotparent */

      atr = THD_find_float_atr( VL_rotpar_dset->dblk , "VOLREG_MATVEC_000000" );
      matar = atr->fl ;
      LOAD_DMAT(rmat,matar[0],matar[1],matar[2],    /* rmat = rotation matrix */
                     matar[4],matar[5],matar[6],
                     matar[8],matar[9],matar[10] ) ;
      LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ; /* tvec = shift vector */

      /* check if [rmat] is orthogonal */

      pp = TRANSPOSE_DMAT(rmat) ; pp = DMAT_MUL(pp,rmat) ;
      sum = fabs(pp.mat[0][0]-1.)+fabs(pp.mat[1][0])   +fabs(pp.mat[2][0])
           +fabs(pp.mat[0][1])   +fabs(pp.mat[1][1]-1.)+fabs(pp.mat[2][1])
           +fabs(pp.mat[0][2])   +fabs(pp.mat[1][2])   +fabs(pp.mat[2][2]-1.) ;
      if( sum > 0.01 ) ERREX("-rotparent matrix not orthogonal!") ;

      /* must alter shift [tvec] to allow for differing
         coordinates in the rotparent, gridparent, and input datasets */

      /* cv_e2 = center of input dataset (Dicom coordinates) */

      fv = THD_dataset_center( new_dset ) ;
      FVEC3_TO_DFVEC3( fv , cv_e2 ) ;       /* convert to double */

      /* cv_e1 = center of gridparent */

      if( VL_gridpar_dset != NULL ){
         fv = THD_dataset_center( VL_gridpar_dset ) ;
         FVEC3_TO_DFVEC3( fv , cv_e1 ) ;
      } else {
         cv_e1 = cv_e2 ;  /* no gridparent: what else to do? */
      }

      /* cv_s2 = center of rotation in rotparent */

      atr = THD_find_float_atr( VL_rotpar_dset->dblk , "VOLREG_CENTER_OLD" ) ;
      LOAD_DFVEC3( cv_s2 , atr->fl[0] , atr->fl[1] , atr->fl[2] ) ;

      /* cv_s1 = center of base dataset for rotparent */

      atr = THD_find_float_atr( VL_rotpar_dset->dblk , "VOLREG_CENTER_BASE" );
      LOAD_DFVEC3( cv_s1 , atr->fl[0] , atr->fl[1] , atr->fl[2] ) ;

      /* compute extra shift due to difference in
         center of rotation between rotparent and input dataset,
         then add in shifts caused by -twodup for rotparent and input */

      dv = SUB_DFVEC3( cv_e2 , cv_s2 ) ;
      ev = DMATVEC( rmat , dv ) ;         /* R[E2-S2]         */

      dv = ev ;  /* vestige of a stupid bug, since fixed */

      ev = SUB_DFVEC3( cv_e1 , cv_s1 ) ;  /* E1-S1            */

      qv = SUB_DFVEC3( dv , ev ) ;        /* R[E2-S2] + S1-E1 */

      tvec = ADD_DFVEC3( tvec , qv ) ;    /* shifted translation vector */

      /* convert transformation from Dicom to dataset coords */

      pp   = DBLE_mat_to_dicomm( new_dset ) ;
      ppt  = TRANSPOSE_DMAT(pp);
      rmat = DMAT_MUL(ppt,rmat); rmat = DMAT_MUL(rmat,pp);
      tvec = DMATVEC(ppt,tvec);

      /* modify origin of output dataset to match -gridparent */

      if( VL_gridpar_dset != NULL ){
         new_dset->daxes->xxorg = VL_gridpar_dset->daxes->xxorg ;
         new_dset->daxes->yyorg = VL_gridpar_dset->daxes->yyorg ;
         new_dset->daxes->zzorg = VL_gridpar_dset->daxes->zzorg ;

         /* 12 Feb 2001: adjust origin of time-offsets as well */

         if( new_dset->taxis != NULL && new_dset->taxis->nsl > 0 ){
            new_dset->taxis->zorg_sl = new_dset->daxes->zzorg ;
         }
      }

      matvec = MATVEC_ORDER ;  /* flag that transform comes from rmat/tvec */
   }

   /*-- 14 Feb 2001: adjust time-offsets for slice direction shifts --*/

   if( new_dset->taxis != NULL && new_dset->taxis->nsl > 0 && matvec ){
     int ndz ;
     int kk,jj , nsl = new_dset->taxis->nsl ;

     ndz = (int)rint( tvec.xyz[2] / fabs(new_dset->daxes->zzdel) ); /* shift */

     if( ndz != 0 ){
       float * tsl = (float *)malloc(sizeof(float)*nsl) ;
       for( kk=0 ; kk < nsl ; kk ++ ){
         jj = kk - ndz ;
         if( jj < 0 || jj >= nsl ) tsl[kk] = 0.0 ;
         else                      tsl[kk] = new_dset->taxis->toff_sl[jj] ;
       }
       EDIT_dset_items( new_dset , ADN_toff_sl , tsl , ADN_none ) ;
       free(tsl) ;
       if( VL_verbose )
         fprintf(stderr,"++ adjusting time-offsets by %d slices\n",ndz) ;
     }
   }

   /*-- read the input dataset into memory --*/

   if( VL_verbose ){
     if( VL_tshift )
       fprintf(stderr,"++ Time shifting input dataset %s\n",
               DSET_BRIKNAME(VL_dset)) ;
     else
       fprintf(stderr,"++ Reading input dataset %s\n",
               DSET_BRIKNAME(VL_dset)) ;
   }

   if( VL_tshift ){
      int eee = THD_dataset_tshift( VL_dset , VL_tshift_ignore ) ;
      if( eee )
         fprintf(stderr,"++ WARNING: some error during -tshift operation!\n") ;
      else
         EDIT_dset_items( new_dset ,
                            ADN_nsl    , 0   ,  /* has no offsets now */
                            ADN_ttorg  , 0.0 ,  /* in case not already set */
                            ADN_ttdur  , 0.0 ,  /* in case not already set */
                          ADN_none ) ;
   }
   DSET_load( VL_dset ) ;

   /*-- initialize the registration algorithm --*/

   if( VL_imbase == NULL ){
     fac = DSET_BRICK_FACTOR(VL_dset,VL_nbase) ;
     VL_imbase = mri_scale_to_float(fac,DSET_BRICK(VL_dset,VL_nbase)) ;
   } else {
     VL_nbase = -1 ;  /* will not match any sub-brick index */
   }

   { int nnz = mri_nonzero_count(VL_imbase) ;
     if( nnz < 100 )
       ERROR_exit("3dvolreg fails: base image has %d nonzero voxel%s (< 100)",
                  nnz , (nnz==1) ? "\0" : "s" ) ;
   }

   VL_imbase->dx = fabs( DSET_DX(VL_dset) ) ;  /* set the voxel dimensions */
   VL_imbase->dy = fabs( DSET_DY(VL_dset) ) ;  /* in the MRI_IMAGE struct  */
   VL_imbase->dz = fabs( DSET_DZ(VL_dset) ) ;
   imb = MRI_FLOAT_PTR( VL_imbase ) ;          /* need this to compute rms */

   nx = DSET_NX(VL_dset) ; ny = DSET_NY(VL_dset) ; nz = DSET_NZ(VL_dset) ;

   /*-- 10 Dec 2000: set edging in the alignment function --*/

   { int xf=0,yf=0,zf=0 ;
     switch( VL_edperc ){
        case 0:
           xf = (int)( MIN(0.25*nx,VL_edging) ) ;
           yf = (int)( MIN(0.25*ny,VL_edging) ) ;
           zf = (int)( MIN(0.25*nz,VL_edging) ) ;
        break ;

        case 1:
           xf = (int)( 0.01*VL_edging*nx + 0.5 ) ;
           yf = (int)( 0.01*VL_edging*ny + 0.5 ) ;
           zf = (int)( 0.01*VL_edging*nz + 0.5 ) ;
        break ;
     }
     mri_3dalign_edging(xf,yf,zf) ;
     if( VL_verbose )
        fprintf(stderr,"++ Edging: x=%d y=%d z=%d\n",xf,yf,zf) ;
   }

   /*--- 03 Aug 2006: create a set of vectors to look for maxdisp ---*/

#undef  DSK
#define DSK(i,j,k) dsk[(i)+(j)*nx+(k)*nxy]
   if( VL_maxdisp ){
     byte *dsk , *msk=NULL ;
     if( VL_verbose ){
       INFO_message("Creating mask for -maxdisp") ; THD_automask_verbose(0) ;
     }
     THD_automask_set_clipfrac(0.333f) ;
     dsk = THD_automask( VL_dset ) ;
     if( dsk != NULL ){
       int ii,jj,kk, mm, nxy=nx*ny, nxyz=nxy*nz, ip,jp,kp, im,jm,km, nmsk=0 ;
       THD_ivec3 iv ;
       msk = (byte *)calloc(1,nxyz) ;
       if( VL_verbose )
         ININFO_message("Automask has %d voxels",THD_countmask(nxyz,dsk)) ;
       for( mm=0 ; mm < nxyz ; mm++ ){
         if( dsk[mm] == 0 ) continue ;                      /* not in the mask */
         ii = mm % nx ; kk = mm / nxy ; jj = (mm%nxy) / nx ;  /* voxel indexes */
         ip=ii+1; im=ii-1; if(ip>=nx || im<0){ msk[mm]=1; nmsk++; continue; }
         jp=jj+1; jm=jj-1; if(jp>=ny || jm<0){ msk[mm]=1; nmsk++; continue; }
         kp=kk+1; km=kk-1; if(kp>=nz || km<0){ msk[mm]=1; nmsk++; continue; }
         if( DSK(ip,jj,kk) && DSK(im,jj,kk) &&              /* if all 6 nbhrs  */
             DSK(ii,jp,kk) && DSK(ii,jm,kk) &&              /* are in automask */
             DSK(ii,jj,kp) && DSK(ii,jj,km)   ) continue ;  /* skip this voxel */
         msk[mm] = 1 ; nmsk++ ;
       }
       if( VL_verbose )
         ININFO_message("%d voxels left in -maxdisp mask after erosion",nmsk) ;
       free(dsk) ;
       VL_maxdisp = nmsk ;
       if( nmsk > 0 ){
         int qq ;
         VL_dispvec = (THD_fvec3 *)malloc(sizeof(THD_fvec3)*nmsk) ;
         for( qq=mm=0 ; mm < nxyz ; mm++ ){
           if( msk[mm] == 0 ) continue ;
           ii = mm % nx ; kk = mm / nxy ; jj = (mm%nxy) / nx ;
           iv.ijk[0] = ii ; iv.ijk[1] = jj ; iv.ijk[2] = kk ;
           VL_dispvec[qq++] = THD_3dind_to_3dmm_no_wod( VL_dset , iv ) ;
         }
       }
       free(msk) ;
     } else {
       VL_maxdisp = 0 ; WARNING_message("Can't create -maxdisp mask?!") ;
     }
   }

   /*--- 11 Sep 2000: if in twopass mode, do the first pass ---*/

   if( VL_twopass ){
      MRI_IMAGE *tp_base ;
      int sx=66666,sy,sz ;
      float rr=0.0f,pp=0.0f,yy=0.0f ;  /* 01 Dec 2005 */

      if( VL_verbose ){
        fprintf(stderr,"++ Start of first pass alignment on all sub-bricks\n");
        cputim = COX_cpu_time();
      }

      tp_base = mri_to_float(VL_imbase) ;  /* make a copy, blur it */

      EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 ,
                           MRI_float , MRI_FLOAT_PTR(tp_base) ,
                           VL_twoblur,VL_twoblur,VL_twoblur ) ;

      mri_3dalign_params( VL_maxite ,
                          VL_twoblur*VL_dxy, VL_twoblur*VL_dph,
                          VL_twoblur*VL_del,
                          abs(ax1)-1 , abs(ax2)-1 , abs(ax3)-1 , -1 ) ;

                                              /* no reg | */
                                              /*        V */
      mri_3dalign_method( MRI_LINEAR , (VL_verbose>1) , 1 , 0 ) ;

      /* 08 Dec 2000: (perhaps) compute the weight as the blurred
                      average of the base and the 1st data brick  */

      if( VL_imwt != NULL || !VL_twosum || VL_imbase == DSET_BRICK(VL_dset,0) ){

         albase = mri_3dalign_setup( tp_base , VL_imwt ) ;

      } else {

         float *far , *bar=MRI_FLOAT_PTR(tp_base) , *qar , clip ;
         int ii,jj,kk , nxy=nx*ny , nxyz=nxy*nz ;
         int nxbot,nxtop,nybot,nytop,nzbot,nztop , ee,fade,ff ;

         if( VL_verbose )
           fprintf(stderr,
                   "++ Computing first pass weight as sum of base and brick\n");

         fac = DSET_BRICK_FACTOR(VL_dset,0) ;
         fim = mri_scale_to_float( fac,DSET_BRICK(VL_dset,0) ) ; /* 1st data brick */
         far = MRI_FLOAT_PTR(fim) ;
         EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 , /* blur it */
                              MRI_float , far ,
                              VL_twoblur,VL_twoblur,VL_twoblur ) ;

         /* find shift of 1st data brick that best overlaps with base brick */

         if( VL_coarse_del > 0 && VL_coarse_num > 0 ){
           if( VL_coarse_rot == 0 ){
             if( VL_verbose )
               fprintf(stderr,"++ Getting best coarse shift [0]:") ;
             (void)get_best_shift( nx,ny,nz , bar,far , &sx,&sy,&sz ) ;
             if( VL_verbose ) fprintf(stderr," %d %d %d\n",sx,sy,sz) ;
           } else {                   /* 01 Dec 2005 */
             if( VL_verbose )
               fprintf(stderr,"++ Getting best coarse rot+shift [0]:") ;
               (void)new_get_best_shiftrot( VL_dset , tp_base , fim ,
                                            &rr, &pp, &yy, &sx, &sy, &sz ) ;
               if( hax1 < 0 ) rr = -rr ;
               if( hax2 < 0 ) pp = -pp ;
               if( hax3 < 0 ) yy = -yy ;
               if( VL_verbose ) fprintf(stderr," %g %g %g : %d %d %d\n",
                                        rr,pp,yy , sx,sy,sz) ;
           }
         } else {
           sx = sy = sz = 0 ; rr = pp = yy = 0.0f ;
         }

#undef  BAR
#undef  QAR
#undef  FAR
#define BAR(i,j,k) bar[(i)+(j)*nx+(k)*nxy]
#define QAR(i,j,k) qar[(i)+(j)*nx+(k)*nxy]
#define FAR(i,j,k) far[(i)+(j)*nx+(k)*nxy]

         qim = mri_copy(tp_base) ; qar = MRI_FLOAT_PTR(qim) ;

         ee = abs(sx) ; nxbot = ee ; nxtop = nx-ee ;
         ee = abs(sy) ; nybot = ee ; nytop = ny-ee ;
         ee = abs(sz) ; nzbot = ee ; nztop = nz-ee ;

         if( VL_sinit ){        /* 22 Mar 2004: initialize scale factor */
           float sf=0.0,sq=0.0 ;
           for( kk=nzbot ; kk < nztop ; kk++ )
            for( jj=nybot ; jj < nytop ; jj++ )
             for( ii=nxbot ; ii < nxtop ; ii++ ){
              sf += FAR(ii-sx,jj-sy,kk-sz) ; sq += QAR(ii,jj,kk) ;
             }
           if( sq > 0.0 ){
             sf = sf / sq ;
             if( sf > 0.005 && sf < 2000.0 ){ /* ZSS: sf increased to 2000 because sf of 1200 has been encountered with acceptable data */
               mri_3dalign_scaleinit(sf) ;
               if (sf < 200.0) {
                  if (VL_verbose) fprintf(stderr,"++ Scale init = %g\n",sf) ;
               } else {
                  fprintf(stderr,"++ Warning: Scale init = %g is large. Check output.\n",sf) ;
               }
             } else {
               fprintf(stderr,"-- Large scale difference between datasets.\n"
                              "   Scale init = %g\n"
                              "   3dvolreg might not converge.",sf) ;
            }
           }
         }

         /* add the blurred+shifted data brick to the blurred base brick */

         for( kk=nzbot ; kk < nztop ; kk++ )
          for( jj=nybot ; jj < nytop ; jj++ )
           for( ii=nxbot ; ii < nxtop ; ii++ )
            QAR(ii,jj,kk) += FAR(ii-sx,jj-sy,kk-sz) ;

         mri_free(fim) ;

         /* blur the sum to get the weight brick */

         if( VL_verbose )
           fprintf(stderr,"++ Blurring first pass weight\n") ;

#if 1
         EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 ,
                              MRI_float , qar ,
                              VL_twoblur,VL_twoblur,VL_twoblur ) ;
#else
         MRI_5blur_inplace_3D( qim ) ;              /* 07 Jun 2002 */
#endif

         clip = 0.025 * mri_max(qim) ;              /* 06 Jun 2002 */
         for( ii=0 ; ii < nxyz ; ii++ )
           if( qar[ii] < clip ) qar[ii] = 0.0 ;

         mri_3dalign_force_edging( 1 ) ;
         albase = mri_3dalign_setup( tp_base , qim ) ;
         mri_3dalign_force_edging( 0 ) ;
         mri_free(qim) ;
      }

      /* check if base was computed correctly */

      if( albase == NULL ){
         fprintf(stderr,
                 "** Can't initialize first pass alignment algorithm\n");
         exit(1);
      }

      dx_1    = (float *) malloc( sizeof(float) * imcount ) ;
      dy_1    = (float *) malloc( sizeof(float) * imcount ) ;
      dz_1    = (float *) malloc( sizeof(float) * imcount ) ;
      roll_1  = (float *) malloc( sizeof(float) * imcount ) ;
      pitch_1 = (float *) malloc( sizeof(float) * imcount ) ;
      yaw_1   = (float *) malloc( sizeof(float) * imcount ) ;

      /* do alignment on blurred copy of each brick;
         save parameters for later feed into pass #2 */

      for( kim=0 ; kim < imcount ; kim++ ){

         qim     = DSET_BRICK( VL_dset , kim ) ; /* the sub-brick in question */
         fac     = DSET_BRICK_FACTOR( VL_dset , kim ) ;
         fim     = mri_scale_to_float( fac , qim ) ;     /* make a float copy */
         fim->dx = fabs( DSET_DX(VL_dset) ) ;    /* must set voxel dimensions */
         fim->dy = fabs( DSET_DY(VL_dset) ) ;
         fim->dz = fabs( DSET_DZ(VL_dset) ) ;

         if( kim != VL_nbase && mri_nonzero_count(fim) >= 66 ){ /* don't register to base image */

            EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 ,
                                 MRI_float , MRI_FLOAT_PTR(fim) ,
                                 VL_twoblur,VL_twoblur,VL_twoblur ) ;

            if( kim > 0 || sx == 66666 ){ /* if didn't already get best shift */
              if( VL_coarse_del > 0 && VL_coarse_num > 0 ){
                if( VL_coarse_rot == 0 ){
                  if( VL_verbose )
                    fprintf(stderr,"++ Getting best coarse shift [%d]:",kim) ;
                  (void)get_best_shift( nx,ny,nz ,
                                        MRI_FLOAT_PTR(tp_base),MRI_FLOAT_PTR(fim) ,
                                        &sx,&sy,&sz ) ;
                  if( VL_verbose ) fprintf(stderr," %d %d %d\n",sx,sy,sz) ;
                } else {                   /* 01 Dec 2005 */
                  if( VL_verbose )
                    fprintf(stderr,"++ Getting best coarse rot+shift [%d]:",kim) ;
                    (void)new_get_best_shiftrot( VL_dset , tp_base , fim ,
                                                 &rr, &pp, &yy, &sx, &sy, &sz ) ;
                    if( hax1 < 0 ) rr = -rr ;
                    if( hax2 < 0 ) pp = -pp ;
                    if( hax3 < 0 ) yy = -yy ;
                    if( VL_verbose ) fprintf(stderr," %g %g %g : %d %d %d\n",
                                             rr,pp,yy , sx,sy,sz) ;
                }
              } else {
                sx = sy = sz = 0 ; rr = pp = yy = 0.0f ;
              }
            }

            mri_3dalign_initvals( rr , pp , yy ,
                                  sx*fim->dx , sy*fim->dy , sz*fim->dz ) ;

            (void) mri_3dalign_one( albase , fim ,
                                    roll_1+kim , pitch_1+kim , yaw_1+kim ,
                                    dx_1  +kim , dy_1   +kim , dz_1 +kim  ) ;

            roll_1[kim]  *= (180.0/PI) ;  /* convert to degrees */
            pitch_1[kim] *= (180.0/PI) ;
            yaw_1[kim]   *= (180.0/PI) ;

         } else {
            roll_1[kim]  =           /* This   */
             pitch_1[kim] =           /* looks   */
              yaw_1[kim]   =           /* kind     */
               dx_1[kim]    =           /* of        */
                dy_1[kim]    =           /* cool,      */
                 dz_1[kim]    = 0.0 ;     /* doesn't it? */
         }

         mri_free(fim) ;
      }

      mri_3dalign_cleanup( albase ) ; mri_free( tp_base ) ;

      if( VL_verbose ){
         cputim = COX_cpu_time() - cputim ;
         fprintf(stderr,"++ CPU time for first pass=%.3g s\n" , cputim) ;
      }

   }  /* end of twopass */

   /*-----------------------------------*/
   /*-- prepare for (final) alignment --*/

   if( VL_verbose ) fprintf(stderr,"++ Initializing alignment base\n") ;

   mri_3dalign_params( VL_maxite , VL_dxy , VL_dph , VL_del ,
                       abs(ax1)-1 , abs(ax2)-1 , abs(ax3)-1 , -1 ) ;

   /* 14 Feb 2001:
      if have a final transformation, then don't produce output
                                                   |||||||||||||
                                                   VVVVVVVVVVVVV   */
   mri_3dalign_method( VL_resam , (VL_verbose>1) , (matvec != 0) , VL_clipit );

   if( VL_final < 0 ) VL_final = VL_resam ;  /* 20 Nov 1998 */
   mri_3dalign_final_regmode( VL_final ) ;

   /* 06 Jun 2002: create -wtinp weight now */

   if( VL_wtinp ){
     fac = DSET_BRICK_FACTOR(VL_dset,0) ;
     VL_imwt = mri_scale_to_float( fac , DSET_BRICK(VL_dset,0) ) ;
     mri_3dalign_wproccing( 1 ) ;
   }

   albase = mri_3dalign_setup( VL_imbase , VL_imwt ) ;
   if( albase == NULL ){
      fprintf(stderr,"** Can't initialize base image for alignment\n"); exit(1);
   }
   if( VL_imwt != NULL ) mri_free( VL_imwt ) ;

   /*-- loop over sub-bricks and register them --*/

   if( VL_verbose ){
      fprintf(stderr,"++ Starting final pass on %d sub-bricks: ",imcount);
      cputim = COX_cpu_time();
   }

   dxbar = dybar = dzbar = rollbar = yawbar = pitchbar = 0.0 ;  /* stats */

   for( kim=0 ; kim < imcount ; kim++ ){

      if( VL_verbose ) fprintf(stderr,"%d",kim) ;  /* mark start of this one */

      qim     = DSET_BRICK( VL_dset , kim ) ; /* the sub-brick in question */
      fac     = DSET_BRICK_FACTOR( VL_dset , kim ) ;
      fim     = mri_scale_to_float( fac , qim ) ;     /* make a float copy */
      fim->dx = fabs( DSET_DX(VL_dset) ) ;    /* must set voxel dimensions */
      fim->dy = fabs( DSET_DY(VL_dset) ) ;
      fim->dz = fabs( DSET_DZ(VL_dset) ) ;

      /*-- the actual registration [please bow your head] --*/

      if( kim != VL_nbase && mri_nonzero_count(fim) >= 66 ){ /* don't register base to self */

         if( VL_twopass )
            mri_3dalign_initvals( roll_1[kim] , pitch_1[kim] , yaw_1[kim] ,
                                  dx_1[kim]   , dy_1[kim]    , dz_1[kim]   ) ;

         tim = mri_3dalign_one( albase , fim ,
                                roll+kim , pitch+kim , yaw+kim ,
                                &ddx     , &ddy      , &ddz     ) ;

      } else {               /* 16 Nov 1998: just make a copy of base image */

         if( kim == VL_nbase ){
           if( !matvec ) tim = mri_to_float( VL_imbase ) ; /* make a copy */
           else          tim = NULL ;                      /* 14 Feb 2001 */
         } else {                                          /* 13 Mar 2017 */
           WARNING_message("skipped registration for #%d because image is empty-ish",kim) ;
           if( !matvec ) tim = mri_to_float( fim ) ;
           else          tim = NULL ;
         }

         roll[kim] = pitch[kim] = yaw[kim] = ddx = ddy = ddz = 0.0 ;

      }

      /* 14 Feb 2001: at this point,
           if we have a final transform (matvec != 0), fim = unrotated image;
           if we don't have a final transform,         tim = rotated image    */

      if( VL_verbose ) fprintf(stderr,"."); /* mark that registration is done */

      /*-- need to massage output parameters --*/

      roll[kim]  *= (180.0/PI) ; if( hax1 < 0 ) roll[kim]  = -roll[kim] ;
      pitch[kim] *= (180.0/PI) ; if( hax2 < 0 ) pitch[kim] = -pitch[kim] ;
      yaw[kim]   *= (180.0/PI) ; if( hax3 < 0 ) yaw[kim]   = -yaw[kim] ;

      switch( new_dset->daxes->xxorient ){
         case ORI_R2L_TYPE: dy[kim] =  ddx ; break ;
         case ORI_L2R_TYPE: dy[kim] = -ddx ; break ;
         case ORI_P2A_TYPE: dz[kim] = -ddx ; break ;
         case ORI_A2P_TYPE: dz[kim] =  ddx ; break ;
         case ORI_I2S_TYPE: dx[kim] =  ddx ; break ;
         case ORI_S2I_TYPE: dx[kim] = -ddx ; break ;
      }

      switch( new_dset->daxes->yyorient ){
         case ORI_R2L_TYPE: dy[kim] =  ddy ; break ;
         case ORI_L2R_TYPE: dy[kim] = -ddy ; break ;
         case ORI_P2A_TYPE: dz[kim] = -ddy ; break ;
         case ORI_A2P_TYPE: dz[kim] =  ddy ; break ;
         case ORI_I2S_TYPE: dx[kim] =  ddy ; break ;
         case ORI_S2I_TYPE: dx[kim] = -ddy ; break ;
      }

      switch( new_dset->daxes->zzorient ){
         case ORI_R2L_TYPE: dy[kim] =  ddz ; break ;
         case ORI_L2R_TYPE: dy[kim] = -ddz ; break ;
         case ORI_P2A_TYPE: dz[kim] = -ddz ; break ;
         case ORI_A2P_TYPE: dz[kim] =  ddz ; break ;
         case ORI_I2S_TYPE: dx[kim] =  ddz ; break ;
         case ORI_S2I_TYPE: dx[kim] = -ddz ; break ;
      }

      /*** 14 Feb 2001: if needed, now apply final transformation
                        on top of this just-computed transformation ***/

      if( matvec ){
         THD_dvecmat vm1 , vm2 , vmtot ;
         char sbuf[128] ;

         vm2.mm = rmat ; vm2.vv = tvec ;  /* second transform */

         sprintf(sbuf,"-rotate %.4fI %.4fR %.4fA -ashift %.4fS %.4fL %.4fP" ,
                 roll[kim],pitch[kim],yaw[kim], dx[kim],dy[kim],dz[kim]  ) ;
         vm1 = THD_rotcom_to_matvec( new_dset , sbuf ) ;

         vmtot = MUL_DVECMAT(vm2,vm1 ) ;  /* total transform */

         /* zero pad before final transformation? */

         if( npadd ){
           MRI_IMAGE *zim = mri_zeropad_3D( 0,0,0,0,npad_neg,npad_pos , fim ) ;
           if( zim == NULL ) ERROR_exit("Can't zeropad at kim=%d !",kim);
           mri_free(fim) ; fim = zim ;
         }

         THD_rota_method( VL_final ) ;
         tim = THD_rota3D_matvec( fim , vmtot.mm,vmtot.vv ) ; /* the work */

         if( VL_clipit &&
             (VL_final == MRI_QUINTIC || VL_final==MRI_CUBIC  ||
              VL_final == MRI_HEPTIC  || VL_final==MRI_FOURIER  )){

            register int ii ;
            register float ftop , fbot , * tar ;

            ftop = mri_max( fim ); fbot = mri_min( fim ); /* input range */
            tar  = MRI_FLOAT_PTR(tim) ;                   /* output array */
            for( ii=0 ; ii < tim->nvox ; ii++ ){
                    if( tar[ii] < fbot ) tar[ii] = fbot ; /* clipping */
               else if( tar[ii] > ftop ) tar[ii] = ftop ;
            }
         }
      } /* at last, have the output brick! */

      /*-- collect statistics --*/

      if( kim == 0 ){
        dxtop    = dxbot    = dx[kim]    ;
        dytop    = dybot    = dy[kim]    ;
        dztop    = dzbot    = dz[kim]    ;
        rolltop  = rollbot  = roll[kim]  ;
        pitchtop = pitchbot = pitch[kim] ;
        yawtop   = yawbot   = yaw[kim]   ;
      } else {
        dxtop    = MAX(dxtop   ,dx[kim]   ); dxbot    = MIN(dxbot   ,dx[kim]   );
        dytop    = MAX(dytop   ,dy[kim]   ); dybot    = MIN(dybot   ,dy[kim]   );
        dztop    = MAX(dztop   ,dz[kim]   ); dzbot    = MIN(dzbot   ,dz[kim]   );
        rolltop  = MAX(rolltop ,roll[kim] ); rollbot  = MIN(rollbot ,roll[kim] );
        pitchtop = MAX(pitchtop,pitch[kim]); pitchbot = MIN(pitchbot,pitch[kim]);
        yawtop   = MAX(yawtop  ,yaw[kim]  ); yawbot   = MIN(yawbot  ,yaw[kim]  );
      }

      dxbar   += dx[kim]   ; dybar    += dy[kim]    ; dzbar  += dz[kim]  ;
      rollbar += roll[kim] ; pitchbar += pitch[kim] ; yawbar += yaw[kim] ;

      if( !matvec ){
        sum = 0.0 ;
        tar = MRI_FLOAT_PTR(tim) ;
        for( ii=0 ; ii < tim->nvox ; ii++ ) sum += SQR( imb[ii] - tar[ii] ) ;
        rmsnew[kim] = sqrt( sum / tim->nvox ) ;

        sum = 0.0 ;
        tar = MRI_FLOAT_PTR(fim) ;
        for( ii=0 ; ii < fim->nvox ; ii++ ) sum += SQR( imb[ii] - tar[ii] ) ;
        rmsold[kim] = sqrt( sum / fim->nvox ) ;
      } else {
        rmsold[kim] = rmsnew[kim] = 0.0 ;  /* can't compute these */
      }

      mri_free(fim) ;

      /*-- Attach the registered brick to output dataset,
           converting it to the correct type, if necessary
           (the new registered brick in "tim" is stored as floats). --*/

      if( tim != NULL && !null_output ){
        int typ = (VL_floatize) ? MRI_float : qim->kind ;  /* 07 Jan 2008 */
        switch( typ ){

          case MRI_float:
            EDIT_substitute_brick( new_dset, kim, MRI_float, MRI_FLOAT_PTR(tim) );
            mri_fix_data_pointer( NULL , tim ) ; mri_free( tim ) ;
          break ;

          case MRI_short:
            fim = mri_to_short(1.0,tim) ; mri_free( tim ) ;
            EDIT_substitute_brick( new_dset, kim, MRI_short, MRI_SHORT_PTR(fim) );
            mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
          break ;

          case MRI_byte:
            fim = mri_to_byte(tim) ; mri_free( tim ) ;
            EDIT_substitute_brick( new_dset, kim, MRI_byte, MRI_BYTE_PTR(fim) ) ;
            mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
          break ;

          /*-- should not ever get here, but who knows? --*/

          default:
            fprintf(stderr,"\n*** Can't register bricks of type %s\n",
                    MRI_TYPE_name[qim->kind] ) ;
            exit(1) ;
        }
      } else if( tim != NULL ) mri_free(tim) ; /* 24 Jul 2007 */

      DSET_unload_one( VL_dset , kim ) ;      /* don't need this anymore */

      if( VL_verbose ) fprintf(stderr,".") ;  /* mark end of this one */

   }  /*--- end of loop over sub-bricks ---*/

   /*------------------ done with registration ------------------*/

   mri_3dalign_cleanup( albase ) ;
   DSET_unload( VL_dset ) ;        /* 06 Feb 2001: unload instead of delete */
   mri_free( VL_imbase ) ;
   if( VL_twopass ){
     free(dx_1);free(dy_1);free(dz_1);free(roll_1);free(pitch_1);free(yaw_1);
   }

   /*-- print some summaries (maybe) --*/

   dxbar   /= imcount ; dybar    /= imcount ; dzbar  /= imcount ;
   rollbar /= imcount ; pitchbar /= imcount ; yawbar /= imcount ;

   if( VL_verbose ){
     cputim = COX_cpu_time() - cputim ;
     fprintf(stderr,"\n++ CPU time for realignment=%.3g s" , cputim) ;
     if( imcount > 1 ) fprintf(stderr,"  [=%.3g s/sub-brick]" , cputim/imcount) ;
     fprintf(stderr,"\n") ;

     fprintf(stderr,"++ Min : roll=%+.3f  pitch=%+.3f  yaw=%+.3f"
                    "  dS=%+.3f  dL=%+.3f  dP=%+.3f\n" ,
             rollbot , pitchbot , yawbot , dxbot , dybot , dzbot ) ;

     fprintf(stderr,"++ Mean: roll=%+.3f  pitch=%+.3f  yaw=%+.3f"
                    "  dS=%+.3f  dL=%+.3f  dP=%+.3f\n" ,
             rollbar , pitchbar , yawbar , dxbar , dybar , dzbar ) ;

     fprintf(stderr,"++ Max : roll=%+.3f  pitch=%+.3f  yaw=%+.3f"
                    "  dS=%+.3f  dL=%+.3f  dP=%+.3f\n" ,
             rolltop , pitchtop , yawtop , dxtop , dytop , dztop ) ;
   }

   /*-- 12 Sep 2000: add some history? --*/

   if( imcount == 1 && VL_rotpar_dset == NULL ){
     char *str = NULL ;
     str = THD_zzprintf( str , "3dvolreg did: %s" , modes[VL_final] ) ;
     if( VL_clipit ) str = THD_zzprintf( str , " -clipit" ) ;
     else            str = THD_zzprintf( str , " -noclip" ) ;
     if( VL_zpad )   str = THD_zzprintf( str , " -zpad %d" , VL_zpad ) ;
     str = THD_zzprintf(str,
                     " -rotate %.4fI %.4fR %.4fA -ashift %.4fS %.4fL %.4fP\n",
                     roll[0],pitch[0],yaw[0], dx[0],dy[0],dz[0]  ) ;
     tross_Append_History( new_dset , str ) ;
     free(str) ;
   }

   /*-- 06 Feb 2000: save parameters to header of output in attributes --*/
   /*--              N.B.: vectors and matrices are in Dicom order!    --*/

   { char sbuf[128] , anam[32] ;
     THD_fvec3 cv ;
     THD_dmat33 rmat ; float matar[12] ;
     THD_dfvec3 *qvec=NULL ;             /* 22 Jun 2015 */

     /* -rotparent and -gridparent datasets, if present */

     if( VL_rotpar_dset != NULL ){
       THD_set_string_atr(new_dset->dblk,"VOLREG_ROTPARENT_IDCODE",
                                         VL_rotpar_dset->idcode.str   );
       THD_set_string_atr(new_dset->dblk,"VOLREG_ROTPARENT_NAME"  ,
                                         DSET_HEADNAME(VL_rotpar_dset));
     }

     if( VL_gridpar_dset != NULL ){
       THD_set_string_atr(new_dset->dblk,"VOLREG_GRIDPARENT_IDCODE",
                                         VL_gridpar_dset->idcode.str   );
       THD_set_string_atr(new_dset->dblk,"VOLREG_GRIDPARENT_NAME"  ,
                                         DSET_HEADNAME(VL_gridpar_dset));
     }

     /* Dicom center of input dataset */

     THD_set_string_atr( new_dset->dblk, "VOLREG_INPUT_IDCODE",
                                         VL_dset->idcode.str ) ;
     THD_set_string_atr( new_dset->dblk, "VOLREG_INPUT_NAME"  ,
                                         DSET_HEADNAME(VL_dset) ) ;

     cv = THD_dataset_center( new_dset ) ;
     THD_set_float_atr( new_dset->dblk , "VOLREG_CENTER_OLD" , 3 , cv.xyz ) ;

     /* info about base dataset */

     THD_set_string_atr( new_dset->dblk , "VOLREG_BASE_IDCODE" ,
                                          VL_bset->idcode.str ) ;
     THD_set_string_atr( new_dset->dblk , "VOLREG_BASE_NAME" ,
                                          DSET_HEADNAME(VL_bset) ) ;

     cv = VL_cen_bas ;
     THD_set_float_atr( new_dset->dblk , "VOLREG_CENTER_BASE" , 3 , cv.xyz ) ;

     /* number of images registered */

     THD_set_int_atr( new_dset->dblk , "VOLREG_ROTCOM_NUM" , 1 , &imcount ) ;

     /* each volume's transformation parameters, matrix, and vector */

     if( VL_maxdisp > 0 ){  /* for computing and saving max displacements */
       if( VL_verbose )
         INFO_message("Max displacements (mm) for each sub-brick:") ;
       VL_dmaxar  = (float *)calloc(sizeof(float),imcount) ;
       VL_emaxar  = (float *)calloc(sizeof(float),imcount) ;
       VL_emaxvec = (THD_dfvec3 *)calloc( sizeof(THD_dfvec3) , VL_maxdisp ) ;
     }

     if( VL_matrix_save_1D != NULL ){             /* 24 Jul 2007 */
       VL_msfp = fopen(VL_matrix_save_1D,"w") ;
       if( VL_msfp != NULL )
         fprintf(VL_msfp,
                 "# 3dvolreg matrices (DICOM-to-DICOM, row-by-row):\n") ;
       else
         ERROR_message("Cannot open '%s' for output :(",VL_matrix_save_1D) ;
     }

#undef  SDAPP
#define SDAPP(aa) \
 do{ strcpy(pr,VL_savedisp_prA); strcat(pr,(aa)); strcat(pr,VL_savedisp_prB); } while(0)

     if( VL_savedisp != NULL ){             /* 04 Apr 2012: create -savedisp datasets */
       char *pr = malloc(sizeof(char)*THD_MAX_NAME) ;

       VL_xdset = EDIT_empty_copy( new_dset ) ;
       tross_Copy_History( new_dset , VL_xdset ) ;
       SDAPP("_DX") ;
       EDIT_dset_items( VL_xdset , ADN_prefix , pr , ADN_none ) ;
       tross_Append_History( VL_xdset , "-- _DX savedisp output" ) ;

       VL_ydset = EDIT_empty_copy( new_dset ) ;
       tross_Copy_History( new_dset , VL_ydset ) ;
       SDAPP("_DY") ;
       EDIT_dset_items( VL_ydset , ADN_prefix , pr , ADN_none ) ;
       tross_Append_History( VL_ydset , "-- _DY savedisp output" ) ;

       VL_zdset = EDIT_empty_copy( new_dset ) ;
       tross_Copy_History( new_dset , VL_zdset ) ;
       SDAPP("_DZ") ;
       EDIT_dset_items( VL_zdset , ADN_prefix , pr , ADN_none ) ;
       tross_Append_History( VL_zdset , "-- _DZ savedisp output" ) ;

       free(pr) ;
     }

     for( kim=0 ; kim < imcount ; kim++ ){
        sprintf(anam,"VOLREG_ROTCOM_%06d",kim) ;
        sprintf(sbuf,"-rotate %.4fI %.4fR %.4fA -ashift %.4fS %.4fL %.4fP" ,
                roll[kim],pitch[kim],yaw[kim], dx[kim],dy[kim],dz[kim]  ) ;
        THD_set_string_atr( new_dset->dblk , anam , sbuf ) ;

        /*-- note minus sign and conversion to radians --*/
        /*                        |                      */
        /*                        V                      */
        rmat = rot_to_matrix( 2 , -(PI/180.0)*roll[kim]  ,   /* Dicom order */
                              0 , -(PI/180.0)*pitch[kim] ,   /* of the axes */
                              1 , -(PI/180.0)*yaw[kim]    ) ;

        /* matrix and vector are 12 numbers:
                   a11 a12 a13 v1
                   a21 a22 a23 v2
                   a31 a32 a33 v3
           stored as in 3dTagalign.c's TAGALIGN_MATVEC attribute */

        UNLOAD_DMAT(rmat,matar[0],matar[1],matar[2],
                         matar[4],matar[5],matar[6],
                         matar[8],matar[9],matar[10] ) ;
        matar[3] = dy[kim] ; matar[7] = dz[kim] ; matar[11] = dx[kim] ;
        sprintf(anam,"VOLREG_MATVEC_%06d",kim) ;
        THD_set_float_atr( new_dset->dblk , anam , 12 , matar ) ;

        if( VL_msfp != NULL ){  /* 24 Jul 2007 */
          THD_dvecmat vm , ivm ;
          float xd=matar[3] , yd=matar[7] , zd=matar[11] ;
#if 0 /* ZSS: That's not good enough
               One has to do the dance
               even if dist is 0.0, just like in cat_matvec */
          if( VL_cen_dist > 0.01f ){  /* 11 Mar 2008 */
            THD_fvec3 dv , ev ;
            dv = MATVEC(rmat,VL_cen_inp) ;
            ev = SUB_FVEC3(VL_cen_bas,dv) ;
            xd += dv.xyz[0] ; yd += dv.xyz[1] ; zd += dv.xyz[2] ;
          }
          LOAD_DFVEC3(vm.vv,xd,yd,zd) ;
          vm.mm = rmat ; ivm = invert_dvecmat(vm) ;
#else
          {
            THD_fvec3 dv , ev ;
            dv = MATVEC(rmat, VL_cen_inp) ;
            xd += VL_cen_bas.xyz[0] - dv.xyz[0];
            yd += VL_cen_bas.xyz[1] - dv.xyz[1];
            zd += VL_cen_bas.xyz[2] - dv.xyz[2];
            LOAD_DFVEC3(vm.vv,xd,yd,zd) ;
            vm.mm = rmat ; ivm = invert_dvecmat(vm) ;
          }
#endif
          fprintf(VL_msfp,"%13.6g %13.6g %13.6g %13.6g "
                          "%13.6g %13.6g %13.6g %13.6g "
                          "%13.6g %13.6g %13.6g %13.6g\n" ,
          ivm.mm.mat[0][0], ivm.mm.mat[0][1], ivm.mm.mat[0][2], ivm.vv.xyz[0],
          ivm.mm.mat[1][0], ivm.mm.mat[1][1], ivm.mm.mat[1][2], ivm.vv.xyz[1],
          ivm.mm.mat[2][0], ivm.mm.mat[2][1], ivm.mm.mat[2][2], ivm.vv.xyz[2] ) ;
        }

        /* 04 Apr 2012: save all displacements into _D{XYZ} datasets */

        if( VL_savedisp != NULL ){
          float **dispar ;
          LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ;
          dispar = VL_get_displacments( VL_xdset , rmat , tvec ) ;
          EDIT_substitute_brick( VL_xdset , kim , MRI_float , dispar[0] ) ;
          EDIT_substitute_brick( VL_ydset , kim , MRI_float , dispar[1] ) ;
          EDIT_substitute_brick( VL_zdset , kim , MRI_float , dispar[2] ) ;
          free(dispar) ;
        }

        /* 04 Aug 2006: max displacement calculation */

        if( VL_maxdisp > 0 ){
          THD_dmat33 pp,ppt ; THD_dfvec3 dv,qv,vorg , ev ;
          int qq ; float dmax=0.0f , xo,yo,zo , ddd , eee,emax=0.0f ;
          LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ;
          pp   = DBLE_mat_to_dicomm( VL_dset ) ;   /* convert rmat to dataset coord order */
          ppt  = TRANSPOSE_DMAT(pp);
          rmat = DMAT_MUL(ppt,rmat); rmat = DMAT_MUL(rmat,pp); tvec = DMATVEC(ppt,tvec);
          xo = VL_dset->daxes->xxorg + 0.5*(VL_dset->daxes->nxx - 1)*VL_dset->daxes->xxdel ;
          yo = VL_dset->daxes->yyorg + 0.5*(VL_dset->daxes->nyy - 1)*VL_dset->daxes->yydel ;
          zo = VL_dset->daxes->zzorg + 0.5*(VL_dset->daxes->nzz - 1)*VL_dset->daxes->zzdel ;
          LOAD_DFVEC3(vorg,xo,yo,zo) ;             /* rotation is around dataset center */
          for( qq=0 ; qq < VL_maxdisp ; qq++ ){
            FVEC3_TO_DFVEC3( VL_dispvec[qq] , dv );
            qv = SUB_DFVEC3(dv,vorg); qv = DMATVEC_ADD(rmat,qv,tvec); qv = ADD_DFVEC3(qv,vorg);
            qv = SUB_DFVEC3(dv,qv); ddd = SIZE_DFVEC3(qv); if( ddd > dmax ) dmax = ddd;
            if( VL_emaxvec != NULL ){
              ev = (kim > 0) ? VL_emaxvec[qq] : qv ;
              VL_emaxvec[qq] = qv ;
              ev = SUB_DFVEC3(ev,qv) ; eee = SIZE_DFVEC3(ev) ; if( eee > emax ) emax = eee ;
            }
          }
          if( VL_dmax < dmax ){ VL_dmax = dmax ; VL_dmaxi = kim ; }
          if( VL_emax < emax ){ VL_emax = emax ; VL_emaxi = kim ; }
          if( VL_verbose ) fprintf(stderr," %.2f(%.2f)",dmax,emax) ;
          if( VL_dmaxar != NULL ) VL_dmaxar[kim] = dmax ;
          if( VL_emaxar != NULL ) VL_emaxar[kim] = emax ;
        }

     }  /*--- end of loop over registered sub-bricks ---*/

     if( VL_msfp != NULL ) fclose(VL_msfp) ;  /* 24 Jul 2007 */

     if( VL_maxdisp > 0 ){
       if( VL_verbose ) fprintf(stderr,"\n") ;
       INFO_message("Max displacement in automask = %.2f (mm) at sub-brick %d",VL_dmax,VL_dmaxi);
       INFO_message("Max delta displ  in automask = %.2f (mm) at sub-brick %d",VL_emax,VL_emaxi);
       free((void *)VL_dispvec) ; free((void *)VL_emaxvec) ;
       if( *VL_dmaxfile != '\0' && VL_dmaxar != NULL ){
         FILE *fp ;
         if( strcmp(VL_dmaxfile,"-") != 0 ){
           if( THD_is_file(VL_dmaxfile) ) WARNING_message("Overwriting file %s",VL_dmaxfile);
           fp = fopen( VL_dmaxfile , "w" ) ;
           if( fp == NULL ){
             ERROR_message("Cannot open '%s' for output :(",VL_dmaxfile) ;
           }
         } else {
           fp = stdout ;
         }
         if( fp != NULL ){
           fprintf(fp,"# %s\n",VL_commandline) ;
           fprintf(fp,"# max displacement (mm) for each volume\n") ;
           for( kim=0 ; kim < imcount ; kim++ ) fprintf(fp," %.3f\n",VL_dmaxar[kim]) ;
           if( fp != stdout ) fclose(fp) ;
         }
       }
       if( *VL_emaxfile != '\0' && VL_emaxar != NULL ){
         FILE *fp ;
         if( strcmp(VL_emaxfile,"-") != 0 ){
           if( THD_is_file(VL_emaxfile) ) WARNING_message("Overwriting file %s",VL_emaxfile);
           fp = fopen( VL_emaxfile , "w" ) ;
           if( fp == NULL ){
             ERROR_message("Cannot open '%s' for output :(",VL_emaxfile) ;
           }
         } else {
           fp = stdout ;
         }
         if( fp != NULL ){
           fprintf(fp,"# %s\n",VL_commandline) ;
           fprintf(fp,"# max delta displ (mm) for each volume\n") ;
           for( kim=0 ; kim < imcount ; kim++ ) fprintf(fp," %.3f\n",VL_emaxar[kim]) ;
           if( fp != stdout ) fclose(fp) ;
         }
       }
     }
   }

   /*-- 08 Dec 2000: execute -twodup? --*/

   if( VL_twodup && !VL_intern && VL_rotpar_dset == NULL ){
     new_dset->daxes->xxorg = VL_bxorg ;
     new_dset->daxes->yyorg = VL_byorg ;
     new_dset->daxes->zzorg = VL_bzorg ;

     if( new_dset->taxis != NULL && new_dset->taxis->nsl > 0 ){ /* 12 Feb 2001 */
       new_dset->taxis->zorg_sl = new_dset->daxes->zzorg ;
     }
   }

   /*-- save new dataset to disk --*/

   if( !null_output ){
     DSET_write(new_dset) ;
     if( VL_verbose )
       INFO_message("Wrote dataset to disk in %s",DSET_BRIKNAME(new_dset));
   }
   DSET_unload(new_dset) ;


   if( VL_xdset != NULL ){    /* 04 Apr 2012 */
     if( VL_savedisp19 ) VL_normalize_timeseries(VL_xdset) ;
     DSET_write(VL_xdset) ;
     if( VL_verbose ) INFO_message("Wrote dataset to disk in %s",DSET_BRIKNAME(VL_xdset));
   }
   if( VL_ydset != NULL ){
     if( VL_savedisp19 ) VL_normalize_timeseries(VL_ydset) ;
     DSET_write(VL_ydset) ;
     if( VL_verbose ) INFO_message("Wrote dataset to disk in %s",DSET_BRIKNAME(VL_ydset));
   }
   if( VL_zdset != NULL ){
     if( VL_savedisp19 ) VL_normalize_timeseries(VL_zdset) ;
     DSET_write(VL_zdset) ;
     if( VL_verbose ) INFO_message("Wrote dataset to disk in %s",DSET_BRIKNAME(VL_zdset));
   }

#if 1
   if( VL_savedisp19 && VL_xdset != NULL && VL_ydset != NULL && VL_zdset != NULL ){
     VL_create_disprod( new_dset , VL_xdset,2 , VL_ydset,0 , VL_zdset,0 ) ;  /* DXX */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,2 , VL_zdset,0 ) ;  /* DYY */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,0 , VL_zdset,2 ) ;  /* DZZ */
     VL_create_disprod( new_dset , VL_xdset,1 , VL_ydset,1 , VL_zdset,0 ) ;  /* DXY */
     VL_create_disprod( new_dset , VL_xdset,1 , VL_ydset,0 , VL_zdset,1 ) ;  /* DXZ */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,1 , VL_zdset,1 ) ;  /* DYZ */
     VL_create_disprod( new_dset , VL_xdset,3 , VL_ydset,0 , VL_zdset,0 ) ;  /* DXXX */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,3 , VL_zdset,0 ) ;  /* DYYY */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,0 , VL_zdset,3 ) ;  /* DZZZ */
     VL_create_disprod( new_dset , VL_xdset,2 , VL_ydset,1 , VL_zdset,0 ) ;  /* DXXY */
     VL_create_disprod( new_dset , VL_xdset,2 , VL_ydset,0 , VL_zdset,1 ) ;  /* DXXZ */
     VL_create_disprod( new_dset , VL_xdset,1 , VL_ydset,2 , VL_zdset,0 ) ;  /* DXYY */
     VL_create_disprod( new_dset , VL_xdset,1 , VL_ydset,0 , VL_zdset,2 ) ;  /* DXZZ */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,2 , VL_zdset,1 ) ;  /* DYYZ */
     VL_create_disprod( new_dset , VL_xdset,0 , VL_ydset,1 , VL_zdset,2 ) ;  /* DYZZ */
     VL_create_disprod( new_dset , VL_xdset,1 , VL_ydset,1 , VL_zdset,1 ) ;  /* DXYZ */
   }
   DSET_delete(VL_xdset) ; DSET_delete(VL_ydset) ; DSET_delete(VL_zdset) ;
#endif

   /*-- save movement parameters to disk --*/

   if( VL_dfile[0] != '\0' ){
     FILE *fp ;

     if( THD_is_file(VL_dfile) )
       fprintf(stderr,"** Warning: overwriting file %s\n",VL_dfile) ;

     fp = fopen( VL_dfile , "w" ) ;
     if( fp == NULL ){
       ERROR_message("Cannot open '%s' for output",VL_dfile) ;
     } else {
       for( kim=0 ; kim < imcount ; kim++ )
         fprintf(fp , "%4d %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f  %11.4g %11.4g\n" ,
                 kim , roll[kim], pitch[kim], yaw[kim],
                       dx[kim], dy[kim], dz[kim],
                       rmsold[kim] , rmsnew[kim]  ) ;
       fclose(fp) ;
     }
   }

   if( VL_1Dfile[0] != '\0' ){  /* 14 Apr 2000 */
      FILE *fp ;

      if( THD_is_file(VL_1Dfile) )
         fprintf(stderr,"** Warning: overwriting file %s\n",VL_1Dfile) ;

      fp = fopen( VL_1Dfile , "w" ) ;
      if( fp == NULL ){
        ERROR_message("Cannot open '%s' for output",VL_1Dfile) ;
      } else {
        for( kim=0 ; kim < imcount ; kim++ )
           fprintf(fp , "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n" ,
                   roll[kim], pitch[kim], yaw[kim],
                   dx[kim]  , dy[kim]   , dz[kim]  ) ;
        fclose(fp) ;
      }
   }

   if( VL_rotcom ){ /* 04 Sep 2000 */

      printf("\n3drotate fragment%s:\n\n", (imcount > 1)? "s" : "" ) ;
      for( kim=0 ; kim < imcount ; kim++ ){
         printf("3drotate %s" , modes[VL_final] ) ;
         if( VL_clipit ) printf(" -clipit" ) ;
         else            printf(" -noclip" ) ;
         if( VL_zpad )   printf(" -zpad %d" , VL_zpad ) ;
         printf(" -rotate %.4fI %.4fR %.4fA -ashift %.4fS %.4fL %.4fP\n" ,
                 roll[kim],pitch[kim],yaw[kim], dx[kim],dy[kim],dz[kim]  ) ;
      }
      printf("\n") ;  /* 11 Dec 2000 */
   }

   if( VL_floatize_forced && !null_output )
     WARNING_message(
       "Input dataset has scale factors ==> output is stored as floats!");

   exit(0) ;
}

/*---------------------------------------------------------------------*/

void VL_syntax(void)
{
   printf(
    "Usage: 3dvolreg [options] dataset\n"
    "Registers each 3D sub-brick from the input dataset to the base brick.\n"
    "'dataset' may contain a sub-brick selector list.\n"
    "\n"
    "-->> Also see the script align_epi_anat.py for a more general\n"
    "     alignment procedure, which does not require that the base\n"
    "     and source datasets be defined on the same 3D grid.\n"
    "-->> Program 3dAllineate can do nonlinear (polynomial) warping in 3D\n"
    "     to align 2 datasets.  Script @2dwarper.Allin can do nonlinear\n"
    "     warping in 2D to align 2 datasets on a slice-wise basis\n"
    "     (no out-of-slice movements; each slice registered separately).\n"
    "\n"
    "OPTIONS:\n"
    "  -verbose        Print progress reports.  Use twice for LOTS of output.\n"
    "  -Fourier        Perform the alignments using Fourier interpolation.\n"
    "  -heptic         Use heptic polynomial interpolation.\n"
    "  -quintic        Use quintic polynomical interpolation.\n"
    "  -cubic          Use cubic polynomial interpolation.\n"
    "  -linear         Use linear interpolation.\n"
    "             -->>   OLD Default = Fourier [slowest and most accurate interpolator]\n"
    "             -->>   NEW Default = Heptic [7th order polynomials]\n"
    "  -clipit         Clips the values in each output sub-brick to be in the same\n"
    "                    range as the corresponding input volume.\n"
    "                    The interpolation schemes can produce values outside\n"
    "                    the input range, which is sometimes annoying.\n"
    "                    [16 Apr 2002: -clipit is now the default]\n"
    "  -noclip         Turns off -clipit\n"
    "  -zpad n         Zeropad around the edges by 'n' voxels during rotations\n"
    "                    (these edge values will be stripped off in the output)\n"
    "              N.B.: Unlike to3d, in this program '-zpad' adds zeros in\n"
    "                     all directions.\n"
    "              N.B.: The environment variable AFNI_ROTA_ZPAD can be used\n"
    "                     to set a nonzero default value for this parameter.\n"
    "  -prefix fname   Use 'fname' for the output dataset prefix.\n"
    "                    The program tries not to overwrite an existing dataset.\n"
    "                    Default = 'volreg'.\n"
    "              N.B.: If the prefix is 'NULL', no output dataset will be written.\n"
    "\n"
    "  -float          Force output dataset to be written in floating point format.\n"
    "              N.B.: If the input dataset has scale factors attached to ANY\n"
    "                    sub-bricks, then the output will always be written in\n"
    "                    float format!\n"
    "\n"
    "  -base n         Sets the base brick to be the 'n'th sub-brick\n"
    "                    from the input dataset (indexing starts at 0).\n"
    "                    Default = 0 (first sub-brick).\n"
    "  -base 'bset[n]' Sets the base brick to be the 'n'th sub-brick\n"
    "                    from the dataset specified by 'bset', as in\n"
    "                       -base 'elvis+orig[4]'\n"
    "                    The quotes are needed because the '[]' characters\n"
    "                    are special to the shell.\n"
    "\n"
    "  -dfile dname    Save the motion parameters in file 'dname'.\n"
    "                    The output is in 9 ASCII formatted columns:\n"
    "\n"
    "                    n  roll  pitch  yaw  dS  dL  dP  rmsold rmsnew\n"
    "\n"
    "           where:   n     = sub-brick index\n"
    "                    roll  = rotation about the I-S axis }\n"
    "                    pitch = rotation about the R-L axis } degrees CCW\n"
    "                    yaw   = rotation about the A-P axis }\n"
    "                      dS  = displacement in the Superior direction  }\n"
    "                      dL  = displacement in the Left direction      } mm\n"
    "                      dP  = displacement in the Posterior direction }\n"
    "                   rmsold = RMS difference between input brick and base brick\n"
    "                   rmsnew = RMS difference between output brick and base brick\n"
    "       N.B.: If the '-dfile' option is not given, the parameters aren't saved.\n"
    "       N.B.: The motion parameters are those needed to bring the sub-brick\n"
    "             back into alignment with the base.  In 3drotate, it is as if\n"
    "             the following options were applied to each input sub-brick:\n"
    "              -rotate 'roll'I 'pitch'R 'yaw'A  -ashift 'dS'S 'dL'L 'dP'P\n"
    "\n"
    "  -1Dfile ename   Save the motion parameters ONLY in file 'ename'.\n"
    "                    The output is in 6 ASCII formatted columns:\n"
    "\n"
    "                    roll pitch yaw dS  dL  dP\n"
    "\n"
    "                  This file can be used in FIM as an 'ort', to detrend\n"
    "                  the data against correlation with the movements.\n"
    "                  This type of analysis can be useful in removing\n"
    "                  errors made in the interpolation.\n"
    "\n"
    "  -1Dmatrix_save ff = Save the matrix transformation from base to input\n"
    "                      coordinates in file 'ff' (1 row per sub-brick in\n"
    "                      the input dataset).  If 'ff' does NOT end in '.1D',\n"
    "                      then the program will append '.aff12.1D' to 'ff' to\n"
    "                      make the output filename.\n"
    "               *N.B.: This matrix is the coordinate transformation from base\n"
    "                      to input DICOM coordinates.  To get the inverse matrix\n"
    "                      (input to base), use the cat_matvec program, as in\n"
    "                        cat_matvec fred.aff12.1D -I\n"
    "               *N.B.: This matrix is the inverse of the matrix stored in\n"
    "                      the output dataset VOLREG_MATVEC_* attributes.\n"
    "                      The base-to-input convention followed with this\n"
    "                      option corresponds to the convention in 3dAllineate.\n"
    "               *N.B.: 3dvolreg does not have a '-1Dmatrix_apply' option.\n"
    "                      See 3dAllineate for this function.  Also confer with\n"
    "                      program cat_matvec.\n"
    "\n"
    "  -rotcom         Write the fragmentary 3drotate commands needed to\n"
    "                  perform the realignments to stdout; for example:\n"
    "                    3drotate -rotate 7.2I 3.2R -5.7A -ashift 2.7S -3.8L 4.9P\n"
    "                  The purpose of this is to make it easier to shift other\n"
    "                  datasets using exactly the same parameters.\n"
    "\n"
    "  -maxdisp      = Print the maximum displacement (in mm) for brain voxels.\n"
    "                    ('Brain' here is defined by the same algorithm as used\n"
    "                    in the command '3dAutomask -clfrac 0.33'; the displacement\n"
    "                    for each non-interior point in this mask is calculated.)\n"
    "                    If '-verbose' is given, the max displacement will be\n"
    "                    printed to the screen for each sub-brick; otherwise,\n"
    "                    just the overall maximum displacement will get output.\n"
    "                 ** This displacement is relative to the base volume.\n"
    "                    [-maxdisp is now turned on by default]\n"
    "  -nomaxdisp    = Do NOT calculate and print the maximum displacement.\n"
    "                    [maybe it offends you in some theological sense?]\n"
    "                    [maybe you have some real 'need for speed'?]\n"
    "  -maxdisp1D mm = Do '-maxdisp' and also write the max displacement for each\n"
    "                    sub-brick into file 'mm' in 1D (columnar) format.\n"
    "                    You may find that graphing this file (cf. 1dplot)\n"
    "                    is a useful diagnostic tool for your FMRI datasets.\n"
    "                    [the 'mm' filename can be '-', which means stdout]\n"
    "                 ** The program also outputs the maximum change (delta) in\n"
    "                    displacement between 2 successive time points, into the\n"
    "                    file with name 'mm_delt'.  This output can let you see\n"
    "                    when there is a sudden head jerk, for example. [22 Jun 2015]\n"
    "\n"
    "  -savedisp sss = Save 3 3D+time datasets with the x,y,z displacments at each\n"
    "                  voxel at each time point.  The prefix for the x displacement\n"
    "                  dataset will be the string 'sss' with '_DX' appended, etc.\n"
    "                  This option is intended for use with various processing\n"
    "                  scripts now under construction, and is probably otherwise\n"
    "                  completely useless.\n"
#if 0
    "               ** If you use '-SAVEDISP', then you will get 19 datasets instead\n"
    "                  of just 3.  These are various powers of the displacements:\n"
    "                    _DX   = x       _DY   = y      _DZ   = z\n"
    "                    _DXX  = x*x     _DYY  = y*y    _DZZ  = z*z\n"
    "                    _DXY  = x*y     _DXZ  = x*z    _DYZ  = y*z\n"
    "                    _DXXX = x*x*x   _DYYY = y*y*y  _DZZZ = z*z*z\n"
    "                    _DXXY = x*x*y   _DXXZ = x*x*z  _DXYY = x*x*y\n"
    "                    _DXZZ = x*x*z   _DYYZ = y*y*z  _DYZZ = y*z*z  _DXYZ = x*y*z\n"
    "                  and are intended for use in regressing out per-voxel\n"
    "                  movements at a higher order (currently under construction).\n"
    "               ** To be precise, by 'x*x' and 'x*x*x', I actually mean\n"
    "                    Pleg2(x) = 1.5*x*x-0.5     = 2nd order Legendre polynomial\n"
    "                    Pleg3(x) = (2.5*x*x-1.5)*x = 3rd order Legendre polynomial\n"
    "               ** Also, '-SAVEDISP' will cause all 19 output datasets to be\n"
    "                  normalized, with the mean and linear trend removed, and then\n"
    "                  scaled so that the peak absolute value in each voxel's time\n"
    "                  series is 1.  These operations are for use in the mythical\n"
    "                  processing script in mythical preparation.\n"
#endif
    "\n"
    "  -tshift ii      If the input dataset is 3D+time and has slice-dependent\n"
    "                  time-offsets (cf. the output of 3dinfo -v), then this\n"
    "                  option tells 3dvolreg to time shift it to the average\n"
    "                  slice time-offset prior to doing the spatial registration.\n"
    "                  The integer 'ii' is the number of time points at the\n"
    "                  beginning to ignore in the time shifting.  The results\n"
    "                  should like running program 3dTshift first, then running\n"
    "                  3dvolreg -- this is primarily a convenience option.\n"
    "            N.B.: If the base brick is taken from this dataset, as in\n"
    "                  '-base 4', then it will be the time shifted brick.\n"
    "                  If for some bizarre reason this is undesirable, you\n"
    "                  could use '-base this+orig[4]' instead.\n"
    "\n"
    "  -rotparent rset\n"
    "    Specifies that AFTER the registration algorithm finds the best\n"
    "    transformation for each sub-brick of the input, an additional\n"
    "    rotation+translation should be performed before computing the\n"
    "    final output dataset; this extra transformation is taken from\n"
    "    the first 3dvolreg transformation found in dataset 'rset'.\n"
    "  -gridparent gset\n"
    "    Specifies that the output dataset of 3dvolreg should be shifted to\n"
    "    match the grid of dataset 'gset'.  Can only be used with -rotparent.\n"
    "    This dataset should be one this is properly aligned with 'rset' when\n"
    "    overlaid in AFNI.\n"
    "  * If 'gset' has a different number of slices than the input dataset,\n"
    "    then the output dataset will be zero-padded in the slice direction\n"
    "    to match 'gset'.\n"
    "  * These options are intended to be used to align datasets between sessions:\n"
    "     S1 = SPGR from session 1    E1 = EPI from session 1\n"
    "     S2 = SPGR from session 2    E2 = EPI from session 2\n"
    " 3dvolreg -twopass -twodup -base S1+orig -prefix S2reg S2+orig\n"
    " 3dvolreg -rotparent S2reg+orig -gridparent E1+orig -prefix E2reg \\\n"
    "          -base 4 E2+orig\n"
    "     Each sub-brick in E2 is registered to sub-brick E2+orig[4], then the\n"
    "     rotation from S2 to S2reg is also applied, which shifting+padding\n"
    "     applied to properly overlap with E1.\n"
    "  * A similar effect could be done by using commands\n"
    " 3dvolreg -twopass -twodup -base S1+orig -prefix S2reg S2+orig\n"
    " 3dvolreg -prefix E2tmp -base 4 E2+orig\n"
    " 3drotate -rotparent S2reg+orig -gridparent E1+orig -prefix E2reg E2tmp+orig\n"
    "    The principal difference is that the latter method results in E2\n"
    "    being interpolated twice to make E2reg: once in the 3dvolreg run to\n"
    "    produce E2tmp, then again when E2tmp is rotated to make E2reg.  Using\n"
    "    3dvolreg with the -rotparent and -gridparent options simply skips the\n"
    "    intermediate interpolation.\n"
    "\n"
    "          *** Please read file README.registration for more   ***\n"
    "          *** information on the use of 3dvolreg and 3drotate ***\n"
    "\n"
    " Algorithm: Iterated linearized weighted least squares to make each\n"
    "              sub-brick as like as possible to the base brick.\n"
    "              This method is useful for finding SMALL MOTIONS ONLY.\n"
    "              See program 3drotate for the volume shift/rotate algorithm.\n"
    "              The following options can be used to control the iterations:\n"
    "                -maxite     m = Allow up to 'm' iterations for convergence\n"
    "                                  [default = %d].\n"
    "                -x_thresh   x = Iterations converge when maximum movement\n"
    "                                  is less than 'x' voxels [default=%f],\n"
    "                -rot_thresh r = And when maximum rotation is less than\n"
    "                                  'r' degrees [default=%f].\n"
    "                -delta      d = Distance, in voxel size, used to compute\n"
    "                                  image derivatives using finite differences\n"
    "                                  [default=%f].\n"
    "                -final   mode = Do the final interpolation using the method\n"
    "                                  defined by 'mode', which is one of the\n"
    "                                  strings 'NN', 'cubic', 'quintic', 'heptic',\n"
    "                                  or 'Fourier' or 'linear'\n"
    "                                  [default=mode used to estimate parameters].\n"
    "            -weight 'wset[n]' = Set the weighting applied to each voxel\n"
    "                                  proportional to the brick specified here\n"
    "                                  [default=smoothed base brick].\n"
    "                                N.B.: if no weight is given, and -twopass is\n"
    "                                  engaged, then the first pass weight is the\n"
    "                                  blurred sum of the base brick and the first\n"
    "                                  data brick to be registered.\n"
    "                   -edging ee = Set the size of the region around the edges of\n"
    "                                  the base volume where the default weight will\n"
    "                                  be set to zero.  If 'ee' is a plain number,\n"
    "                                  then it is a voxel count, giving the thickness\n"
    "                                  along each face of the 3D brick.  If 'ee' is\n"
    "                                  of the form '5%%', then it is a fraction of\n"
    "                                  of each brick size.  For example, '5%%' of\n"
    "                                  a 256x256x124 volume means that 13 voxels\n"
    "                                  on each side of the xy-axes will get zero\n"
    "                                  weight, and 6 along the z-axis.  If this\n"
    "                                  option is not used, then 'ee' is read from\n"
    "                                  the environment variable AFNI_VOLREG_EDGING.\n"
    "                                  If that variable is not set, then 5%% is used.\n"
    "                                N.B.: This option has NO effect if the -weight\n"
    "                                  option is used.\n"
    "                                N.B.: The largest %% value allowed is 25%%.\n"
    "                     -twopass = Do two passes of the registration algorithm:\n"
    "                                 (1) with smoothed base and data bricks, with\n"
    "                                     linear interpolation, to get a crude\n"
    "                                     alignment, then\n"
    "                                 (2) with the input base and data bricks, to\n"
    "                                     get a fine alignment.\n"
    "                                  This method is useful when aligning high-\n"
    "                                  resolution datasets that may need to be\n"
    "                                  moved more than a few voxels to be aligned.\n"
    "                  -twoblur bb = 'bb' is the blurring factor for pass 1 of\n"
    "                                  the -twopass registration.  This should be\n"
    "                                  a number >= 2.0 (which is the default).\n"
    "                                  Larger values would be reasonable if pass 1\n"
    "                                  has to move the input dataset a long ways.\n"
    "                                  Use '-verbose -verbose' to check on the\n"
    "                                  iterative progress of the passes.\n"
    "                                N.B.: when using -twopass, and you expect the\n"
    "                                  data bricks to move a long ways, you might\n"
    "                                  want to use '-heptic' rather than\n"
    "                                  the default '-Fourier', since you can get\n"
    "                                  wraparound from Fourier interpolation.\n"
    "                      -twodup = If this option is set, along with -twopass,\n"
    "                                  then the output dataset will have its\n"
    "                                  xyz-axes origins reset to those of the\n"
    "                                  base dataset.  This is equivalent to using\n"
    "                                  '3drefit -duporigin' on the output dataset.\n"
    "                       -sinit = When using -twopass registration on volumes\n"
    "                                  whose magnitude differs significantly, the\n"
    "                                  least squares fitting procedure is started\n"
    "                                  by doing a zero-th pass estimate of the\n"
    "                                  scale difference between the bricks.\n"
    "                                  Use this option to turn this feature OFF.\n"
    "              -coarse del num = When doing the first pass, the first step is\n"
    "                                  to do a number of coarse shifts in order to\n"
    "                                  find a starting point for the iterations.\n"
    "                                  'del' is the size of these steps, in voxels;\n"
    "                                  'num' is the number of these steps along\n"
    "                                  each direction (+x,-x,+y,-y,+z,-z).  The\n"
    "                                  default values are del=10 and num=2.  If\n"
    "                                  you don't want this step performed, set\n"
    "                                  num=0.  Note that the amount of computation\n"
    "                                  grows as num**3, so don't increase num\n"
    "                                  past 4, or the program will run forever!\n"
    "                             N.B.: The 'del' parameter cannot be larger than\n"
    "                                   10%% of the smallest dimension of the input\n"
    "                                   dataset.\n"
    "              -coarserot        Also do a coarse search in angle for the\n"
    "                                  starting point of the first pass.\n"
    "              -nocoarserot      Don't search angles coarsely.\n"
    "                                  [-coarserot is now the default - RWCox]\n"
#if 0
    "              -wtrim          = Attempt to trim the intermediate volumes to\n"
    "                                  a smaller region (determined by the weight\n"
    "                                  volume).  The purpose of this is to save\n"
    "                                  memory.  The use of '-verbose -verbose'\n"
    "                                  will report on the trimming usage.\n"
    "                             N.B.: At some point in the future, -wtrim will\n"
    "                                   become the default.\n"
#endif
    "              -wtinp          = Use sub-brick[0] of the input dataset as the\n"
    "                                  weight brick in the final registration pass.\n"
    "\n"
    " N.B.: * This program can consume VERY large quantities of memory.\n"
    "          (Rule of thumb: 40 bytes per input voxel.)\n"
    "          Use of '-verbose -verbose' will show the amount of workspace,\n"
    "          and the steps used in each iteration.\n"
    "       * ALWAYS check the results visually to make sure that the program\n"
    "          wasn't trapped in a 'false optimum'.\n"
    "       * The default rotation threshold is reasonable for 64x64 images.\n"
    "          You may want to decrease it proportionally for larger datasets.\n"
    "       * -twopass resets the -maxite parameter to 66; if you want to use\n"
    "          a different value, use -maxite AFTER the -twopass option.\n"
    "       * The -twopass option can be slow; several CPU minutes for a\n"
    "          256x256x124 volume is a typical run time.\n"
    "       * After registering high-resolution anatomicals, you may need to\n"
    "          set their origins in 3D space to match.  This can be done using\n"
    "          the '-duporigin' option to program 3drefit, or by using the\n"
    "          '-twodup' option to this program.\n"

   , VL_maxite , VL_dxy , VL_dph , VL_del ) ;

   PRINT_COMPILE_DATE ; return ;
}

/*---------------------------------------------------------------------*/

void VL_command_line(void)
{
   int ii , nxbase , nybase , nerr , basecode ;
   MRI_IMAGE * tim ;
   MRI_IMARR * tarr ;
   float bdx,bdy,bdz ;

   /***========= look for options on command line =========***/

   while( Iarg < Argc && Argv[Iarg][0] == '-' ){

      if( strcmp(Argv[Iarg],"-input") == 0 || strcmp(Argv[Iarg],"-source") == 0 ){
        if( VL_dset != NULL ) ERROR_exit("Can't have 2 '%s' options!",Argv[Iarg]) ;
        VL_dset = THD_open_dataset( Argv[++Iarg] ) ;
        if( VL_dset == NULL ) ERROR_exit("Can't open input dataset '%s'",Argv[Iarg]) ;
        Iarg++ ; continue ;
      }

      if( strcmp(Argv[Iarg],"-maxdisp") == 0 ){  /* 03 Aug 2006 */
        VL_maxdisp = 1 ; Iarg++ ; continue ;
      }
      if( strcmp(Argv[Iarg],"-nomaxdisp") == 0 ){
        VL_maxdisp = 0 ; Iarg++ ; continue ;
      }
      if( strcmp(Argv[Iarg],"-maxdisp1D") == 0 ){
        char *eq ;
        VL_maxdisp = 1 ; MCW_strncpy(VL_dmaxfile,Argv[++Iarg],998) ;
        eq = modify_afni_prefix(VL_dmaxfile,NULL,"_delt") ;
        if( eq != NULL ) MCW_strncpy(VL_emaxfile,eq,998) ;
        Iarg++ ; continue ;
      }

      /** -savedisp sss [04 Apr 2012] **/

      if( strcmp(Argv[Iarg],"-savedisp") == 0 || strcmp(Argv[Iarg],"-SAVEDISP") == 0 ){
        int lll ;
        if( ++Iarg > Argc ) ERROR_exit("need argument after %s",Argv[Iarg-1]) ;
        VL_savedisp = strdup(Argv[Iarg]) ;
        if( !THD_filename_ok(VL_savedisp) )
          ERROR_exit("%s '%s' is invalid output dataset name",Argv[Iarg-1],VL_savedisp) ;
        VL_savedisp19 = (strcmp(Argv[Iarg-1],"-SAVEDISP") == 0) ;

        VL_savedisp_prA = strdup(VL_savedisp) ; lll = strlen(VL_savedisp_prA ) ;

        /* prA = first part of output prefix (to be appended with "_DX" etc.),
           prB = last part of output prefix (".nii" or ".nii.gz" or nothing at all) */

        if( STRING_HAS_SUFFIX(VL_savedisp_prA,".nii") ){
          if( lll < 5 )
            ERROR_exit("%s '%s' is invalid output dataset name",Argv[Iarg-1],VL_savedisp) ;
          VL_savedisp_prA[lll-4] = '\0' ;
          VL_savedisp_prB        = strdup(".nii") ;
        } else if( STRING_HAS_SUFFIX(VL_savedisp_prA,".nii.gz") ){
          if( lll < 8 )
            ERROR_exit("%s '%s' is invalid output dataset name",Argv[Iarg-1],VL_savedisp) ;
          VL_savedisp_prA[lll-7] = '\0' ;
          VL_savedisp_prB        = strdup(".nii.gz") ;
        } else {
          VL_savedisp_prB = strdup("\0") ;
        }

        Iarg++ ; continue ;
      }

      /** -1Dmatrix_save [24 Jul 2007] **/

      if( strcmp(Argv[Iarg],"-1Dmatrix_save") == 0 ){
        if( VL_matrix_save_1D != NULL )
          ERROR_exit("Can't have multiple %s options!",Argv[Iarg]);
        if( ++Iarg >= Argc )
          ERROR_exit("no argument after '%s'!",Argv[Iarg-1]) ;
        if( !THD_filename_ok(Argv[Iarg]) )
          ERROR_exit("badly formed filename: %s '%s'",Argv[Iarg-1],Argv[Iarg]) ;
        if( STRING_HAS_SUFFIX(Argv[Iarg],".1D") ){
          VL_matrix_save_1D = Argv[Iarg] ;
        } else {
          VL_matrix_save_1D = calloc(sizeof(char),strlen(Argv[Iarg])+16) ;
          strcpy(VL_matrix_save_1D,Argv[Iarg]) ; strcat(VL_matrix_save_1D,".aff12.1D") ;
        }
        Iarg++ ; continue ;
      }

      /** -sinit [22 Mar 2004] **/

      if( strcmp(Argv[Iarg],"-sinit") == 0 ){
        VL_sinit = 0 ; Iarg++ ; continue ;
      }

      /** -params [not in the help list] **/

      if( strncmp(Argv[Iarg],"-params",4) == 0 ){
         VL_maxite = strtol( Argv[++Iarg] , NULL , 10 ) ;
         VL_dxy    = strtod( Argv[++Iarg] , NULL ) ;      /* voxels */
         VL_dph    = strtod( Argv[++Iarg] , NULL ) ;      /* degrees */
         VL_del    = strtod( Argv[++Iarg] , NULL ) ;      /* voxels */
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-maxite",4) == 0 ){
         int mit = strtol( Argv[++Iarg] , NULL , 10 ) ;
         if( mit > 0 ) VL_maxite = mit ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-x_thresh",4) == 0 ){
         float dxy = strtod( Argv[++Iarg] , NULL ) ;
         if( dxy > 0.0 ) VL_dxy = dxy ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-rot_thresh",6) == 0 ){
         float dph = strtod( Argv[++Iarg] , NULL ) ;
         if( dph > 0.0 ) VL_dph = dph ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-delta",4) == 0 ){
         float del = strtod( Argv[++Iarg] , NULL ) ;
         if( del > 0.0 ) VL_del = del ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-final",4) == 0 ){  /* 20 Nov 1998 */
         char * str = Argv[++Iarg] ;

              if( strcmp(str,"cubic")   == 0 ) VL_final = MRI_CUBIC ;
         else if( strcmp(str,"quintic") == 0 ) VL_final = MRI_QUINTIC ;
         else if( strcmp(str,"heptic")  == 0 ) VL_final = MRI_HEPTIC ;
         else if( strcmp(str,"Fourier") == 0 ) VL_final = MRI_FOURIER ;
         else if( strcmp(str,"NN")      == 0 ) VL_final = MRI_NN ;     /* 02 Apr 2003 */
         else if( strcmp(str,"linear")  == 0 ) VL_final = MRI_LINEAR ; /* 22 Dec 2015 */
         else {
           fprintf(stderr,"** Illegal mode after -final\n"); exit(1);
         }
         Iarg++ ; continue ;
      }

      if( strcmp(Argv[Iarg],"-rotcom") == 0 ){  /* 04 Sep 2000 */
         VL_rotcom++ ;
         Iarg++ ; continue ;
      }

      if( strcmp(Argv[Iarg],"-twopass") == 0 ){ /* 11 Sep 2000 */
         VL_twopass++ ;
         if( VL_maxite < 10 ) VL_maxite = 66 ;
         Iarg++ ; continue ;
      }

      if( strcmp(Argv[Iarg],"-twoblur") == 0 ){ /* 11 Sep 2000 */
         VL_twoblur = strtod( Argv[++Iarg] , NULL ) ;
         if( VL_twoblur < 2.0 ){
            fprintf(stderr,"** ERROR: value after -twoblur is < 2.0\n") ;
            exit(1) ;
         }
         Iarg++ ; continue ;
      }

      /** -verbose **/

      if( strncmp(Argv[Iarg],"-verbose",5) == 0 ){
         VL_verbose++ ;
         Iarg++ ; continue ;
      }

      /** -clipit **/

      if( strncmp(Argv[Iarg],"-clipit",4) == 0 ){
         fprintf(stderr,"++ Notice: -clipit is now the default\n") ;
         VL_clipit = 1 ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-noclip",4) == 0 ){
         VL_clipit = 0 ;
         Iarg++ ; continue ;
      }

      /** -zpad [05 Feb 2001] */

      if( strncmp(Argv[Iarg],"-zpad",5) == 0 ){     /* 05 Feb 2001 */
         if( VL_zpad > 0 )
            fprintf(stderr,"++ WARNING: second -zpad option!\n") ;
         VL_zpad = (int) strtod( Argv[++Iarg] , NULL ) ;
         if( VL_zpad < 0 ){
            fprintf(stderr,"** ERROR: Can't use -zpad %d\n",VL_zpad) ;
            exit(1) ;
         }
         THD_rota_setpad(VL_zpad,VL_zpad,VL_zpad) ;
         Iarg++ ; continue ;
      }

      /** -Fourier **/

      if( strncmp(Argv[Iarg],"-Fourier",4) == 0 ||
          strncmp(Argv[Iarg],"-fourier",4) == 0   ){

         VL_resam = MRI_FOURIER ;
         Iarg++ ; continue ;
      }

      /** -linear [not in -help output] **/

      if( strncmp(Argv[Iarg],"-linear",4) == 0 ||
          strncmp(Argv[Iarg],"-Linear",4) == 0   ){

         VL_resam = MRI_LINEAR ;
         Iarg++ ; continue ;
      }

      /** -cubic **/

      if( strncmp(Argv[Iarg],"-cubic",4) == 0 ||
          strncmp(Argv[Iarg],"-Cubic",4) == 0   ){

         VL_resam = MRI_CUBIC ;
         Iarg++ ; continue ;
      }

      /** -quintic **/

      if( strncmp(Argv[Iarg],"-quintic",4) == 0 ||
          strncmp(Argv[Iarg],"-Quintic",4) == 0   ){

         VL_resam = MRI_QUINTIC ;
         Iarg++ ; continue ;
      }

      /** -heptic **/

      if( strncmp(Argv[Iarg],"-heptic",4) == 0 ||
          strncmp(Argv[Iarg],"-Heptic",4) == 0   ){

         VL_resam = MRI_HEPTIC ;
         Iarg++ ; continue ;
      }


      /** -prefix **/

      if( strncmp(Argv[Iarg],"-prefix",4) == 0 ){
         strcpy( VL_prefix , Argv[++Iarg] ) ;
         Iarg++ ; continue ;
      }

      /** -dfile **/

      if( strncmp(Argv[Iarg],"-dfile",4) == 0 ){
         strcpy( VL_dfile , Argv[++Iarg] ) ;
         Iarg++ ; continue ;
      }

      /** -1Dfile [14 Apr 2000] **/

      if( strncmp(Argv[Iarg],"-1Dfile",4) == 0 ){
         strcpy( VL_1Dfile , Argv[++Iarg] ) ;
         Iarg++ ; continue ;
      }

      /** -base **/

      if( strncmp(Argv[Iarg],"-base",4) == 0 ){
        int bb,ii ; char * cpt ;

        if( VL_imbase != NULL || VL_nbase > 0 ){
           fprintf(stderr,"** Can't have two -base arguments\n") ; exit(1) ;
        }

        /* try an integer */

        bb = strtol( Argv[++Iarg] , &cpt , 10 ) ;
        if( bb < 0 )
          ERROR_exit("Illegal negative number after -base") ;

        if( *cpt == '\0' ){  /* it WAS an integer */

          VL_nbase  = bb ;
          VL_imbase = NULL ;
          VL_intern = 1 ;

        } else {             /* it WAS NOT an integer */

          float fac ;

          /* 06 Feb 2001: now we store the base dataset in a global variable */
          /* 13 Sep 2000: replaced old code with use of THD_open_dataset()   */

          VL_bset = THD_open_dataset( Argv[Iarg] ) ;
          if( VL_bset == NULL ){
            ERROR_exit("Couldn't open -base dataset %s",Argv[Iarg]) ;
          }
          if( VL_verbose )
            fprintf(stderr,
                    "++ Reading in base dataset %s\n",DSET_BRIKNAME(VL_bset)) ;
          DSET_load(VL_bset) ; CHECK_LOAD_ERROR(VL_bset) ;
          if( DSET_NVALS(VL_bset) > 1 )
             WARNING_message("-base dataset %s has more than 1 sub-brick",
                             Argv[Iarg]) ;

          VL_intern = 0 ;   /* not internal to input dataset */

          bdx = fabs(DSET_DX(VL_bset)) ;  /* save for comparison later */
          bdy = fabs(DSET_DY(VL_bset)) ;  /* (14 Sep 2000)            */
          bdz = fabs(DSET_DZ(VL_bset)) ;

          fac = DSET_BRICK_FACTOR(VL_bset,0) ;

          /* 10 Apr 2001: Tom Ross noticed that the "bb" which
                          used to be here as the brick selector
                          was no longer defined, and should be
                          replaced by 0, which I just did -- RWCox.
                                                                  |
                                                                  v */
          VL_imbase = mri_scale_to_float( fac, DSET_BRICK(VL_bset,0) ) ;

          VL_bxorg = VL_bset->daxes->xxorg ;                   /* 08 Dec 2000 */
          VL_byorg = VL_bset->daxes->yyorg ;
          VL_bzorg = VL_bset->daxes->zzorg ;

          DSET_unload( VL_bset ) ;  /* 06 Feb 2001: unload instead of delete */
        }
        Iarg++ ; continue ;
      }

      /** 11 Dec 2000: -coarse **/

      if( strcmp(Argv[Iarg],"-coarse") == 0 ){
         VL_coarse_del = strtol(Argv[++Iarg],NULL,10) ;
         VL_coarse_num = strtol(Argv[++Iarg],NULL,10) ;
         Iarg++ ; continue ;
      }

      if( strcmp(Argv[Iarg],"-coarserot") == 0 ){  /* 01 Dec 2005 */
         INFO_message("-coarserot is now the default") ;
         VL_coarse_rot = 1 ;
         Iarg++ ; continue ;
      }
      if( strcmp(Argv[Iarg],"-nocoarserot") == 0 ){ /* 04 Aug 2006 */
         VL_coarse_rot = 0 ;
         Iarg++ ; continue ;
      }

      /** 06 Jun 2002: -wtrim **/

      if( strcmp(Argv[Iarg],"-wtrim") == 0 ){
#if 0
         mri_3dalign_wtrimming(1) ;
#else
         fprintf(stderr,"++ Notice: -wtrim is now always enabled\n"); /* 22 Mar 2004 */
#endif
         Iarg++ ; continue ;
      }

      /** 06 Jun 2002: -wtinp **/

      if( strcmp(Argv[Iarg],"-wtinp") == 0 ){
         VL_wtinp = 1 ;
         Iarg++ ; continue ;
      }

      /** 10 Dec 2000: -edging **/

      if( strcmp(Argv[Iarg],"-edging") == 0 ){
        float ee ; char * eq ;
        ee = strtod(Argv[++Iarg],&eq) ;
        if( ee < 0 ){
           fprintf(stderr,"** Illegal value after -edging\n"); exit(1);
        }
        if( *eq == '%' ){
           if( ee > 25.0 ){
              fprintf(stderr,"** Illegal percentage after -edging\n"); exit(1);
           }
           VL_edperc = 1 ; VL_edging = ee ;
        } else {
           VL_edperc = 0 ; VL_edging = ee ;
        }
        Iarg++ ; continue ;
      }

      /** -weight **/

      if( strncmp(Argv[Iarg],"-weight",4) == 0 ){
        int bb,ii ; char *cpt ;
        THD_3dim_dataset *wset ;
        char dname[256] ; float fac ;

        if( VL_imwt != NULL ){
           fprintf(stderr,"** Can't have two -weight options\n") ; exit(1) ;
        }

        /* break it into 'wset[bb]' pieces */

        cpt = strstr( Argv[++Iarg] , "[" ) ;
        if( cpt == NULL || cpt == Argv[Iarg] ){
           fprintf(stderr,"** Illegal weight dataset after -weight\n"); exit(1);
        }
        ii = cpt - Argv[Iarg] ;
        memcpy(dname,Argv[Iarg],ii) ; dname[ii] = '\0' ;
        bb = -1 ; sscanf( cpt+1 , "%d" , &bb ) ;
        if( bb < 0 ){
           fprintf(stderr,"** Illegal sub-brick selector after -weight\n"); exit(1);
        }
        wset = THD_open_one_dataset( dname ) ;
        if( wset == NULL ){
           fprintf(stderr,"** Can't open weight dataset %s\n",dname); exit(1);
        }
        if( bb >= DSET_NVALS(wset) ){
           fprintf(stderr,"** Illegal sub-brick selector for dataset %s\n",dname);
           exit(1) ;
        }
        if( VL_verbose )
           fprintf(stderr,"++ Reading in weight dataset %s\n",DSET_BRIKNAME(wset)) ;
        DSET_load(wset) ;
        fac = DSET_BRICK_FACTOR(wset,bb) ;
        VL_imwt = mri_scale_to_float( fac , DSET_BRICK(wset,bb) ) ;  /* copy this */
        DSET_delete( wset ) ;                                        /* toss this */
        Iarg++ ; continue ;
      }

      /** 08 Dec 2000: -twodup **/

      if( strcmp(Argv[Iarg],"-twodup") == 0 ){
        VL_twodup++ ; Iarg++ ; continue ;
      }

      /** 15 Feb 2001: -tshift **/

      if( strcmp(Argv[Iarg],"-tshift") == 0 ){
         VL_tshift = 1 ;
         VL_tshift_ignore = (int) strtod(Argv[++Iarg],NULL) ;
         if( VL_tshift_ignore < 0 ) ERREX("-tshift parameter is negative!") ;
         Iarg++ ; continue ;
      }

      /** 14 Feb 2001: -rotpar and -gridpar **/

      if( strncmp(Argv[Iarg],"-rotpar",7) == 0 ){
         ATR_float * atr ;

         if( VL_rotpar_dset != NULL )
            ERREX("Can't use -2 -rotparent options!") ;

         VL_rotpar_dset = THD_open_one_dataset( Argv[++Iarg] ) ;
         if( VL_rotpar_dset == NULL )
            ERREX("Can't open -rotparent dataset!\n") ;

         atr = THD_find_float_atr( VL_rotpar_dset->dblk , "VOLREG_MATVEC_000000" ) ;
         if( atr == NULL || atr->nfl < 12 )
            ERREX("-rotparent dataset doesn't have VOLREG attributes!?") ;

         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-gridpar",7) == 0 ){

         if( VL_gridpar_dset != NULL )
            ERREX("Can't use -2 -gridparent options!") ;

         VL_gridpar_dset = THD_open_one_dataset( Argv[++Iarg] ) ;
         if( VL_gridpar_dset == NULL )
            ERREX("Can't open -gridparent dataset!\n") ;

         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-float",6) == 0 ){  /* 07 Jan 2008 */
        VL_floatize = 1 ; Iarg++ ; continue ;
      }

      /***** get to here ==> bad news! *****/

      fprintf(stderr,"** Unknown option: %s\a\n",Argv[Iarg]) ; exit(1) ;
   }
   /***========== end of loop over options; only input dataset left ==========***/

   /*** 08 Dec 2000: check some twopass options ***/

   if( VL_twodup ){
      if( !VL_twopass || VL_intern ) VL_twodup == 0 ;
   }

   /*** 14 Feb 2001 ***/

   if( VL_gridpar_dset != NULL && VL_rotpar_dset == NULL ){
      fprintf(stderr,
              "++ WARNING: -gridparent means nothing without -rotparent!\n");
      DSET_delete( VL_gridpar_dset ) ;
      VL_gridpar_dset = NULL ;
   }

   if( VL_rotpar_dset != NULL && VL_twopass ){
      fprintf(stderr,
              "++ WARNING: Combining -rotparent and -twopass isn't recommended!\n");
   }

   if( VL_gridpar_dset != NULL && VL_twodup ){
      fprintf(stderr,"++ WARNING: -gridparent overrides -twodup!\n") ;
      VL_twodup = 0 ;
   }

   /*** 10 Dec 2000: if -edging not set, check environment ***/

   if( VL_edperc < 0 ){
      char *ef=my_getenv("AFNI_VOLREG_EDGING") , *eq ;
      float ee ;

      if( ef == NULL ){
         VL_edperc = 1 ; VL_edging = 5.0 ;
      } else {
        ee = strtod(ef,&eq) ;
        if( ee < 0 ){
           fprintf(stderr,"** Illegal value in AFNI_VOLREG_EDGING\n"); exit(1);
        }
        if( *eq == '%' ){
           if( ee > 25.0 ){
              fprintf(stderr,"** Illegal percentage in AFNI_VOLREG_EDGING\n"); exit(1);
           }
           VL_edperc = 1 ; VL_edging = ee ;
        } else {
           VL_edperc = 0 ; VL_edging = ee ;
        }
      }
   }

   /***---------- Open the dataset to be registered ----------***/

   if( VL_dset == NULL ){
          if( Iarg >= Argc  ) ERROR_exit("Too few arguments!?  Last=%s",Argv[Argc-1]);
     else if( Iarg < Argc-1 ) ERROR_exit("Too many arguments?!  Dataset=%s?",Argv[Iarg]);

     VL_dset = THD_open_dataset( Argv[Iarg] ) ;

   } else if( Iarg < Argc ){
     WARNING_message("Skipping argument(s) after last option? '%s'",Argv[Iarg]) ;
   }

   if( !VL_floatize && THD_need_brick_factor(VL_dset) ){  /* 07 Jan 2008 */
     VL_floatize = 1 ; VL_floatize_forced = 1 ;
     WARNING_message(
       "Input dataset has scale factors ==> output will be stored as floats!");
   }

   /** Check for errors **/

   if( VL_dset == NULL ){
      fprintf(stderr,"** Can't open dataset %s\n",Argv[Iarg]) ; exit(1) ;
   }

   if( VL_bset == NULL ) VL_bset = VL_dset ;  /* default base */
   VL_cen_bas = THD_dataset_center(VL_bset) ;  /* 11 Mar 2008 */
   VL_cen_inp = THD_dataset_center(VL_dset) ;
   if( VL_bset != VL_dset ){
     THD_fvec3 dv ; float dd ;
     dv = SUB_FVEC3(VL_cen_bas,VL_cen_inp) ;
     VL_cen_dist = SIZE_FVEC3(dv) ;
     if( VL_cen_dist > 0.01f && VL_verbose )
       INFO_message("centers of base and input datasets are %.2f mm apart",VL_cen_dist);
   }

   if( VL_tshift ){
      if( DSET_NVALS(VL_dset) < 2 ){
         fprintf(stderr,"++ WARNING: -tshift used on a 1 brick dataset!\n") ;
         VL_tshift = 0 ;
      } else if( VL_dset->taxis == NULL || VL_dset->taxis->nsl < DSET_NZ(VL_dset) ){
         fprintf(stderr,"++ WARNING: -tshift used on a dataset with no time-offsets!\n") ;
         VL_tshift = 0 ;
      } else if( VL_tshift_ignore > DSET_NVALS(VL_dset)-5 ){
         fprintf(stderr,"++ WARNING: -tshift ignore is too large for this dataset!\n") ;
         VL_tshift = 0 ;
      } else if( VL_imbase == NULL && VL_nbase < VL_tshift_ignore ){
         fprintf(stderr,"++ WARNING: base brick is prior to -tshift ignore point.\n") ;
      }
   }

   if( VL_imbase == NULL && VL_nbase >= DSET_NVALS(VL_dset) ){
      fprintf(stderr,"** Dataset %s doesn't have base brick index = %d\n",
              Argv[Iarg] , VL_nbase ) ;
      exit(1) ;
   }

   /*-- 27 Feb 2001: do a better check for mismatch between base and input --*/
#if 1
   if( VL_bset != NULL && VL_bset != VL_dset ){
      int mm = THD_dataset_mismatch( VL_dset , VL_bset ) , nn=0 ;

      if( mm & MISMATCH_DIMEN ){
         fprintf(stderr,
                 "** Input %s and base %s don't have same dimensions!\n"
                 "   Input: nx=%d  ny=%d  nz=%d\n"
                 "   Base:  nx=%d  ny=%d  nz=%d\n",
                 DSET_HEADNAME(VL_dset) , DSET_HEADNAME(VL_bset) ,
                 DSET_NX(VL_dset) , DSET_NY(VL_dset) , DSET_NZ(VL_dset) ,
                 DSET_NX(VL_bset) , DSET_NY(VL_bset) , DSET_NZ(VL_bset)  ) ;
         nn++ ;
      }

      if( mm & MISMATCH_DELTA ){
         fprintf(stderr,
                 "** Input %s and base %s don't have same grid spacing!\n"
                 "   Input: dx=%6.3f  dy=%6.3f  dz=%6.3f\n"
                 "   Base:  dx=%6.3f  dy=%6.3f  dz=%6.3f\n",
                 DSET_HEADNAME(VL_dset) , DSET_HEADNAME(VL_bset) ,
                 DSET_DX(VL_dset) , DSET_DY(VL_dset) , DSET_DZ(VL_dset) ,
                 DSET_DX(VL_bset) , DSET_DY(VL_bset) , DSET_DZ(VL_bset)  ) ;
         nn++ ;
      }

      if( mm & MISMATCH_ORIENT ){
         fprintf(stderr,
                 "** Input %s and base %s don't have same orientation!\n"
                 "   Input: %s %s %s\n"
                 "   Base:  %s %s %s \n" ,
                 DSET_HEADNAME(VL_dset) , DSET_HEADNAME(VL_bset) ,
                 ORIENT_shortstr[VL_dset->daxes->xxorient] ,
                 ORIENT_shortstr[VL_dset->daxes->yyorient] ,
                 ORIENT_shortstr[VL_dset->daxes->zzorient] ,
                 ORIENT_shortstr[VL_bset->daxes->xxorient] ,
                 ORIENT_shortstr[VL_bset->daxes->yyorient] ,
                 ORIENT_shortstr[VL_bset->daxes->zzorient]  ) ;
         nn++ ;
      }

      if( nn > 0 ){
         fprintf(stderr,
                 "** FATAL ERROR: perhaps you could make your datasets match?\n") ;
         exit(1) ;
      }
   }
#else /* the old code */
   if( VL_imbase != NULL && ( VL_imbase->nx != DSET_NX(VL_dset) ||
                              VL_imbase->ny != DSET_NY(VL_dset) ||
                              VL_imbase->nz != DSET_NZ(VL_dset)   ) ){

      fprintf(stderr,"** Dataset %s doesn't conform to dimensions of base brick\n",
               Argv[Iarg] ) ;
      fprintf(stderr,"    Base    has nx = %d  ny = %d  nz = %d\n",
                     VL_imbase->nx, VL_imbase->ny, VL_imbase->nz ) ;
      fprintf(stderr,"    Dataset has nx = %d  ny = %d  nz = %d\n",
                     DSET_NX(VL_dset) , DSET_NY(VL_dset) , DSET_NZ(VL_dset) ) ;
      exit(1) ;
   }

   if( VL_imbase != NULL &&                  /* 14 Sep 2000 */
      ( fabs(DSET_DX(VL_dset)) != bdx ||
        fabs(DSET_DY(VL_dset)) != bdy ||
        fabs(DSET_DZ(VL_dset)) != bdz   ) ){

     fprintf(stderr,"++ WARNING:\n"
                    "++ Dataset %s and base have different grid spacings:\n"
                    "++ Dataset: dx=%9.3f  dy=%9.3f  dz=%9.3f\n"
                    "++    Base: dx=%9.3f  dy=%9.3f  dz=%9.3f\n" ,
             Argv[Iarg] ,
             fabs(DSET_DX(VL_dset)),fabs(DSET_DY(VL_dset)),fabs(DSET_DZ(VL_dset)),
             bdx,bdy,bdz ) ;
   }
#endif /* 27 Feb 2001: end of #if-ing out old code */

   if( VL_imwt != NULL && ( VL_imwt->nx != DSET_NX(VL_dset) ||
                            VL_imwt->ny != DSET_NY(VL_dset) ||
                            VL_imwt->nz != DSET_NZ(VL_dset)   ) ){

      fprintf(stderr,"** Dataset %s doesn't conform to dimensions of weight brick\n",
               Argv[Iarg] ) ;
      fprintf(stderr,"    Weight  has nx = %d  ny = %d  nz = %d\n",
                     VL_imwt->nx, VL_imwt->ny, VL_imwt->nz ) ;
      fprintf(stderr,"    Dataset has nx = %d  ny = %d  nz = %d\n",
                     DSET_NX(VL_dset) , DSET_NY(VL_dset) , DSET_NZ(VL_dset) ) ;
      exit(1) ;
   }

   if( VL_intern && DSET_NVALS(VL_dset) == 1 )
     ERROR_exit("You can't register a 1 brick dataset to itself!") ;

   /* 15 Mar 2001: adjust VL_coarse_del, perhaps */

   if( VL_twopass && VL_coarse_del > 0 && VL_coarse_num > 0 ){
     int mm ;
     mm = MIN( DSET_NX(VL_dset) , DSET_NY(VL_dset) ) ;
     mm = MIN( DSET_NZ(VL_dset) , mm ) ;
     mm = (int)(0.1*mm + 0.499) ; if( mm < 1 ) mm = 1 ; /* suggested by DRG */
     if( VL_coarse_del > mm ){
       fprintf(stderr,"++ Coarse del was %d, replaced with %d\n",VL_coarse_del,mm) ;
       VL_coarse_del = mm ;
     }
   }

   /* 06 Jun 2002 */

   if( VL_imwt != NULL && VL_wtinp ){
     fprintf(stderr,"++ Input weight file overrides -wtinp option!\n") ;
     VL_wtinp = 0 ;
   }

   /*** done (we hope) ***/

   return ;
}

/*--------------------------------------------------------------------
  Calculate
                 (      [                                 2 ] )
             min ( sum  [ {a v(i-dx,j-dy,k-dz) - b(i,j,k)}  ] )
              a  (  ijk [                                   ] )

  where the sum is taken over voxels at least 'edge' in from the edge.
  'edge' must be bigger than max(|dx|,|dy|,|dz|).
----------------------------------------------------------------------*/

#define B(i,j,k) b[(i)+(j)*nx+(k)*nxy]
#define V(i,j,k) v[(i)+(j)*nx+(k)*nxy]

float voldif( int nx, int ny, int nz, float *b,
              int dx, int dy, int dz, float *v, int edge )
{
   int nxy=nx*ny, nxtop=nx-edge, nytop=ny-edge, nztop=nz-edge , ii,jj,kk ;
   float bbsum=0.0 , vvsum=0.0 , bvsum=0.0 , bb,vv ;

   for( kk=edge ; kk < nztop ; kk++ ){
     for( jj=edge ; jj < nytop ; jj++ ){
       for( ii=edge ; ii < nxtop ; ii++ ){
         bb = B(ii,jj,kk) ; vv = V(ii-dx,jj-dy,kk-dz) ;
         bbsum += bb*bb ; vvsum += vv*vv ; bvsum += bb*vv ;
       }
     }
   }

   if( vvsum > 0.0 ) bbsum -= bvsum*bvsum/vvsum ;
   return bbsum ;
}

/*---------------------------------------------------------------------
  Do some shifts to find the best starting point for registration
  (globals VL_coarse_del and VL_coarse_num control operations).
-----------------------------------------------------------------------*/

float get_best_shift( int nx, int ny, int nz,
                      float *b, float *v ,
                      int *dxp , int *dyp , int *dzp )
{
   int bdx=0 , bdy=0 , bdz=0 , dx,dy,dz , nxyz=nx*ny*nz ;
   float bsum , sum ;

   int shift = VL_coarse_del, numsh = VL_coarse_num,
       shtop = shift*numsh  , edge  = shtop+shift  , sqtop = shtop*shtop ;

   bsum = 0.0 ;
   for( dx=0 ; dx < nxyz ; dx++ ) bsum += b[dx]*b[dx] ;

   for( dz=-shtop ; dz <= shtop ; dz+=shift ){
    for( dy=-shtop ; dy <= shtop ; dy+=shift ){
     for( dx=-shtop ; dx <= shtop ; dx+=shift ){
       if( dx*dx+dy*dy+dz*dz > sqtop ) continue ;
       sum = voldif( nx,ny,nz , b , dx,dy,dz , v , edge ) ;
       if( sum < bsum ){ bsum = sum; bdx = dx; bdy = dy; bdz = dz; }
   }}}

   *dxp = bdx ; *dyp = bdy ; *dzp = bdz ; return bsum ;
}

/*==========================================================================*/
/*=============== 01 Dec 2005: Newer versions of the above =================*/
/*==========================================================================*/

/*--------------------------------------------------------------------
  Calculate
              (      [                                 2          ] )
          min ( sum  [ {a v(i-dx,j-dy,k-dz) - b(i,j,k)}  w(i,j,k) ] )
           a  (  ijk [                                            ] )

  where the sum is taken over voxels at least 'edge' in from the edge.
  'edge' must be bigger than max(|dx|,|dy|,|dz|).  The weight w may
  be NULL, in which case it is taken to be identically 1.
----------------------------------------------------------------------*/

#define B(i,j,k) b[(i)+(j)*nx+(k)*nxy]
#define V(i,j,k) v[(i)+(j)*nx+(k)*nxy]
#define W(i,j,k) w[(i)+(j)*nx+(k)*nxy]

float new_voldif( int nx, int ny, int nz, float *b,
                  int dx, int dy, int dz, float *v, int edge , float *w )
{
   int nxy=nx*ny, nxtop=nx-edge, nytop=ny-edge, nztop=nz-edge , ii,jj,kk ;
   float bbsum=0.0f , vvsum=0.0f , bvsum=0.0f , bb,vv,ww ;

   if( w == NULL ){                         /** no weight given **/
     for( kk=edge ; kk < nztop ; kk++ ){
      for( jj=edge ; jj < nytop ; jj++ ){
       for( ii=edge ; ii < nxtop ; ii++ ){
         bb = B(ii,jj,kk) ; vv = V(ii-dx,jj-dy,kk-dz) ;
         bbsum += bb*bb ; vvsum += vv*vv ; bvsum += bb*vv ;
     }}}
   } else {                                /** use given weight **/
     for( kk=edge ; kk < nztop ; kk++ ){
      for( jj=edge ; jj < nytop ; jj++ ){
       for( ii=edge ; ii < nxtop ; ii++ ){
         ww = W(ii,jj,kk) ;
         if( ww > 0.0f ){
           bb = B(ii,jj,kk) ; vv = V(ii-dx,jj-dy,kk-dz) ;
           bbsum += ww*bb*bb ; vvsum += ww*vv*vv ; bvsum += ww*bb*vv ;
         }
     }}}
   }

   if( vvsum > 0.0f ) bbsum -= bvsum*bvsum/vvsum ;
   return bbsum ;
}

/*---------------------------------------------------------------------
  Do some shifts to find the best starting point for registration
  (globals VL_coarse_del and VL_coarse_num control operations).
-----------------------------------------------------------------------*/

float new_get_best_shift( int nx, int ny, int nz,
                          float *b, float *v , float *w ,
                          int *dxp , int *dyp , int *dzp )
{
   int bdx=0 , bdy=0 , bdz=0 , dx,dy,dz , nxyz=nx*ny*nz ;
   float bsum , sum ;

   int shift = VL_coarse_del, numsh = VL_coarse_num,
       shtop = shift*numsh  , edge  = shtop+shift  , sqtop = shtop*shtop ;

   bsum = 0.0 ;
   for( dx=0 ; dx < nxyz ; dx++ ) bsum += b[dx]*b[dx] ;

   for( dz=-shtop ; dz <= shtop ; dz+=shift ){
    for( dy=-shtop ; dy <= shtop ; dy+=shift ){
     for( dx=-shtop ; dx <= shtop ; dx+=shift ){
       if( dx*dx+dy*dy+dz*dz > sqtop ) continue ;
       sum = new_voldif( nx,ny,nz , b , dx,dy,dz , v , edge , w ) ;
       if( sum < bsum ){ bsum = sum; bdx = dx; bdy = dy; bdz = dz; }
   }}}

   *dxp = bdx ; *dyp = bdy ; *dzp = bdz ; return bsum ;
}

/*----------------------------------------------------------------------*/
/* Find best angles AND best shifts all at once't.
------------------------------------------------------------------------*/

#define DANGLE 9.0f
#define NROLL  1
#define NPITCH 2
#define NYAW   1

float new_get_best_shiftrot( THD_3dim_dataset *dset ,   /* template */
                             MRI_IMAGE *base , MRI_IMAGE *vol ,
                             float *roll , float *pitch , float *yaw ,
                             int   *dxp  , int   *dyp   , int   *dzp  )
{
   int ii,jj,kk ;
   float r,p,y , br=0.0f , bp=0.0f , by=0.0f ;
   float bsum=1.e+38 , sum ;
   MRI_IMAGE *tim , *wim=NULL , *bim, *vim ;
   float *bar , *tar , *var , dif , *www=NULL , wtop ;
   byte *mmm ;
   int nx,ny,nz , sx,sy,sz , bsx=0,bsy=0,bsz=0 , nxy,nxyz , subd=0 ;

   *roll = *pitch = *yaw = 0.0f ;   /* in case of sudden death */
   *dxp  = *dyp   = *dzp = 0    ;

   nx  = base->nx ; ny = base->ny ; nz = base->nz ; nxy = nx*ny ;

   /** if image volume is big enough, sub-sample by 2 for speedup **/

   bim = base ; vim = vol ;

   if( nx >= 120 && ny >= 120 && nz >= 120 ){
     int hnx=(nx+1)/2 , hny=(ny+1)/2 , hnz=(nz+1)/2 , hnxy=hnx*hny ;

     /* copy and blur base, then subsample it into new image bim */

     if( VL_verbose ) fprintf(stderr,"x") ;

     tim = mri_copy(base) ; tar = MRI_FLOAT_PTR(tim) ;
     FIR_blur_volume( nx,ny,nz , 1.0f,1.0f,1.0f , tar , 1.0f ) ;
     bim = mri_new_vol( hnx,hny,hnz , MRI_float ) ; bar = MRI_FLOAT_PTR(bim) ;
     for( kk=0 ; kk < hnz ; kk++ )    /* subsampling */
      for( jj=0 ; jj < hny ; jj++ )
       for( ii=0 ; ii < hnx ; ii++ )
         bar[ii+jj*hnx+kk*hnxy] = tar[2*(ii+jj*nx+kk*nxy)] ;
     mri_free(tim) ;

     /* copy and blur vol, then subsample it into a new image vim */

     tim = mri_copy(vol) ; tar = MRI_FLOAT_PTR(tim) ;
     FIR_blur_volume( nx,ny,nz , 1.0f,1.0f,1.0f , tar , 1.0f ) ;
     vim = mri_new_vol( hnx,hny,hnz , MRI_float ) ; var = MRI_FLOAT_PTR(vim) ;
     for( kk=0 ; kk < hnz ; kk++ )    /* subsampling */
      for( jj=0 ; jj < hny ; jj++ )
       for( ii=0 ; ii < hnx ; ii++ )
         var[ii+jj*hnx+kk*hnxy] = tar[2*(ii+jj*nx+kk*nxy)] ;
     mri_free(tim) ;

     /* adjust grid spacing in new images */

     bim->dx = vim->dx = 2.0f * base->dx ;
     bim->dy = vim->dy = 2.0f * base->dy ;
     bim->dz = vim->dz = 2.0f * base->dz ;

     nx = hnx; ny = hny; nz = hnz; nxy = hnxy; subd = 2;
     VL_coarse_del /= 2 ;
   }

   /* make a weighting image (blurred & masked copy of base) */

   if( VL_verbose ) fprintf(stderr,"w") ;

   wim = mri_copy(bim) ; www = MRI_FLOAT_PTR(wim) ; nxyz = nx*ny*nz ;
   for( ii=0 ; ii < nxyz ; ii++ ) www[ii] = fabsf(www[ii]) ;
   FIR_blur_volume( nx,ny,nz , 1.0f,1.0f,1.0f , www , 1.0f ) ;
   wtop = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ) wtop = MAX(wtop,www[ii]) ;
   wtop = 1.0f / wtop ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     www[ii] *= wtop ; if( www[ii] < 0.05 ) www[ii] = 0.0f ;
   }
   mmm = mri_automask_image( wim ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( mmm[ii] == 0 ) www[ii] = 0.0f ;
   if( VL_verbose )
     fprintf(stderr,"[%.1f%%]" , (100.0*THD_countmask(nxyz,mmm))/nxyz );
   free(mmm) ;

   /* prepare to rotate and shift the night away */

   THD_rota_method( MRI_NN ) ;
   bar = MRI_FLOAT_PTR(bim) ;

   for( kk=-NROLL  ; kk <= NROLL  ; kk++ ){
    for( jj=-NPITCH ; jj <= NPITCH ; jj++ ){
     for( ii=-NYAW   ; ii <= NYAW   ; ii++ ){
       r = kk*DANGLE ; p = jj*DANGLE ; y = ii*DANGLE ;

       if( r == 0.0f && p == 0.0f && y == 0.0f ){  /* no rotate */
         tim = vim ;
       } else {                                    /* rotate vim */
         char sbuf[128] ; THD_dvecmat vm ;
         sprintf(sbuf,"-rotate %.4fI %.4fR %.4fA" , r,p,y ) ;
         vm  = THD_rotcom_to_matvec( dset , sbuf ) ;
         tim = THD_rota3D_matvec( vim , vm.mm,vm.vv ) ;
       }
       tar = MRI_FLOAT_PTR(tim) ;
       sum = new_get_best_shift( nx,ny,nz, bar, tar, www, &sx,&sy,&sz ) ;
       if( VL_verbose ) fprintf(stderr,"%s", (sum<bsum)?"*":"." ) ;
       if( sum < bsum ){
         br=r ; bp=p ; by=y ; bsx=sx ; bsy=sy; bsz=sz ; bsum=sum ;
       }
       if( tim != vim ) mri_free(tim) ;
   }}}

   /* cleanup and exeunt */

   mri_free(wim) ;
   if( subd ){
     mri_free(bim); mri_free(vim);
     bsx *= 2; bsy *= 2; bsz *= 2; VL_coarse_del *=2;
   }

   *roll = br ; *pitch = bp ; *yaw = by ;
   *dxp  = bsx; *dyp   = bsy; *dzp = bsz ;

   return bsum ;
}

/*----------------------------------------------------------------------------*/

float ** VL_get_displacments( THD_3dim_dataset *dset, THD_dmat33 rmat, THD_dfvec3 tvec )
{
   int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny, nxyz=nxy*nz ;
   float xo=DSET_XCEN(dset), yo=DSET_YCEN(dset), zo=DSET_ZCEN(dset) ;
   THD_fvec3 fv , gv , cv ; THD_ivec3 iv ;
   THD_dmat33 pp,ppt ; THD_dfvec3 dv,qv,vorg ;
   int mm , ii , jj , kk ;
   float **dispar ;

   pp   = DBLE_mat_to_dicomm( dset ) ; /* convert rmat to dataset coord order */
   ppt  = TRANSPOSE_DMAT(pp);
   rmat = DMAT_MUL(ppt,rmat); rmat = DMAT_MUL(rmat,pp); tvec = DMATVEC(ppt,tvec);
   LOAD_DFVEC3(vorg,xo,yo,zo) ;          /* rotation is around dataset center */

   dispar    = (float **)malloc(sizeof(float *)*3) ;
   dispar[0] = (float * )calloc(sizeof(float),nxyz) ;
   dispar[1] = (float * )calloc(sizeof(float),nxyz) ;
   dispar[2] = (float * )calloc(sizeof(float),nxyz) ;

   for( mm=0 ; mm < nxyz ; mm++ ){
     ii = mm % nx ; kk = mm / nxy ; jj = (mm%nxy) / nx ;
     iv.ijk[0] = ii ; iv.ijk[1] = jj ; iv.ijk[2] = kk ;
     fv = THD_3dind_to_3dmm_no_wod( VL_dset , iv ) ;
     FVEC3_TO_DFVEC3( fv , dv );
     qv = SUB_DFVEC3(dv,vorg); qv = DMATVEC_ADD(rmat,qv,tvec); qv = ADD_DFVEC3(qv,vorg);
     qv = SUB_DFVEC3(dv,qv);

     dispar[0][mm] = qv.xyz[0] ; dispar[1][mm] = qv.xyz[1] ; dispar[2][mm] = qv.xyz[2] ;
   }

   return dispar ;
}

/*----------------------------------------------------------------------------*/

void VL_normalize_timeseries( THD_3dim_dataset *dset )
{
   int nvox=DSET_NVOX(dset) , nvals=DSET_NVALS(dset) , ii , jj ;
   float *tar=(float *)malloc(sizeof(float)*nvals) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     THD_extract_float_array( ii , dset , tar ) ;
     THD_linear_detrend( nvals , tar , NULL,NULL ) ;
     THD_normmax( nvals , tar ) ;
     THD_insert_series( ii , dset , nvals , MRI_float , tar , 0 ) ;
   }
   free(tar) ; return ;
}

/*----------------------------------------------------------------------------*/

#undef  Pleg2
#undef  Pleg3
#define Pleg2(x) (1.5f*(x)*(x)-0.5f)
#define Pleg3(x) ((2.5f*(x)*(x)-1.5f)*(x))

#undef  Pleg
#define Pleg(n,x) ( ((n)==0) ? 1 : ((n)==1) ? (x) : ((n)==2) ? Pleg2(x) : Pleg3(x) )

void * VL_create_disprod( THD_3dim_dataset *tdset ,
                          THD_3dim_dataset *xdset , int xp ,
                          THD_3dim_dataset *ydset , int yp ,
                          THD_3dim_dataset *zdset , int zp  )
{
   char *pr = malloc(sizeof(char)*THD_MAX_NAME) ;
   char lab[8] ,
       *xlab[4] = { "\0" , "X" , "XX" , "XXX" } ,
       *ylab[4] = { "\0" , "Y" , "YY" , "YYY" } ,
       *zlab[4] = { "\0" , "Z" , "ZZ" , "ZZZ" }  ;
   THD_3dim_dataset *ddset ;
   int nvox=DSET_NVOX(tdset) , nvals=DSET_NVALS(tdset) , ii,iv ;
   float *xd , *yd , *zd , *dd ;

   ddset = EDIT_empty_copy(tdset) ;
   tross_Copy_History(tdset,ddset) ;
   strcpy(lab,"_D") ; strcat(lab,xlab[xp]) ; strcat(lab,ylab[yp]) ; strcat(lab,zlab[zp]) ;
   SDAPP(lab) ;
   EDIT_dset_items(ddset,ADN_prefix,pr,ADN_none) ;
   sprintf(pr,"-- %s savedisp output",lab) ; tross_Append_History(ddset,pr) ;

   for( iv=0 ; iv < nvals ; iv++ ){
     EDIT_substitute_brick( ddset , iv , MRI_float , NULL ) ;
     dd = DSET_BRICK_ARRAY(ddset,iv) ;
     xd = DSET_BRICK_ARRAY(xdset,iv) ;
     yd = DSET_BRICK_ARRAY(ydset,iv) ;
     zd = DSET_BRICK_ARRAY(zdset,iv) ;
     for( ii=0 ; ii < nvox ; ii++ ){
       dd[ii] = Pleg(xp,xd[ii]) * Pleg(yp,yd[ii]) * Pleg(zp,zd[ii]) ;
     }
   }
   VL_normalize_timeseries(ddset) ;
   DSET_write(ddset) ;
   if( VL_verbose ) INFO_message("Wrote dataset to disk in %s",DSET_BRIKNAME(ddset)) ;
   DSET_delete(ddset) ; free(pr) ;
}
