/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <string.h>

/******* global data *******/

/** define results of scanning the command line **/

static int     Iarg = 1 ;
static int     Argc ;
static char ** Argv ;

static int         VL_nbase  = 0 ;
static int         VL_intern = 1 ;
static int         VL_resam  = MRI_FOURIER ;
static int         VL_final  = -1 ;   /* 20 Nov 1998 */
static int         VL_clipit = 0 ;    /* 23 Oct 1998 */
static MRI_IMAGE * VL_imbase = NULL ;
static MRI_IMAGE * VL_imwt   = NULL ;

static int         VL_twopass= 0 ;    /* 11 Sep 2000 */
static float       VL_twoblur= 2.0 ;
static int         VL_twosum = 1 ;    /* 08 Dec 2000 */
static int         VL_twodup = 0 ;
static float       VL_bxorg, VL_byorg, VL_bzorg ;

static float       VL_edging ;        /* 10 Dec 2000 */
static int         VL_edperc=-1 ;

static int         VL_coarse_del=10 ; /* 11 Dec 2000 */
static int         VL_coarse_num=2  ;

static THD_3dim_dataset * VL_dset = NULL ;

static char VL_prefix[256] = "volreg" ;
static int  VL_verbose     = 0 ;
static char VL_dfile[256]  = "\0" ;
static char VL_1Dfile[256] = "\0" ;  /* 14 Apr 2000 */

static int VL_maxite = 9 ;
static float VL_dxy  = 0.05 ;  /* voxels */
static float VL_dph  = 0.07 ;  /* degrees */
static float VL_del  = 0.70 ;  /* voxels */

static int VL_rotcom = 0 ;     /* 04 Sep 2000: print out 3drotate commands? */

/******* prototypes *******/

void VL_syntax(void) ;
void VL_command_line(void) ;

float voldif( int nx, int ny, int nz, float *b,
              int dx, int dy, int dz, float *v, int edge ) ;

void get_best_shift( int nx, int ny, int nz,
                     float *b, float *v , int *dxp , int *dyp , int *dzp ) ;

/*--------------------------------------------------------------------*/

/* These 2 routines criminally lifted from 3drotate.c
   -- but I wrote those programs too, so what's your point? */

#include <ctype.h>

int axcode( THD_3dim_dataset * dset , char ori )
{
   ori = toupper(ori) ;
   if( ori == ORIENT_tinystr[dset->daxes->xxorient][0] ) return  1 ;
   if( ori == ORIENT_tinystr[dset->daxes->xxorient][1] ) return -1 ;
   if( ori == ORIENT_tinystr[dset->daxes->yyorient][0] ) return  2 ;
   if( ori == ORIENT_tinystr[dset->daxes->yyorient][1] ) return -2 ;
   if( ori == ORIENT_tinystr[dset->daxes->zzorient][0] ) return  3 ;
   if( ori == ORIENT_tinystr[dset->daxes->zzorient][1] ) return -3 ;
   return -99 ;
}

/*--------------------------------------------------------------------*/

int handedness( THD_3dim_dataset * dset )
{
   THD_dataxes * dax = dset->daxes ;
   THD_mat33 q ;
   int col ;
   float val ;

   LOAD_ZERO_MAT(q) ;

   col = 0 ;
   switch( dax->xxorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   col = 1 ;
   switch( dax->yyorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   col = 2 ;
   switch( dax->zzorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   val = MAT_DET(q) ;
   if( val > 0.0 ) return  1 ;   /* right handed */
   else            return -1 ;   /* left handed */
}

/**********************************************************************/
/***************************** the program! ***************************/

int main( int argc , char *argv[] )
{
   MRI_3dalign_basis * albase ;
   THD_3dim_dataset * new_dset ;
   MRI_IMAGE * qim , * tim , * fim ;
   double cputim ;
   float *dx, *dy, *dz, *roll, *yaw, *pitch, *rmsnew, *rmsold, *imb, *tar ;
   float ddx,ddy,ddz , sum ;
   float dxtop,dytop,dztop , rolltop,yawtop,pitchtop ;
   float dxbot,dybot,dzbot , rollbot,yawbot,pitchbot ;
   float dxbar,dybar,dzbar , rollbar,yawbar,pitchbar ;
   int kim,ii , imcount , iha , ax1,ax2,ax3 , hax1,hax2,hax3 ;

   float *dx_1,*dy_1,*dz_1, *roll_1,*yaw_1,*pitch_1 ;  /* 11 Sep 2000 */
   int   nx,ny,nz ;

   static char * modes[] = {
        "-NN" , "-linear" , "-cubic" , "-Fourier" , "-quintic" , "-heptic" } ;

   /*-- handle command line options --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){ VL_syntax() ; exit(0); }

   Argc = argc ; Argv = argv ; Iarg = 1 ;
   VL_command_line() ;

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

   iha = handedness( VL_dset )   ;                     /* LH or RH? */
   ax1 = axcode( VL_dset , 'I' ) ; hax1 = ax1 * iha ;  /* roll */
   ax2 = axcode( VL_dset , 'R' ) ; hax2 = ax2 * iha ;  /* pitch */
   ax3 = axcode( VL_dset , 'A' ) ; hax3 = ax3 * iha ;  /* yaw */

   /*-- create the output dataset --*/

   new_dset = EDIT_empty_copy( VL_dset ) ;
   EDIT_dset_items( new_dset , ADN_prefix , VL_prefix , ADN_none ) ;
   if( THD_is_file( DSET_HEADNAME(new_dset) ) ){
      fprintf(stderr,
              "*** Output file %s already exists -- cannot continue!\n",
              DSET_HEADNAME(new_dset) ) ;
      exit(1) ;
   }

   tross_Copy_History( VL_dset , new_dset ) ;
   tross_Make_History( "3dvolreg" , argc,argv , new_dset ) ;

   /*-- read the input dataset into memory --*/

   if( VL_verbose )
      fprintf(stderr,"++ Reading input dataset %s\n",DSET_BRIKNAME(VL_dset)) ;
   DSET_load( VL_dset ) ;

   /*-- initialize the registration algorithm --*/

   if( VL_imbase == NULL ){
      VL_imbase = mri_to_float(DSET_BRICK(VL_dset,VL_nbase)) ; /* copy this */
   } else {
      VL_nbase = -1 ;  /* will not match any sub-brick index */
   }

   VL_imbase->dx = fabs( DSET_DX(VL_dset) ) ;  /* must set the voxel dimensions */
   VL_imbase->dy = fabs( DSET_DY(VL_dset) ) ;
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

   /*--- 11 Sep 2000: if in twopass mode, do the first pass ---*/

   if( VL_twopass ){
      MRI_IMAGE * tp_base ;
      int sx=66666,sy,sz ;

      if( VL_verbose ){
         fprintf(stderr,"++ Start of first pass alignment on all sub-bricks\n") ;
         cputim = COX_cpu_time();
      }

      tp_base = mri_to_float(VL_imbase) ;  /* make a copy, blur it */

      EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 ,
                           MRI_float , MRI_FLOAT_PTR(tp_base) ,
                           VL_twoblur,VL_twoblur,VL_twoblur ) ;

      mri_3dalign_params( VL_maxite ,
                          VL_twoblur*VL_dxy, VL_twoblur*VL_dph, VL_twoblur*VL_del,
                          abs(ax1)-1 , abs(ax2)-1 , abs(ax3)-1 , -1 ) ;

                                              /* no reg | */
                                              /*        v */

      mri_3dalign_method( MRI_LINEAR , (VL_verbose>1) , 1 , 0 ) ;

      /* 08 Dec 2000: (perhaps) compute the weight as the blurred
                      average of the base and the 1st data brick  */

      if( VL_imwt != NULL || !VL_twosum || VL_imbase == DSET_BRICK(VL_dset,0) ){

         albase = mri_3dalign_setup( tp_base , VL_imwt ) ;

      } else {

         float *far , *bar=MRI_FLOAT_PTR(tp_base) , *qar ;
         int ii,jj,kk , nxy=nx*ny , nxyz=nxy*nz ;
         int nxbot,nxtop,nybot,nytop,nzbot,nztop , ee,fade,ff ;

         if( VL_verbose )
           fprintf(stderr,"++ Computing first pass weight as sum of base and brick\n") ;

         fim = mri_to_float( DSET_BRICK(VL_dset,0) ) ; /* 1st data brick */
         far = MRI_FLOAT_PTR(fim) ;
         EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 , /* blur it */
                              MRI_float , far ,
                              VL_twoblur,VL_twoblur,VL_twoblur ) ;

         /* find shift of 1st data brick that best overlaps with base brick */

         if( VL_coarse_del > 0 && VL_coarse_num > 0 ){
            if( VL_verbose ) fprintf(stderr,"++ Getting best coarse shift:") ;
            get_best_shift( nx,ny,nz , bar,far , &sx,&sy,&sz ) ;
            if( VL_verbose ) fprintf(stderr," %d %d %d\n",sx,sy,sz) ;
         } else {
            sx = sy = sz = 0 ;
         }

#define BAR(i,j,k) bar[(i)+(j)*nx+(k)*nxy]
#define QAR(i,j,k) qar[(i)+(j)*nx+(k)*nxy]
#define FAR(i,j,k) far[(i)+(j)*nx+(k)*nxy]

         /* add the blurred+shifted data brick to the blurred base brick */

         qim = mri_copy(tp_base) ; qar = MRI_FLOAT_PTR(qim) ;

         ee = abs(sx) ; nxbot = ee ; nxtop = nx-ee ;
         ee = abs(sy) ; nybot = ee ; nytop = ny-ee ;
         ee = abs(sz) ; nzbot = ee ; nztop = nz-ee ;

         for( kk=nztop ; kk < nztop ; kk++ )
            for( jj=nybot ; jj < nytop ; jj++ )
               for( ii=nxbot ; ii < nxtop ; ii++ )
                  QAR(ii,jj,kk) += FAR(ii-sx,jj-sy,kk-sz) ;

         mri_free(fim) ;

         /* blur the sum to get the weight brick */

         if( VL_verbose )
           fprintf(stderr,"++ Blurring first pass weight\n") ;

         EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 ,
                              MRI_float , qar ,
                              VL_twoblur,VL_twoblur,VL_twoblur ) ;

         mri_3dalign_force_edging( 1 ) ;
         albase = mri_3dalign_setup( tp_base , qim ) ;
         mri_3dalign_force_edging( 0 ) ;
         mri_free(qim) ;
      }

      /* check if base was computed correctly */

      if( albase == NULL ){
         fprintf(stderr,
                 "*** Can't initialize first pass alignment algorithm\n");
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
         fim     = mri_to_float( qim ) ;         /* make a float copy */
         fim->dx = fabs( DSET_DX(VL_dset) ) ;    /* must set voxel dimensions */
         fim->dy = fabs( DSET_DY(VL_dset) ) ;
         fim->dz = fabs( DSET_DZ(VL_dset) ) ;

         if( kim != VL_nbase ){ /* 16 Nov 1998: don't register to base image */

             EDIT_blur_volume_3d( nx,ny,nz , 1.0,1.0,1.0 ,
                                  MRI_float , MRI_FLOAT_PTR(fim) ,
                                  VL_twoblur,VL_twoblur,VL_twoblur ) ;

            if( kim > 0 || sx == 66666 ){  /* if didn't already get best shift */
               if( VL_coarse_del > 0 && VL_coarse_num > 0 ){
                  if( VL_verbose )
                     fprintf(stderr,"++ Getting best coarse shift:") ;
                  get_best_shift( nx,ny,nz , MRI_FLOAT_PTR(tp_base),MRI_FLOAT_PTR(fim) ,
                                  &sx,&sy,&sz ) ;
                  if( VL_verbose )
                     fprintf(stderr," %d %d %d\n",sx,sy,sz) ;
               } else {
                  sx = sy = sz = 0 ;
               }
            }

            mri_3dalign_initvals( 0.0 , 0.0 , 0.0 ,
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

   mri_3dalign_method( VL_resam , (VL_verbose>1) , 0 , VL_clipit ) ;

   if( VL_final < 0 ) VL_final = VL_resam ;  /* 20 Nov 1998 */
   mri_3dalign_final_regmode( VL_final ) ;

   albase = mri_3dalign_setup( VL_imbase , VL_imwt ) ;
   if( albase == NULL ){
      fprintf(stderr,"*** Can't initialize base image for alignment\n"); exit(1);
   }
   if( VL_imwt != NULL ) mri_free( VL_imwt ) ;

   /*-- loop over sub-bricks and register them --*/

   if( VL_verbose ){
      fprintf(stderr,"++ Starting final pass on %d sub-bricks: ",imcount);
      cputim = COX_cpu_time();
   }

   dxbar = dybar = dzbar = rollbar = yawbar = pitchbar = 0.0 ;

   for( kim=0 ; kim < imcount ; kim++ ){

      if( VL_verbose ) fprintf(stderr,"%d",kim) ;  /* mark start of this one */

      qim     = DSET_BRICK( VL_dset , kim ) ; /* the sub-brick in question */
      fim     = mri_to_float( qim ) ;         /* make a float copy */
      fim->dx = fabs( DSET_DX(VL_dset) ) ;    /* must set voxel dimensions */
      fim->dy = fabs( DSET_DY(VL_dset) ) ;
      fim->dz = fabs( DSET_DZ(VL_dset) ) ;

      /*-- the actual registration [please bow your head] --*/

      if( kim != VL_nbase ){ /* 16 Nov 1998: don't register to base image */

         if( VL_twopass )
            mri_3dalign_initvals( roll_1[kim] , pitch_1[kim] , yaw_1[kim] ,
                                  dx_1[kim]   , dy_1[kim]    , dz_1[kim]   ) ;

         tim = mri_3dalign_one( albase , fim ,
                                roll+kim , pitch+kim , yaw+kim ,
                                &ddx     , &ddy      , &ddz     ) ;

      } else {               /* 16 Nov 1998: just make a copy of base image */

         tim = mri_to_float( VL_imbase ) ;
         roll[kim] = pitch[kim] = yaw[kim] = ddx = ddy = ddz = 0.0 ;

      }

      if( VL_verbose ) fprintf(stderr,".") ;  /* mark that registration is done */

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

      sum = 0.0 ;
      tar = MRI_FLOAT_PTR(tim) ;
      for( ii=0 ; ii < tim->nvox ; ii++ ) sum += SQR( imb[ii] - tar[ii] ) ;
      rmsnew[kim] = sqrt( sum / tim->nvox ) ;

      sum = 0.0 ;
      tar = MRI_FLOAT_PTR(fim) ;
      for( ii=0 ; ii < fim->nvox ; ii++ ) sum += SQR( imb[ii] - tar[ii] ) ;
      rmsold[kim] = sqrt( sum / fim->nvox ) ;

      mri_free(fim) ;  /* only needed this to compute rmsold */

      /*-- Attach the registered brick to output dataset,
           converting it to the correct type, if necessary
           (the new registered brick in "tim" is stored as floats). --*/

      switch( qim->kind ){

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

      DSET_unload_one( VL_dset , kim ) ;      /* don't need this anymore */

      if( VL_verbose ) fprintf(stderr,".") ;  /* mark end of this one */

   }  /* end of loop over sub-bricks */

   /*-- done with registration --*/

   mri_3dalign_cleanup( albase ) ;
   DSET_delete( VL_dset ) ;
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

      fprintf(stderr,"++ Writing dataset to disk in %s",DSET_HEADNAME(new_dset) ) ;
   }

   /*-- 12 Sep 2000: add some history? --*/

   if( imcount == 1 ){
      char * str = NULL ;
      str = THD_zzprintf( str , "3dvolreg did: %s" , modes[VL_final] ) ;
      if( VL_clipit ) str = THD_zzprintf( str ," -clipit" ) ;
      str = THD_zzprintf(str,
                      " -rotate %.3fI %.3fR %.3fA -ashift %.3fS %.3fL %.3fP\n" ,
                      roll[0],pitch[0],yaw[0], dx[0],dy[0],dz[0]  ) ;
      tross_Append_History( new_dset , str ) ;
      free(str) ;
   }

   /*-- 08 Dec 2000: execute -twodup? --*/

   if( VL_twodup ){
      new_dset->daxes->xxorg = VL_bxorg ;
      new_dset->daxes->yyorg = VL_byorg ;
      new_dset->daxes->zzorg = VL_bzorg ;
   }

   /*-- save new dataset to disk --*/

   DSET_write(new_dset) ;
   if( VL_verbose ) fprintf(stderr,".\n") ;

   /*-- save movement parameters to disk --*/

   if( VL_dfile[0] != '\0' ){
      FILE * fp ;

      if( THD_is_file(VL_dfile) )
         fprintf(stderr,"** Warning: overwriting file %s\n",VL_dfile) ;

      fp = fopen( VL_dfile , "w" ) ;
      for( kim=0 ; kim < imcount ; kim++ )
         fprintf(fp , "%4d %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f  %11.4g %11.4g\n" ,
                 kim , roll[kim], pitch[kim], yaw[kim],
                       dx[kim], dy[kim], dz[kim],
                       rmsold[kim] , rmsnew[kim]  ) ;
      fclose(fp) ;
   }

   if( VL_1Dfile[0] != '\0' ){  /* 14 Apr 2000 */
      FILE * fp ;

      if( THD_is_file(VL_1Dfile) )
         fprintf(stderr,"** Warning: overwriting file %s\n",VL_1Dfile) ;

      fp = fopen( VL_1Dfile , "w" ) ;
      for( kim=0 ; kim < imcount ; kim++ )
         fprintf(fp , "%7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n" ,
                 roll[kim], pitch[kim], yaw[kim],
                 dx[kim]  , dy[kim]   , dz[kim]  ) ;
      fclose(fp) ;
   }

   if( VL_rotcom ){ /* 04 Sep 2000 */

      printf("\n3drotate fragment%s:\n\n", (imcount > 1)? "s" : "" ) ;
      for( kim=0 ; kim < imcount ; kim++ ){
         printf("3drotate %s" , modes[VL_final] ) ;
         if( VL_clipit ) printf(" -clipit" ) ;
         printf(" -rotate %.3fI %.3fR %.3fA -ashift %.3fS %.3fL %.3fP\n" ,
                 roll[kim],pitch[kim],yaw[kim], dx[kim],dy[kim],dz[kim]  ) ;
      }
      printf("\n") ;  /* 11 Dec 2000 */
   }

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
    "OPTIONS:\n"
    "  -verbose        Print progress reports.  Use twice for LOTS of output.\n"
    "  -Fourier        Perform the alignments using Fourier interpolation.\n"
    "  -heptic         Use heptic polynomial interpolation.\n"
    "  -quintic        Use quintic polynomical interpolation.\n"
    "  -cubic          Use cubic polynomial interpolation.\n"
    "                    Default = Fourier [slowest and most accurate interpolator]\n"
    "  -clipit         Clips the values in each output sub-brick to be in the same\n"
    "                    range as the corresponding input volume.\n"
    "                    The interpolation schemes can produce values outside\n"
    "                    the input range, which is sometimes annoying.\n"
    "  -prefix fname   Use 'fname' for the output dataset prefix.\n"
    "                    The program tries not to overwrite an existing dataset.\n"
    "                    Default = 'volreg'.\n"
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
    "              -rotate <roll>I <pitch>R <yaw>A  -ashift <dS>S <dL>L <dP>P\n"
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
    "  -rotcom         Write the fragmentary 3drotate commands needed to\n"
    "                  perform the realignments to stdout; for example:\n"
    "                    3drotate -rotate 7.2I 3.2R -5.7A -ashift 2.7S -3.8L 4.9P\n"
    "                  The purpose of this is to make it easier to shift other\n"
    "                  datasets using exactly the same parameters.\n"
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
    "                                  strings 'cubic', 'quintic', 'heptic', or\n"
    "                                  'Fourier'\n"
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
    "                                  want to use '-heptic -clipit' rather than\n"
    "                                  the default '-Fourier', since you can get\n"
    "                                  wraparound from Fourier interpolation.\n"
    "                      -twodup = If this option is set, along with -twopass,\n"
    "                                  then the output dataset will have its\n"
    "                                  xyz-axes origins reset to those of the\n"
    "                                  base dataset.  This is equivalent to using\n"
    "                                  '3drefit -duporigin' on the output dataset.\n"
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
    "       * The -twopass option can be slow - several CPU minutes for a\n"
    "          256x256x124 volume is a typical run time.\n"
    "       * After registering high-resolution anatomicals, you may need to\n"
    "          set their origins in 3D space to match.  This can be done using\n"
    "          the '-duporigin' option to program 3drefit, or by using the\n"
    "          '-twodup' option to this program.\n"

   , VL_maxite , VL_dxy , VL_dph , VL_del ) ;

   return ;
}

/*---------------------------------------------------------------------*/

void VL_command_line(void)
{
   int ii , nxbase , nybase , nerr , basecode ;
   MRI_IMAGE * tim ;
   MRI_IMARR * tarr ;
   float bdx,bdy,bdz ;

   /*** look for options ***/

   while( Iarg < Argc && Argv[Iarg][0] == '-' ){

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
         else {
            fprintf(stderr,"*** Illegal mode after -final\n"); exit(1);
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

      if( strncmp(Argv[Iarg],"-verbose",4) == 0 ){
         VL_verbose++ ;
         Iarg++ ; continue ;
      }

      /** -clipit **/

      if( strncmp(Argv[Iarg],"-clipit",4) == 0 ){
         VL_clipit++ ;
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
           fprintf(stderr,"*** Can't have two -base arguments\n") ; exit(1) ;
        }

        /* try an integer */

        bb = strtol( Argv[++Iarg] , &cpt , 10 ) ;
        if( bb < 0 ){
          fprintf(stderr,"*** Illegal number after -base\n"); exit(1);
        }

        if( *cpt == '\0' ){  /* it WAS an integer */

          VL_nbase  = bb ;
          VL_imbase = NULL ;
          VL_intern = 1 ;

        } else {             /* it WAS NOT an integer */

          THD_3dim_dataset * bset ;   /* 13 Sep 2000: replaced old code */
                                      /* with use of THD_open_dataset() */

          bset = THD_open_dataset( Argv[Iarg] ) ;
          if( bset == NULL ){
             fprintf(stderr,"*** Couldn't open -base dataset %s\n",Argv[Iarg]) ;
             exit(1) ;
          }
          if( VL_verbose )
             fprintf(stderr,"++ Reading in base dataset %s\n",DSET_BRIKNAME(bset)) ;
          DSET_load(bset) ;
          if( !DSET_LOADED(bset) ){
             fprintf(stderr,"*** Couldn't read -base dataset %s\n",
                     DSET_BRIKNAME(bset)) ;
             exit(1) ;
          }
          if( DSET_NVALS(bset) > 1 )
             fprintf(stderr,
                     "+++ WARNING: -base dataset %s has more than 1 sub-brick\n",
                     Argv[Iarg]) ;

          VL_intern = 0 ;   /* not internal to input dataset */

          bdx = fabs(DSET_DX(bset)) ;  /* save for comparison later */
          bdy = fabs(DSET_DY(bset)) ;  /* (14 Sep 2000)            */
          bdz = fabs(DSET_DZ(bset)) ;
          VL_imbase = mri_to_float( DSET_BRICK(bset,bb) ) ;  /* copy this */

          VL_bxorg = bset->daxes->xxorg ;                    /* 08 Dec 2000 */
          VL_byorg = bset->daxes->yyorg ;
          VL_bzorg = bset->daxes->zzorg ;

          DSET_delete( bset ) ;                              /* toss this */
        }
        Iarg++ ; continue ;
      }

      /** 11 Dec 2000: -coarse **/

      if( strcmp(Argv[Iarg],"-coarse") == 0 ){
         VL_coarse_del = strtol(Argv[++Iarg],NULL,10) ;
         VL_coarse_num = strtol(Argv[++Iarg],NULL,10) ;
         Iarg++ ; continue ;
      }

      /** 10 Dec 2000: -edging **/

      if( strcmp(Argv[Iarg],"-edging") == 0 ){
        float ee ; char * eq ;
        ee = strtod(Argv[++Iarg],&eq) ;
        if( ee < 0 ){
           fprintf(stderr,"*** Illegal value after -edging\n"); exit(1);
        }
        if( *eq == '%' ){
           if( ee > 25.0 ){
              fprintf(stderr,"*** Illegal percentage after -edging\n"); exit(1);
           }
           VL_edperc = 1 ; VL_edging = ee ;
        } else {
           VL_edperc = 0 ; VL_edging = ee ;
        }
        Iarg++ ; continue ;
      }

      /** -weight **/

      if( strncmp(Argv[Iarg],"-weight",4) == 0 ){
        int bb,ii ; char * cpt ;
        THD_3dim_dataset * wset ;
        char dname[256] ;

        if( VL_imwt != NULL ){
           fprintf(stderr,"*** Can't have two -weight options\n") ; exit(1) ;
        }

        /* break it into 'wset[bb]' pieces */

        cpt = strstr( Argv[++Iarg] , "[" ) ;
        if( cpt == NULL || cpt == Argv[Iarg] ){
           fprintf(stderr,"*** Illegal weight dataset after -weight\n"); exit(1);
        }
        ii = cpt - Argv[Iarg] ;
        memcpy(dname,Argv[Iarg],ii) ; dname[ii] = '\0' ;
        bb = -1 ; sscanf( cpt+1 , "%d" , &bb ) ;
        if( bb < 0 ){
           fprintf(stderr,"*** Illegal sub-brick selector after -weight\n"); exit(1);
        }
        wset = THD_open_one_dataset( dname ) ;
        if( wset == NULL ){
           fprintf(stderr,"*** Can't open weight dataset %s\n",dname); exit(1);
        }
        if( bb >= DSET_NVALS(wset) ){
           fprintf(stderr,"*** Illegal sub-brick selector for dataset %s\n",dname);
           exit(1) ;
        }
        if( VL_verbose )
           fprintf(stderr,"++ Reading in weight dataset %s\n",DSET_BRIKNAME(wset)) ;
        DSET_load(wset) ;
        VL_imwt = mri_to_float( DSET_BRICK(wset,bb) ) ;  /* copy this */
        DSET_delete( wset ) ;                            /* toss this */
        Iarg++ ; continue ;
      }

      /** 08 Dec 2000: -twodup **/

      if( strcmp(Argv[Iarg],"-twodup") == 0 ){
        VL_twodup++ ; Iarg++ ; continue ;
      }

      /** get to here is bad news **/

      fprintf(stderr,"** Unknown option: %s\a\n",Argv[Iarg]) ; exit(1) ;
   }

   /*** 08 Dec 2000: check some twopass options ***/

   if( VL_twodup ){
      if( !VL_twopass || VL_intern ) VL_twodup == 0 ;
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
           fprintf(stderr,"*** Illegal value in AFNI_VOLREG_EDGING\n"); exit(1);
        }
        if( *eq == '%' ){
           if( ee > 25.0 ){
              fprintf(stderr,"*** Illegal percentage in AFNI_VOLREG_EDGING\n"); exit(1);
           }
           VL_edperc = 1 ; VL_edging = ee ;
        } else {
           VL_edperc = 0 ; VL_edging = ee ;
        }
      }
   }

   /*** Open the dataset to be registered ***/

   if( Iarg > Argc ){
      fprintf(stderr,"*** Too few arguments!?\n") ; exit(1) ;
   } else if( Iarg < Argc-1 ){
      fprintf(stderr,"*** Too many arguments?!\n") ; exit(1) ;
   }

   VL_dset = THD_open_dataset( Argv[Iarg] ) ;

   /** Check for errors **/

   if( VL_dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",Argv[Iarg]) ; exit(1) ;
   }

   if( VL_imbase == NULL && VL_nbase >= DSET_NVALS(VL_dset) ){
      fprintf(stderr,"*** Dataset %s doesn't have base brick index = %d\n",
              Argv[Iarg] , VL_nbase ) ;
      exit(1) ;
   }

   if( VL_imbase != NULL && ( VL_imbase->nx != DSET_NX(VL_dset) ||
                              VL_imbase->ny != DSET_NY(VL_dset) ||
                              VL_imbase->nz != DSET_NZ(VL_dset)   ) ){

      fprintf(stderr,"*** Dataset %s doesn't conform to dimensions of base brick\n",
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

     fprintf(stderr,"** WARNING:\n"
                    "** Dataset %s and base have different grid spacings:\n"
                    "** Dataset: dx=%9.3f  dy=%9.3f  dz=%9.3f\n"
                    "**    Base: dx=%9.3f  dy=%9.3f  dz=%9.3f\n" ,
             Argv[Iarg] ,
             fabs(DSET_DX(VL_dset)),fabs(DSET_DY(VL_dset)),fabs(DSET_DZ(VL_dset)),
             bdx,bdy,bdz ) ;
   }

   if( VL_imwt != NULL && ( VL_imwt->nx != DSET_NX(VL_dset) ||
                            VL_imwt->ny != DSET_NY(VL_dset) ||
                            VL_imwt->nz != DSET_NZ(VL_dset)   ) ){

      fprintf(stderr,"*** Dataset %s doesn't conform to dimensions of weight brick\n",
               Argv[Iarg] ) ;
      fprintf(stderr,"    Weight  has nx = %d  ny = %d  nz = %d\n",
                     VL_imwt->nx, VL_imwt->ny, VL_imwt->nz ) ;
      fprintf(stderr,"    Dataset has nx = %d  ny = %d  nz = %d\n",
                     DSET_NX(VL_dset) , DSET_NY(VL_dset) , DSET_NZ(VL_dset) ) ;
      exit(1) ;
   }

   if( VL_intern && DSET_NVALS(VL_dset) == 1 ){
      fprintf(stderr,"*** You can't register a 1 brick dataset to itself!\n") ;
      exit(1) ;
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

void get_best_shift( int nx, int ny, int nz,
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
            if( sum < bsum ){
#if 0
fprintf(stderr,"  get_best_shift: bsum=%g dx=%d dy=%d dz=%d\n",sum,dx,dy,dz) ;
#endif
               bsum = sum ; bdx = dx ; bdy = dy ; bdz = dz ;
            }
         }
      }
   }

   *dxp = bdx ; *dyp = bdy ; *dzp = bdz ; return ;
}
