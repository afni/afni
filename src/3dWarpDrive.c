/*------------------------------------------------------------------------
   ***  This program does something, but nobody is quite sure what.  ***
  ***  But what it does is very important; nobody doubts that, either. ***
--------------------------------------------------------------------------*/

#include "mrilib.h"

/*--------------------------------------------------------------------------*/
/* 02 Dec 2005: prototype for quick initializer for shift+rotate parameters */

static float get_best_shiftrot( MRI_warp3D_align_basis *bas ,
                                MRI_IMAGE *base , MRI_IMAGE *vol ,
                                float *roll , float *pitch , float *yaw ,
                                int   *dxp  , int   *dyp   , int   *dzp  ) ;

static int VL_coarse_del = 12 ;  /* variables for the above, */
static int VL_coarse_num =  3 ;  /* copied from 3dvolreg.c   */
static int VL_coarse_rot =  0 ;
/*--------------------------------------------------------------------------*/

#define MAXPAR 99

typedef struct { int np ; float vp ; } fixed_param ;

static int nparfix ;
static fixed_param parfix[MAXPAR] ;

static float parvec[MAXPAR] ;

static void (*warp_parset)(void) = NULL ;

void load_parvec( int np, float *pv ){
  memcpy( parvec , pv , sizeof(float)*np ) ;
  if( warp_parset != NULL ) warp_parset() ;
}

/*--------------------------------------------------------------------------*/

static void (*warp_for)( float,float,float , float *,float *,float *) ;
static void (*warp_inv)( float,float,float , float *,float *,float *) ;

static THD_vecmat ijk_to_xyz , xyz_to_ijk ;

/*--------------------------------------------------------------------------*/

void ijk_warp_for( float  ii , float  jj , float  kk ,
                   float *pp , float *qq , float *rr  )
{
   THD_fvec3 xxx , yyy ;

   LOAD_FVEC3( xxx , ii,jj,kk ) ;
   yyy = VECMAT_VEC( ijk_to_xyz , xxx ) ;
   warp_for(  yyy.xyz[0] ,   yyy.xyz[1] ,   yyy.xyz[2] ,
            &(xxx.xyz[0]), &(xxx.xyz[1]), &(xxx.xyz[2]) ) ;
   yyy = VECMAT_VEC( xyz_to_ijk , xxx ) ;
   *pp = yyy.xyz[0] ; *qq = yyy.xyz[1] ; *rr = yyy.xyz[2] ;
}

/*--------------------------------------------------------------------------*/

void ijk_warp_inv( float  ii , float  jj , float  kk ,
                   float *pp , float *qq , float *rr  )
{
   THD_fvec3 xxx , yyy ;

   LOAD_FVEC3( xxx , ii,jj,kk ) ;
   yyy = VECMAT_VEC( ijk_to_xyz , xxx ) ;
   warp_inv(  yyy.xyz[0] ,   yyy.xyz[1] ,   yyy.xyz[2] ,
            &(xxx.xyz[0]), &(xxx.xyz[1]), &(xxx.xyz[2]) ) ;
   yyy = VECMAT_VEC( xyz_to_ijk , xxx ) ;
   *pp = yyy.xyz[0] ; *qq = yyy.xyz[1] ; *rr = yyy.xyz[2] ;
}

/*--------------------------------------------------------------------------*/

#define WARPDRIVE_SHIFT    1
#define WARPDRIVE_ROTATE   2
#define WARPDRIVE_SCALE    3
#define WARPDRIVE_AFFINE   4
#define WARPDRIVE_BILINEAR 5

#define WARPDRIVE_IS_AFFINE(wc)                            \
  ( (wc) >= WARPDRIVE_SHIFT && (wc) <= WARPDRIVE_AFFINE )

/*--------------------------------------------------------------------------*/
/* For shift-only 'warps' */

static float xsh , ysh , zsh ;

void parset_shift(void)
{
   xsh = parvec[0] ; ysh = parvec[1] ; zsh = parvec[2] ;
}

void warper_shift_for( float aa , float bb , float cc ,
                       float *p , float *q , float *r  )
{
   *p = aa+xsh ; *q = bb+ysh ; *r = cc+zsh ;
}

void warper_shift_inv( float aa , float bb , float cc ,
                       float *p , float *q , float *r  )
{
   *p = aa-xsh ; *q = bb-ysh ; *r = cc-zsh ;
}

/*--------------------------------------------------------------------------*/
/* For affine warps */

static THD_vecmat mv_for , mv_inv ;

void warper_affine_for( float aa , float bb , float cc ,
                        float *p , float *q , float *r  )
{
   THD_fvec3 v , w ;
   LOAD_FVEC3( v , aa,bb,cc ) ;
   w = VECMAT_VEC( mv_for , v ) ;
   *p = w.xyz[0] ; *q = w.xyz[1] ; *r = w.xyz[2] ;
}

void warper_affine_inv( float aa , float bb , float cc ,
                        float *p , float *q , float *r  )
{
   THD_fvec3 v , w ;
   LOAD_FVEC3( v , aa,bb,cc ) ;
   w = VECMAT_VEC( mv_inv , v ) ;
   *p = w.xyz[0] ; *q = w.xyz[1] ; *r = w.xyz[2] ;
}

/*--------------------------------------------------------------------------
   Compute a rotation matrix specified by 3 angles:
      Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
----------------------------------------------------------------------------*/

static THD_mat33 rot_matrix( int ax1, double th1,
                             int ax2, double th2, int ax3, double th3  )
{
   THD_mat33 q , p ;
   LOAD_ROT_MAT( q , th1 , ax1 ) ;
   LOAD_ROT_MAT( p , th2 , ax2 ) ; q = MAT_MUL( p , q ) ;
   LOAD_ROT_MAT( p , th3 , ax3 ) ; q = MAT_MUL( p , q ) ;
   return q ;
}

#define D2R (PI/180.0)                /* angles are in degrees */

static int matorder = MATORDER_SDU ;  /* cf. mrilib.h */
static int smat     = SMAT_LOWER ;
static int dcode    = DELTA_AFTER  ;  /* cf. 3ddata.h */

void parset_affine(void)
{
   THD_mat33 ss,dd,uu,aa,bb ;
   THD_fvec3 vv ;
   float     a,b,c ;

#if 0
{ int ii;fprintf(stderr,"\nparset:");
  for(ii=0;ii<12;ii++)fprintf(stderr," %g",parvec[ii]); fprintf(stderr,"\n");}
#endif

   /* rotation */

   uu = rot_matrix( 2, D2R*parvec[3] , 0, D2R*parvec[4] , 1, D2R*parvec[5] ) ;

   /* scaling */

   a = parvec[6] ; if( a <= 0.01f ) a = 1.0f ;
   b = parvec[7] ; if( b <= 0.01f ) b = 1.0f ;
   c = parvec[8] ; if( c <= 0.01f ) c = 1.0f ;
   LOAD_DIAG_MAT( dd , a,b,c ) ;

   /* shear */

   a = parvec[ 9] ; if( fabs(a) > 0.3333f ) a = 0.0f ;
   b = parvec[10] ; if( fabs(b) > 0.3333f ) b = 0.0f ;
   c = parvec[11] ; if( fabs(c) > 0.3333f ) c = 0.0f ;

   switch( smat ){
     default:
     case SMAT_LOWER:
       LOAD_MAT( ss , 1.0 , 0.0 , 0.0 ,
                       a  , 1.0 , 0.0 ,
                       b  ,  c  , 1.0  ) ;
     break ;

     case SMAT_UPPER:
       LOAD_MAT( ss , 1.0 ,  a  ,  b ,
                      0.0 , 1.0 ,  c ,
                      0.0 , 0.0 , 1.0  ) ;
     break ;
   }

   /* multiply them, as ordered */

   switch( matorder ){
     case MATORDER_SDU:  aa = MAT_MUL(ss,dd) ; bb = uu ; break ;
     case MATORDER_SUD:  aa = MAT_MUL(ss,uu) ; bb = dd ; break ;
     case MATORDER_DSU:  aa = MAT_MUL(dd,ss) ; bb = uu ; break ;
     case MATORDER_DUS:  aa = MAT_MUL(dd,uu) ; bb = ss ; break ;
     case MATORDER_USD:  aa = MAT_MUL(uu,ss) ; bb = dd ; break ;
     case MATORDER_UDS:  aa = MAT_MUL(uu,dd) ; bb = ss ; break ;
   }
   mv_for.mm = MAT_MUL(aa,bb) ;

   LOAD_FVEC3( vv , parvec[0] , parvec[1] , parvec[2] ) ;

   switch( dcode ){
     case DELTA_AFTER:  mv_for.vv = vv ;                       break ;
     case DELTA_BEFORE: mv_for.vv = MATVEC( mv_for.mm , vv ) ; break ;
   }

   mv_inv = INV_VECMAT( mv_for ) ;

#if 0
DUMP_VECMAT("mv_for",mv_for) ; DUMP_VECMAT("mv_inv",mv_inv) ;
#endif
}

/*--------------------------------------------------------------------------*/
/* For bilinear warps */

static float dd_fac = 1.0f ;
static float dd_for[3][3][3] , dd_inv[3][3][3] ;

void warper_bilinear_for( float aa , float bb , float cc ,
                          float *p , float *q , float *r  )
{
   THD_fvec3 v,w ; THD_mat33 dd,ee ;

   LOAD_FVEC3( v , aa,bb,cc ) ;
   w = VECMAT_VEC( mv_for , v ) ;

   dd.mat[0][0] = 1.0f + dd_for[0][0][0]*aa + dd_for[0][0][1]*bb + dd_for[0][0][2]*cc ;
   dd.mat[0][1] =        dd_for[0][1][0]*aa + dd_for[0][1][1]*bb + dd_for[0][1][2]*cc ;
   dd.mat[0][2] =        dd_for[0][2][0]*aa + dd_for[0][2][1]*bb + dd_for[0][2][2]*cc ;
   dd.mat[1][0] =        dd_for[1][0][0]*aa + dd_for[1][0][1]*bb + dd_for[1][0][2]*cc ;
   dd.mat[1][1] = 1.0f + dd_for[1][1][0]*aa + dd_for[1][1][1]*bb + dd_for[1][1][2]*cc ;
   dd.mat[1][2] =        dd_for[1][2][0]*aa + dd_for[1][2][1]*bb + dd_for[1][2][2]*cc ;
   dd.mat[2][0] =        dd_for[2][0][0]*aa + dd_for[2][0][1]*bb + dd_for[2][0][2]*cc ;
   dd.mat[2][1] =        dd_for[2][1][0]*aa + dd_for[2][1][1]*bb + dd_for[2][1][2]*cc ;
   dd.mat[2][2] = 1.0f + dd_for[2][2][0]*aa + dd_for[2][2][1]*bb + dd_for[2][2][2]*cc ;

   ee = MAT_INV(dd) ;
   v  = MATVEC(ee,w) ;

   *p = v.xyz[0] ; *q = v.xyz[1] ; *r = v.xyz[2] ;
}

void warper_bilinear_inv( float aa , float bb , float cc ,
                          float *p , float *q , float *r  )
{
   THD_fvec3 v,w ; THD_mat33 dd,ee ;

   LOAD_FVEC3( v , aa,bb,cc ) ;
   w = VECMAT_VEC( mv_inv , v ) ;

   dd.mat[0][0] = 1.0f + dd_inv[0][0][0]*aa + dd_inv[0][0][1]*bb + dd_inv[0][0][2]*cc ;
   dd.mat[0][1] =        dd_inv[0][1][0]*aa + dd_inv[0][1][1]*bb + dd_inv[0][1][2]*cc ;
   dd.mat[0][2] =        dd_inv[0][2][0]*aa + dd_inv[0][2][1]*bb + dd_inv[0][2][2]*cc ;
   dd.mat[1][0] =        dd_inv[1][0][0]*aa + dd_inv[1][0][1]*bb + dd_inv[1][0][2]*cc ;
   dd.mat[1][1] = 1.0f + dd_inv[1][1][0]*aa + dd_inv[1][1][1]*bb + dd_inv[1][1][2]*cc ;
   dd.mat[1][2] =        dd_inv[1][2][0]*aa + dd_inv[1][2][1]*bb + dd_inv[1][2][2]*cc ;
   dd.mat[2][0] =        dd_inv[2][0][0]*aa + dd_inv[2][0][1]*bb + dd_inv[2][0][2]*cc ;
   dd.mat[2][1] =        dd_inv[2][1][0]*aa + dd_inv[2][1][1]*bb + dd_inv[2][1][2]*cc ;
   dd.mat[2][2] = 1.0f + dd_inv[2][2][0]*aa + dd_inv[2][2][1]*bb + dd_inv[2][2][2]*cc ;

   ee = MAT_INV(dd) ;
   v  = MATVEC(ee,w) ;

   *p = v.xyz[0] ; *q = v.xyz[1] ; *r = v.xyz[2] ;
}

float warper_bilinear_det( float ii , float jj , float kk )
{
   THD_fvec3 x,v,w ; THD_mat33 dd,ee ; int i,j ; float edet,adet,ddet , aa,bb,cc ;
   static int first=1 ;

   LOAD_FVEC3( x , ii,jj,kk )   ; v = VECMAT_VEC( ijk_to_xyz , x ) ;
   UNLOAD_FVEC3( v , aa,bb,cc ) ; w = VECMAT_VEC( mv_for , v ) ;

   dd.mat[0][0] = 1.0f + dd_for[0][0][0]*aa + dd_for[0][0][1]*bb + dd_for[0][0][2]*cc ;
   dd.mat[0][1] =        dd_for[0][1][0]*aa + dd_for[0][1][1]*bb + dd_for[0][1][2]*cc ;
   dd.mat[0][2] =        dd_for[0][2][0]*aa + dd_for[0][2][1]*bb + dd_for[0][2][2]*cc ;
   dd.mat[1][0] =        dd_for[1][0][0]*aa + dd_for[1][0][1]*bb + dd_for[1][0][2]*cc ;
   dd.mat[1][1] = 1.0f + dd_for[1][1][0]*aa + dd_for[1][1][1]*bb + dd_for[1][1][2]*cc ;
   dd.mat[1][2] =        dd_for[1][2][0]*aa + dd_for[1][2][1]*bb + dd_for[1][2][2]*cc ;
   dd.mat[2][0] =        dd_for[2][0][0]*aa + dd_for[2][0][1]*bb + dd_for[2][0][2]*cc ;
   dd.mat[2][1] =        dd_for[2][1][0]*aa + dd_for[2][1][1]*bb + dd_for[2][1][2]*cc ;
   dd.mat[2][2] = 1.0f + dd_for[2][2][0]*aa + dd_for[2][2][1]*bb + dd_for[2][2][2]*cc ;

   ddet = MAT_DET(dd) ;
   if( first && fabs(ddet) < 0.001f ){
     fprintf(stderr,"******* ddet=%g  ii,jj,kk=%g %g %g  aa,bb,cc=%g %g %g\n",
                    ddet,ii,jj,kk, aa,bb,cc ) ;
     DUMP_MAT33("dd",dd) ;
     DUMP_VECMAT("ijk_to_xyz",ijk_to_xyz) ;
     DUMP_VECMAT("xyz_to_ijk",xyz_to_ijk) ;
     first = 0 ;
   }

   ee = MAT_INV(dd) ; edet = MAT_DET(ee) ; v = MATVEC(ee,w) ;

   for( i=0 ; i < 3 ; i++ )
    for( j=0 ; j < 3 ; j++ )
     dd.mat[i][j] = mv_for.mm.mat[i][j] - dd_for[i][0][j]*v.xyz[0]
                                        - dd_for[i][1][j]*v.xyz[1]
                                        - dd_for[i][2][j]*v.xyz[2] ;

   adet = MAT_DET(dd) ;
   return (adet*edet) ;
}

void parset_bilinear(void)
{
   THD_mat33 ai ; THD_fvec3 df,di ; int i,j,k ;

   parset_affine() ;  /* sets up numerator matrices: mv_for and mv_inv */

   /* load forward denominator 3-tensor */

   dd_for[0][0][0] = parvec[12]; dd_for[0][0][1] = parvec[13]; dd_for[0][0][2] = parvec[14];
   dd_for[0][1][0] = parvec[15]; dd_for[0][1][1] = parvec[16]; dd_for[0][1][2] = parvec[17];
   dd_for[0][2][0] = parvec[18]; dd_for[0][2][1] = parvec[19]; dd_for[0][2][2] = parvec[20];
   dd_for[1][0][0] = parvec[21]; dd_for[1][0][1] = parvec[22]; dd_for[1][0][2] = parvec[23];
   dd_for[1][1][0] = parvec[24]; dd_for[1][1][1] = parvec[25]; dd_for[1][1][2] = parvec[26];
   dd_for[1][2][0] = parvec[27]; dd_for[1][2][1] = parvec[28]; dd_for[1][2][2] = parvec[29];
   dd_for[2][0][0] = parvec[30]; dd_for[2][0][1] = parvec[31]; dd_for[2][0][2] = parvec[32];
   dd_for[2][1][0] = parvec[33]; dd_for[2][1][1] = parvec[34]; dd_for[2][1][2] = parvec[35];
   dd_for[2][2][0] = parvec[36]; dd_for[2][2][1] = parvec[37]; dd_for[2][2][2] = parvec[38];

   for( i=0 ; i < 3 ; i++ )
    for( j=0 ; j < 3 ; j++ )
     for( k=0 ; k < 3 ; k++ ) dd_for[i][j][k] *= dd_fac ;  /* 18 Jul 2005 */

   /* compute inverse denominator 3-tensor */

   ai = mv_inv.mm ;
   for( k=0 ; k < 3 ; k++ ){
     for( j=0 ; j < 3 ; j++ ){
       LOAD_FVEC3( df , -dd_for[0][k][j] , -dd_for[1][k][j] , -dd_for[2][k][j] ) ;
       di = MATVEC( ai , df ) ;
       UNLOAD_FVEC3( di , dd_inv[0][j][k] , dd_inv[1][j][k] , dd_inv[2][j][k] ) ;
     }
   }

#if 0
   fprintf(stderr,"++++++++++++ parset_bilinear: dd_fac = %g\n",dd_fac) ;
   for( k=0 ; k < 3 ; k++ ){
     fprintf(stderr," +-------+ dd_for[][][%d]                       | dd_inv[][][%d]\n"
                    "            %11.4g %11.4g %11.4g |  %11.4g %11.4g %11.4g\n"
                    "            %11.4g %11.4g %11.4g |  %11.4g %11.4g %11.4g\n"
                    "            %11.4g %11.4g %11.4g |  %11.4g %11.4g %11.4g\n" ,
         k , k ,
         dd_for[0][0][k] , dd_for[0][1][k] , dd_for[0][2][k] ,
           dd_inv[0][0][k] , dd_inv[0][1][k] , dd_inv[0][2][k] ,
         dd_for[1][0][k] , dd_for[1][1][k] , dd_for[1][2][k] ,
           dd_inv[1][0][k] , dd_inv[1][1][k] , dd_inv[1][2][k] ,
         dd_for[2][0][k] , dd_for[2][1][k] , dd_for[2][2][k] ,
           dd_inv[2][0][k] , dd_inv[2][1][k] , dd_inv[2][2][k]  ) ;
   }
#endif
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset=NULL, *outset=NULL, *baset=NULL, *wtset=NULL ;
   MRI_warp3D_align_basis abas ;
   char *prefix="warpdriven" ;
   int warpdrive_code=-1 , nerr=0 , nx,ny,nz , nopt=1 ;
   float dx,dy,dz , vp ;
   int kim , nvals , kpar , np , nfree ;
   MRI_IMAGE *qim , *tim , *fim ;
   float clip_baset=0.0f , clip_inset=0.0f ;
   char *W_1Dfile=NULL ;                          /* 04 Jan 2005 */
   float **parsave=NULL ;
   int output_float=0 ;                           /* 06 Jul 2005 */
   char *base_idc=NULL , *wt_idc=NULL ;
   int ctstart = NI_clock_time() ;
   float i_xcm,i_ycm,i_zcm , b_xcm,b_ycm,b_zcm ;  /* 26 Sep 2005 */
   float sdif_before , sdif_after ;               /* 28 Sep 2005 */
   char *W_summfile=NULL ; FILE *summfp=NULL ;
   int   init_set=0 ;                             /* 06 Dec 2005 */
   float init_val[12] ;
   char *matrix_save_1D=NULL ;                    /* 25 Jul 2007 */
   FILE *msfp=NULL ;
   int null_output=0 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
            "Usage: 3dWarpDrive [options] dataset\n"
            "Warp a dataset to match another one (the base).\n"
            "\n"
            "This program is a generalization of 3dvolreg.  It tries to find\n"
            "a spatial transformation that warps a given dataset to match an\n"
            "input dataset (given by the -base option).  It will be slow.\n"
            "\n"
            "-->> Also see the script align_epi_anat.py for a more general\n"
            "     alignment procedure, which does not require that the two\n"
            "     datasets be defined on the same 3D grid.\n"
            "\n"
            "--------------------------\n"
            "Transform Defining Options: [exactly one of these must be used]\n"
            "--------------------------\n"
            "  -shift_only         =  3 parameters (shifts)\n"
            "  -shift_rotate       =  6 parameters (shifts + angles)\n"
            "  -shift_rotate_scale =  9 parameters (shifts + angles + scale factors)\n"
            "  -affine_general     = 12 parameters (3 shifts + 3x3 matrix)\n"
            "  -bilinear_general   = 39 parameters (3 + 3x3 + 3x3x3)\n"
            "\n"
            "  N.B.: At this time, the image intensity is NOT \n"
            "         adjusted for the Jacobian of the transformation.\n"
            "  N.B.: -bilinear_general is not yet implemented.\n"
            "\n"
            "-------------\n"
            "Other Options:\n"
            "-------------\n"
            "  -linear   }\n"
            "  -cubic    } = Chooses spatial interpolation method.\n"
            "  -NN       } =   [default = linear; inaccurate but fast]\n"
            "  -quintic  }     [for accuracy, try '-cubic -final quintic']\n"
            "\n"
            "  -base bbb   = Load dataset 'bbb' as the base to which the\n"
            "                  input dataset will be matched.\n"
            "                  [This is a mandatory option]\n"
            "\n"
            "  -verb       = Print out lots of information along the way.\n"
            "  -prefix ppp = Sets the prefix of the output dataset.\n"
            "                If 'ppp' is 'NULL', no output dataset is written.\n"
            "  -input ddd  = You can put the input dataset anywhere in the\n"
            "                  command line option list by using the '-input'\n"
            "                  option, instead of always putting it last.\n"
            "  -summ sss   = Save summary of calculations into text file 'sss'.\n"
            "                  (N.B.: If 'sss' is '-', summary goes to stdout.)\n"
            "\n"
            "-----------------\n"
            "Technical Options:\n"
            "-----------------\n"
            "  -maxite    m  = Allow up to 'm' iterations for convergence.\n"
            "  -delta     d  = Distance, in voxel size, used to compute\n"
            "                   image derivatives using finite differences.\n"
            "                   [Default=1.0]\n"
            "  -weight  wset = Set the weighting applied to each voxel\n"
            "                   proportional to the brick specified here.\n"
            "                   [Default=computed by program from base]\n"
            "  -thresh    t  = Set the convergence parameter to be RMS 't' voxels\n"
            "                   movement between iterations.  [Default=0.03]\n"
            "  -twopass      = Do the parameter estimation in two passes,\n"
            "                   coarse-but-fast first, then fine-but-slow second\n"
            "                   (much like the same option in program 3dvolreg).\n"
            "                   This is useful if large-ish warping is needed to\n"
            "                   align the volumes.\n"
            "  -final 'mode' = Set the final warp to be interpolated using 'mode'\n"
            "                   instead of the spatial interpolation method used\n"
            "                   to find the warp parameters.\n"
            "  -parfix n v   = Fix the n'th parameter of the warp model to\n"
            "                   the value 'v'.  More than one -parfix option\n"
            "                   can be used, to fix multiple parameters.\n"
            "  -1Dfile ename = Write out the warping parameters to the file\n"
            "                   named 'ename'.  Each sub-brick of the input\n"
            "                   dataset gets one line in this file.  Each\n"
            "                   parameter in the model gets one column.\n"
            "  -float        = Write output dataset in float format, even if\n"
            "                   input dataset is short or byte.\n"
            "  -coarserot    = Initialize shift+rotation parameters by a\n"
            "                   brute force coarse search, as in the similar\n"
            "                   3dvolreg option.\n"
            "\n"
            "  -1Dmatrix_save ff = Save base-to-input transformation matrices\n"
            "                      in file 'ff' (1 row per sub-brick in the input\n"
            "                      dataset).  If 'ff' does NOT end in '.1D', then\n"
            "                      the program will append '.aff12.1D' to 'ff' to\n"
            "                      make the output filename.\n"
            "          *N.B.: This matrix is the coordinate transformation from base\n"
            "                 to input DICOM coordinates.  To get the inverse matrix\n"
            "                 (input-to-base), use the cat_matvec program, as in\n"
            "                   cat_matvec fred.aff12.1D -I\n"
            "\n"
            "----------------------\n"
            "AFFINE TRANSFORMATIONS:\n"
            "----------------------\n"
            "The options below control how the affine tranformations\n"
            "(-shift_rotate, -shift_rotate_scale, -affine_general)\n"
            "are structured in terms of 3x3 matrices:\n"
            "\n"
            "  -SDU or -SUD }= Set the order of the matrix multiplication\n"
            "  -DSU or -DUS }= for the affine transformations:\n"
            "  -USD or -UDS }=   S = triangular shear (params #10-12)\n"
            "                    D = diagonal scaling matrix (params #7-9)\n"
            "                    U = rotation matrix (params #4-6)\n"
            "                  Default order is '-SDU', which means that\n"
            "                  the U matrix is applied first, then the\n"
            "                  D matrix, then the S matrix.\n"
            "\n"
            "  -Supper      }= Set the S matrix to be upper or lower\n"
            "  -Slower      }= triangular [Default=lower triangular]\n"
            "\n"
            "  -ashift OR   }= Apply the shift parameters (#1-3) after OR\n"
            "  -bshift      }= before the matrix transformation. [Default=after]\n"
            "\n"
            "The matrices are specified in DICOM-ordered (x=-R+L,y=-A+P,z=-I+S)\n"
            "coordinates as:\n"
            "\n"
            "  [U] = [Rotate_y(param#6)] [Rotate_x(param#5)] [Rotate_z(param #4)]\n"
            "        (angles are in degrees)\n"
            "\n"
            "  [D] = diag( param#7 , param#8 , param#9 )\n"
            "\n"
            "        [    1        0     0 ]        [ 1 param#10 param#11 ]\n"
            "  [S] = [ param#10    1     0 ]   OR   [ 0    1     param#12 ]\n"
            "        [ param#11 param#12 1 ]        [ 0    0        1     ]\n"
            "\n"
            " For example, the default (-SDU/-ashift/-Slower) has the warp\n"
            " specified as [x]_warped = [S] [D] [U] [x]_in + [shift].\n"
            " The shift vector comprises parameters #1, #2, and #3.\n"
            "\n"
            " The goal of the program is to find the warp parameters such that\n"
            "   I([x]_warped) = s * J([x]_in)\n"
            " as closely as possible in a weighted least squares sense, where\n"
            " 's' is a scaling factor (an extra, invisible, parameter), J(x)\n"
            " is the base image, I(x) is the input image, and the weight image\n"
            " is a blurred copy of J(x).\n"
            "\n"
            " Using '-parfix', you can specify that some of these parameters\n"
            " are fixed.  For example, '-shift_rotate_scale' is equivalent\n"
            " '-affine_general -parfix 10 0 -parfix 11 0 -parfix 12 0'.\n"
            " Don't attempt to use the '-parfix' option unless you understand\n"
            " this example!\n"
            "\n"
            "-------------------------\n"
            "  RWCox - November 2004\n"
            "-------------------------\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- startup mechanics --*/

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   mainENTRY("3dWarpDrive main"); machdep(); AFNI_logger("3dWarpDrive",argc,argv);
   PRINT_VERSION("3dWarpDrive") ; AUTHOR("RW Cox") ;
   THD_check_AFNI_version("3dWarpDrive") ;

   /* initialize parameters of the alignment basis struct */

   abas.nparam     = 0 ;
   abas.param      = NULL ;
   abas.scale_init = 1.0f ;
   abas.delfac     = 1.0f ;
   abas.tolfac     = 0.03f ;
   abas.twoblur    = 0.0f ;
   abas.regmode    = MRI_LINEAR ;
   abas.regfinal   = -1 ;
   abas.verb       = 0 ;
   abas.max_iter   = 0 ;
   abas.wtproc     = 1 ;
   abas.imbase     = NULL ;
   abas.imwt       = NULL ;
   abas.vwfor      = ijk_warp_for ;
   abas.vwinv      = ijk_warp_inv ;
   abas.vwset      = load_parvec ;
   abas.vwdet      = NULL ;

   abas.xedge = abas.yedge = abas.zedge = -1 ;
   abas.imww  = abas.imap  = abas.imps  = abas.imsk = NULL ;
   abas.imps_blur = NULL ;

   nparfix = 0 ;

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     /*-----*/

     if( strcmp(argv[nopt],"-summ") == 0 ){    /* 28 Sep 2005 */
       if( ++nopt >= argc )
         ERROR_exit("Need 1 parameter afer -summ!\n");
       W_summfile = strdup( argv[nopt] ) ;
       if( !THD_filename_ok(W_summfile) )
         ERROR_exit("Name after -summ has bad characters!\n") ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-float") == 0 ){   /* 06 Jul 2005 */
       output_float = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-coarserot") == 0 ){  /* 05 Dec 2005 */
       if( VL_coarse_rot ) INFO_message("-coarserot is already on") ;
       VL_coarse_rot = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-1Dmatrix_save") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need 1 parameter afer -1Dmatrix_save!\n");
       if( !THD_filename_ok(argv[nopt]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[nopt-1],argv[nopt]) ;
       if( STRING_HAS_SUFFIX(argv[nopt],".1D") ){
         matrix_save_1D = strdup(argv[nopt]) ;
       } else {
         matrix_save_1D = calloc(sizeof(char),strlen(argv[nopt])+16) ;
         strcpy(matrix_save_1D,argv[nopt]); strcat(matrix_save_1D,".aff12.1D");
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-1Dfile") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need 1 parameter afer -1Dfile!\n");
       W_1Dfile = strdup( argv[nopt] ) ;
       if( !THD_filename_ok(W_1Dfile) )
         ERROR_exit("Name after -1Dfile has bad characters!\n") ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-twopass") == 0 ){
       float bbb = AFNI_numenv("AFNI_WARPDRIVE_TWOBLUR") ;
       abas.twoblur = (bbb==0.0f) ? 3.0f : bbb ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-SDU") == 0 ){
       matorder = MATORDER_SDU ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-SUD") == 0 ){
       matorder = MATORDER_SUD ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-DSU") == 0 ){
       matorder = MATORDER_DSU ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-DUS") == 0 ){
       matorder = MATORDER_DUS ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-USD") == 0 ){
       matorder = MATORDER_USD ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-UDS") == 0 ){
       matorder = MATORDER_UDS ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-ashift") == 0 ){
       dcode = DELTA_AFTER     ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-bshift") == 0 ){
       dcode = DELTA_BEFORE    ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-Slower") == 0 ){
       smat  = SMAT_LOWER      ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-Supper") == 0 ){
       smat  = SMAT_UPPER      ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-final") == 0 ){
       char *str ;

       if( ++nopt >= argc )
         ERROR_exit("Need 1 parameter afer -final!\n") ;
       str = argv[nopt] ; if( *str == '-' ) str++ ;

            if( strcmp(str,"cubic")   == 0 ) abas.regfinal = MRI_CUBIC ;
       else if( strcmp(str,"quintic") == 0 ) abas.regfinal = MRI_QUINTIC ;
       else if( strcmp(str,"linear") == 0  ) abas.regfinal = MRI_LINEAR ;
       else if( strcmp(str,"NN")      == 0 ) abas.regfinal = MRI_NN ;
       else
         ERROR_exit("Illegal mode after -final\n");
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-parfix") == 0 ){
       int np , ip ; float vp ;
       if( ++nopt >= argc-1 )
         ERROR_exit("Need 2 parameters afer -parfix!\n");
       np = strtol( argv[nopt] , NULL , 10 ) ; nopt++ ;
       vp = strtod( argv[nopt] , NULL ) ;
       if( np <= 0 || np > MAXPAR )
         ERROR_exit("Param #%d after -parfix is illegal!\n",np) ;
       for( ip=0 ; ip < nparfix ; ip++ ){
         if( parfix[ip].np == np ){
           WARNING_message("Ignoring later -parfix option for param #%d\n",ip);
           break ;
         }
       }
       if( ip == nparfix ) nparfix++ ;
       parfix[ip].np = np ; parfix[ip].vp = vp ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-shift_only") == 0 ){
       warpdrive_code = WARPDRIVE_SHIFT ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-shift_rotate") == 0 ){
       warpdrive_code = WARPDRIVE_ROTATE ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-shift_rotate_scale") == 0 ){
       warpdrive_code = WARPDRIVE_SCALE ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-affine_general") == 0 ){
       warpdrive_code = WARPDRIVE_AFFINE ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-bilinear_general") == 0 ){  /* not implemented */
#if 0
       ERROR_exit("3dWarpDrive -bilinear_general NOT IMPLEMENTED!\n");
#else
       WARNING_message("-bilinear_general transformation is experimental!") ;
#endif
       warpdrive_code = WARPDRIVE_BILINEAR ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-NN")     == 0 ){
       abas.regmode = MRI_NN      ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-linear") == 0 ){
       abas.regmode = MRI_LINEAR  ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-cubic")  == 0 ){
       abas.regmode = MRI_CUBIC   ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-quintic") == 0 ){
       abas.regmode = MRI_QUINTIC ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -prefix!\n");
       if( !THD_filename_ok(argv[nopt]) )
         ERROR_exit("-prefix argument is invalid!\n");
       prefix = argv[nopt] ; if( strcmp(prefix,"NULL") == 0 ) null_output = 1 ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[nopt],"-verbose",5) == 0 ){
       abas.verb++ ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-base") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -base!\n");
       baset = THD_open_dataset( argv[nopt] ) ;
       if( baset == NULL )
         ERROR_exit("Can't open -base dataset %s\n",argv[nopt]);
       if( DSET_NVALS(baset) > 1 )
         WARNING_message(
           "-base dataset %s has %d sub-bricks; will only use #0\n",
           argv[nopt],DSET_NVALS(baset) ) ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-weight") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -weight!\n");
       if( wtset != NULL ){
         WARNING_message("2nd -weight option replaces 1st") ;
         DSET_delete(wtset) ;
       }
       wtset = THD_open_dataset( argv[nopt] ) ;
       if( wtset == NULL )
         ERROR_exit("Can't open -weight dataset %s\n",argv[nopt]);
       if( DSET_NVALS(wtset) > 1 )
         WARNING_message(
           "-weight %s has %d sub-bricks; will only use #0\n",
           argv[nopt],DSET_NVALS(wtset) ) ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-input") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -input!\n");
       inset = THD_open_dataset( argv[nopt] ) ;
       if( inset == NULL )
         ERROR_exit("Can't open -input dataset %s\n",argv[nopt]);
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-maxite") == 0 ){
       int ival ;
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -maxite!\n");
       ival = strtol( argv[nopt] , NULL , 10 ) ;
       if( ival > 1 ) abas.max_iter = ival ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-delta") == 0 ){
       float val ;
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -delta!\n");
       val = strtod( argv[nopt] , NULL ) ;
       if( val > 0.0499 && val < 49.99 ) abas.delfac = val ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-thresh") == 0 ){
       float val ;
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -thresh!\n");
       val = strtod( argv[nopt] , NULL ) ;
       if( val > 0.001 && val < 3.01 ) abas.tolfac = val ;
       nopt++ ; continue ;
     }

     /*-----*/

     ERROR_exit("Unknown option %s\n",argv[nopt]) ;

   } /*--- end of loop over command line options ---*/

   /*-- 1 remaining argument should be a dataset --*/

   if( inset == NULL && nopt != argc-1 )
     ERROR_exit("Command line should have exactly 1 dataset!\n"
                "**         Whereas there seems to be %d of them!\n",
             argc-nopt ) ;

   /*-- input dataset header --*/

   if( inset == NULL ){
     inset = THD_open_dataset( argv[nopt] ) ;
     if( !ISVALID_DSET(inset) )
       ERROR_exit("Can't open dataset %s\n",argv[nopt]);
   }

   if( baset == NULL ){
     ERROR_message("Need to specify a base dataset!\n") ;
     nerr++ ;
   }

   nx = DSET_NX(inset) ; ny = DSET_NY(inset) ; nz = DSET_NZ(inset) ;
   dx = DSET_DX(inset) ; dy = DSET_DY(inset) ; dz = DSET_DZ(inset) ;

   if( DSET_NX(baset) != nx || DSET_NY(baset) != ny || DSET_NZ(baset) != nz ){
     ERROR_message("base and input grid dimensions don't match!\n") ;
     ERROR_message("base  is %d X %d X %d voxels\n",
                    DSET_NX(baset),DSET_NY(baset),DSET_NZ(baset) ) ;
     ERROR_exit   ("input is %d X %d X %d voxels\n",nx,ny,nz) ;
   }

   /*- load datasets from disk; if can't do so, fatal errors all around -*/

   if( abas.verb ) INFO_message("Loading datasets\n") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   nvals = DSET_NVALS(inset) ;
   if( nvals == 1 ){
     clip_inset = THD_cliplevel( DSET_BRICK(inset,0) , 0.0 ) ;
     if( DSET_BRICK_FACTOR(inset,0) > 0.0f )
       clip_inset *= DSET_BRICK_FACTOR(inset,0) ;
       mri_get_cmass_3D( DSET_BRICK(inset,0) , &i_xcm,&i_ycm,&i_zcm ) ;
   } else {
     qim = THD_median_brick( inset ) ;
     clip_inset = THD_cliplevel( qim , 0.0 ) ;
     mri_get_cmass_3D( qim , &i_xcm,&i_ycm,&i_zcm ) ;
     mri_free(qim) ;
   }

   DSET_load(baset) ; CHECK_LOAD_ERROR(baset) ;
   mri_get_cmass_3D( DSET_BRICK(baset,0) , &b_xcm,&b_ycm,&b_zcm ) ;
   abas.imbase = mri_scale_to_float( DSET_BRICK_FACTOR(baset,0) ,
                                     DSET_BRICK(baset,0)         ) ;
   clip_baset  = THD_cliplevel( abas.imbase , 0.0 ) ;
   base_idc    = strdup(baset->idcode.str) ;
   DSET_unload(baset) ;

   if( wtset != NULL ){
     DSET_load(wtset) ; CHECK_LOAD_ERROR(wtset) ;
     abas.imwt = mri_scale_to_float( DSET_BRICK_FACTOR(wtset,0) ,
                                     DSET_BRICK(wtset,0)         ) ;
     wt_idc    = strdup(wtset->idcode.str) ;
     DSET_unload(wtset) ;
   }

   if( abas.verb ) INFO_message("Checking inputs\n") ;

   /*-- parameterize the warp model --*/

   /*! Macro to add a parameter to the warp3D model.
        - nm = name of parameter
        - bb = min value allowed
        - tt = max value allowed
        - id = value for identity warp
        - dd = delta to use for stepsize
        - ll = tolerance for convergence test */

#define ADDPAR(nm,bb,tt,id,dd,ll)                               \
 do{ int p=abas.nparam ;                                        \
     abas.param = (MRI_warp3D_param_def *) realloc(             \
                      (void *)abas.param ,                      \
                      sizeof(MRI_warp3D_param_def)*(p+1) ) ;    \
     abas.param[p].min   = (bb) ; abas.param[p].max   = (tt) ;  \
     abas.param[p].delta = (dd) ; abas.param[p].toler = (ll) ;  \
     abas.param[p].ident = abas.param[p].val_init = (id) ;      \
     strcpy( abas.param[p].name , (nm) ) ;                      \
     abas.param[p].fixed = 0 ;                                  \
     abas.nparam = p+1 ;                                        \
 } while(0)

   nerr = 0 ;
   if( warpdrive_code <= 0 ){
     ERROR_message("Need a transform-specifying option!\n");
     nerr++ ;
   } else if( WARPDRIVE_IS_AFFINE(warpdrive_code) ) {

       char *lab09, *lab10, *lab11 ;
       float xd,yd,zd , xl,yl,zl , xxx ;

       xd = 3.33*fabs( dx * (i_xcm-b_xcm) ) ; xl = 0.333 * fabs( dx*nx ) ;
       yd = 3.33*fabs( dy * (i_ycm-b_ycm) ) ; yl = 0.333 * fabs( dy*ny ) ;
       zd = 3.33*fabs( dz * (i_zcm-b_zcm) ) ; zl = 0.333 * fabs( dz*nz ) ;
       xd = MAX(xd,xl) ; yd = MAX(yd,yl) ; zd = MAX(zd,zl) ;
       xxx = sqrt( xd*xd + yd*yd + zd*zd ) ;

       /* add all 12 parameters (may ignore some, later) */

       ADDPAR( "x-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "z-angle" , -120.0 , 120.0 , 0.0 , 0.0 , 0.0 ) ;  /* degrees */
       ADDPAR( "x-angle" , -120.0 , 120.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-angle" , -120.0 , 120.0 , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "x-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;  /* identity */
       ADDPAR( "y-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;  /*  == 1.0 */
       ADDPAR( "z-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;

       switch( smat ){
         default:
         case SMAT_LOWER:
           lab09 = "y/x-shear" ; lab10 = "z/x-shear" ; lab11 = "z/y-shear" ;
         break ;

         case SMAT_UPPER:
           lab09 = "x/y-shear" ; lab10 = "x/z-shear" ; lab11 = "y/z-shear" ;
         break ;
       }
       ADDPAR( lab09 , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( lab10 , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( lab11 , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;

       /* initialize transform parameter vector */

       for( kpar=0 ; kpar < 12 ; kpar++ )
         parvec[kpar] = abas.param[kpar].ident ;

       /* initialize transformation function pointers */

       if( warpdrive_code == WARPDRIVE_SHIFT ){
         warp_parset = parset_shift ;
         warp_for    = warper_shift_for ;
         warp_inv    = warper_shift_inv ;
       } else {
         warp_parset = parset_affine ;
         warp_for    = warper_affine_for ;
         warp_inv    = warper_affine_inv ;
       }

       /* how many parameters to actually pay attention to */

       switch( warpdrive_code ){
         case WARPDRIVE_SHIFT:  abas.nparam =  3 ; break ;
         case WARPDRIVE_ROTATE: abas.nparam =  6 ; break ;
         case WARPDRIVE_SCALE:  abas.nparam =  9 ; break ;
         case WARPDRIVE_AFFINE: abas.nparam = 12 ; break ;
       }

   } else if( warpdrive_code == WARPDRIVE_BILINEAR ){

       char *lab09, *lab10, *lab11 , labxx[16] ;
       float xr,yr,zr,rr ;
       float xd,yd,zd , xl,yl,zl , xxx ;

       xd = 3.33*fabs( dx * (i_xcm-b_xcm) ) ; xl = 0.333 * fabs( dx*nx ) ;
       yd = 3.33*fabs( dy * (i_ycm-b_ycm) ) ; yl = 0.333 * fabs( dy*ny ) ;
       zd = 3.33*fabs( dz * (i_zcm-b_zcm) ) ; zl = 0.333 * fabs( dz*nz ) ;
       xd = MAX(xd,xl) ; yd = MAX(yd,yl) ; zd = MAX(zd,zl) ;
       xxx = sqrt( xd*xd + yd*yd + zd*zd ) ;

       /* add all 39 parameters (may ignore some, later) */

       ADDPAR( "x-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "z-angle" , -120.0 , 120.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "x-angle" , -120.0 , 120.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-angle" , -120.0 , 120.0 , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "x-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;

       switch( smat ){
         default:
         case SMAT_LOWER:
           lab09 = "y/x-shear" ; lab10 = "z/x-shear" ; lab11 = "z/y-shear" ;
         break ;

         case SMAT_UPPER:
           lab09 = "x/y-shear" ; lab10 = "x/z-shear" ; lab11 = "y/z-shear" ;
         break ;
       }
       ADDPAR( lab09 , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( lab10 , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( lab11 , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;

       xr = 0.5f*fabs(dx)*nx ; yr = 0.5f*fabs(dy)*ny ; zr = 0.5f*fabs(dz)*nz ;
       rr = MAX(xr,yr)       ; rr = MAX(rr,zr)       ; dd_fac = 1.0f / rr ;
       for( kpar=12 ; kpar < 39 ; kpar++ ){
         sprintf(labxx,"blin_%02d",kpar+1) ;
         ADDPAR( labxx , -0.1234 , 0.1234 , 0.0 , 0.0 , 0.0 ) ;
       }

       /* initialize transform parameter vector */

       for( kpar=0 ; kpar < 39 ; kpar++ )
         parvec[kpar] = abas.param[kpar].ident ;

       abas.vwdet = warper_bilinear_det ;

       /* initialize transformation function pointers */

       warp_parset = parset_bilinear ;
       warp_for    = warper_bilinear_for ;
       warp_inv    = warper_bilinear_inv ;

       /* how many parameters to actually pay attention to */

       abas.nparam = 39 ;

   } else {
     ERROR_message("Unimplemented transform model!\n") ;
     nerr++ ;
   }

   /* Deal with -parfix options; nfree will be number of free parameters */

   nfree = abas.nparam ;
   for( kpar=0 ; kpar < nparfix ; kpar++ ){
     np = parfix[kpar].np - 1 ; vp = parfix[kpar].vp ;
     if( np >= 0 && np < abas.nparam ){
       if( vp >= abas.param[np].min && vp <= abas.param[np].max ){
         abas.param[np].fixed     = 1  ;
         abas.param[np].val_fixed = vp ;
         nfree -- ;
       } else {        /* bad value */
         ERROR_message("-parfix for param #%d has illegal value!\n",np+1) ;
         nerr++ ;
       }
     } else {          /* bad index */
       WARNING_message("-parfix for param #%d is out of range 1..%d\n",
                        np+1 , abas.nparam+1 ) ;
     }
   }
   if( nfree <= 0 ){
     ERROR_message("No free parameters in transform model!\n") ;
     nerr++ ;
   }

   /*-- default number of iterations allowed --*/

   if( abas.max_iter <= 0 ) abas.max_iter = 11*nfree+5 ;

   /*-- other checks for good set of inputs --*/

   if( nerr ) exit(1) ;  /** bad user!! **/

   if( abas.verb ) INFO_message("Creating empty output dataset\n") ;

   outset = EDIT_empty_copy( inset ) ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   if( output_float ){
     EDIT_dset_items( outset , ADN_datum_all , MRI_float , ADN_none ) ;
     for( kim=0 ; kim < nvals ; kim++ )
       EDIT_BRICK_FACTOR( outset , kim , 0.0 ) ;
   }

   if( THD_deathcon() && THD_is_file( DSET_HEADNAME(outset) ) )
     ERROR_exit("Output file %s already exists!\n",DSET_HEADNAME(outset) ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarpDrive" , argc,argv , outset ) ;

   outset->daxes->xxorg = inset->daxes->xxorg ;
   outset->daxes->yyorg = inset->daxes->yyorg ;
   outset->daxes->zzorg = inset->daxes->zzorg ;

   /*-- more checks --*/

   /* the following aren't fatal errors, but merit a warning slap in the face */

   if( FLDIF(DSET_DX(baset),dx) ||
       FLDIF(DSET_DY(baset),dy) || FLDIF(DSET_DZ(baset),dz) ){
     WARNING_message("base and input grid spacings don't match!\n"
                     /*"If this is not by design or if registration\n"
                     "fails, use 3dresample with -master option:\n"
                     "  3dresample -master %s \\\n"
                     "             -input %s \\\n"
                     "             -prefix RS\n"
                     "to sesample the input data and rerun the\n"
                     "registration with RS as the input.\n", DSET_BRIKNAME(baset), DSET_BRIKNAME(inset) */) ;
     WARNING_message("base  grid = %.5f X %.5f X %.5f mm\n",
                     DSET_DX(baset),DSET_DY(baset),DSET_DZ(baset) ) ;
     WARNING_message("input grid = %.5f X %.5f X %.5f mm\n",
                     DSET_DX(inset),DSET_DY(inset),DSET_DZ(inset) ) ;
   }

   if( FLDIF(DSET_XORG(baset),DSET_XORG(inset)) ||
       FLDIF(DSET_YORG(baset),DSET_YORG(inset)) ||
       FLDIF(DSET_ZORG(baset),DSET_ZORG(inset))   ){
     WARNING_message("base and input grid offsets don't match!\n"
                     /*"If this is not by design or if registration\n"
                     "fails, use 3dresample with -master option:\n"
                     "  3dresample -master %s\\\n"
                     "             -input %s \\\n"
                     "             -prefix RS\n"
                     "to sesample the input data and rerun the\n"
                     "registration with RS as the input.\n", DSET_BRIKNAME(baset), DSET_BRIKNAME(inset) */) ;
     WARNING_message("base  offsets = %.5f X %.5f X %.5f mm\n",
                     DSET_XORG(baset),DSET_YORG(baset),DSET_ZORG(baset) ) ;
     WARNING_message("input offsets = %.5f X %.5f X %.5f mm\n",
                     DSET_XORG(inset),DSET_YORG(inset),DSET_ZORG(inset) ) ;
   }

   if( baset->daxes->xxorient != inset->daxes->xxorient ||
       baset->daxes->yyorient != inset->daxes->yyorient ||
       baset->daxes->zzorient != inset->daxes->zzorient   ){
     WARNING_message("base and input orientations don't match!\n"
                     /*"If this is not by design or if registration\n"
                     "fails, use 3dresample with -master option:\n"
                     "  3dresample -master %s \\\n"
                     "             -input %s \\\n"
                     "             -prefix RS\n"
                     "to sesample the input data and rerun the\n"
                     "registration with RS as the input.\n", DSET_BRIKNAME(baset), DSET_BRIKNAME(inset) */) ;
     WARNING_message("base  = %s X %s X %s\n",
             ORIENT_shortstr[baset->daxes->xxorient] ,
             ORIENT_shortstr[baset->daxes->yyorient] ,
             ORIENT_shortstr[baset->daxes->zzorient]  ) ;
     WARNING_message("input = %s X %s X %s\n",
             ORIENT_shortstr[inset->daxes->xxorient] ,
             ORIENT_shortstr[inset->daxes->yyorient] ,
             ORIENT_shortstr[inset->daxes->zzorient]  ) ;
   }

   /*- however, this is a fatal error -*/

   if( wtset != NULL &&
      (DSET_NX(wtset) != nx || DSET_NY(wtset) != ny || DSET_NZ(wtset) != nz) ){
     ERROR_message("weight and input grid dimensions don't match!\n") ;
     ERROR_message("weight is %d X %d X %d voxels\n",
                   DSET_NX(wtset),DSET_NY(wtset),DSET_NZ(wtset)    ) ;
     ERROR_exit   ("input  is %d X %d X %d voxels\n",nx,ny,nz) ;
   }

   /*-- set up (x,y,z) <-> (i,j,k) transformations ---*/

   { THD_vecmat ijk_to_inset_xyz , xyz_to_dicom ;

     LOAD_DIAG_MAT( ijk_to_inset_xyz.mm ,
                    inset->daxes->xxdel ,
                    inset->daxes->yydel , inset->daxes->zzdel );

     if( warpdrive_code == WARPDRIVE_BILINEAR ){
       /* define (x,y,z)=(0,0,0) at mid-point of dataset 3D array */

       LOAD_FVEC3   ( ijk_to_inset_xyz.vv ,
                      -0.5*(nx-1)*inset->daxes->xxdel ,
                      -0.5*(ny-1)*inset->daxes->yydel ,
                      -0.5*(nz-1)*inset->daxes->zzdel  ) ;

     } else {
       /* define (x,y,z) based strictly on dataset coords */

       LOAD_FVEC3   ( ijk_to_inset_xyz.vv ,
                      DSET_XORG(inset) , DSET_YORG(inset), DSET_ZORG(inset) ) ;
     }

     xyz_to_dicom.mm = inset->daxes->to_dicomm ;
     LOAD_FVEC3( xyz_to_dicom.vv , 0.0,0.0,0.0 ) ;

     ijk_to_xyz = MUL_VECMAT( xyz_to_dicom , ijk_to_inset_xyz ) ;
     xyz_to_ijk = INV_VECMAT( ijk_to_xyz ) ;

#if 1
     if( abas.verb ){
       DUMP_VECMAT("ijk_to_xyz",ijk_to_xyz) ;
       DUMP_VECMAT("xyz_to_ijk",xyz_to_ijk) ;
     }
#endif
   }

   /*-- make the shell of the new dataset --*/

   if( clip_baset > 0.0f && clip_inset > 0.0f ){
     float fac = clip_inset / clip_baset ;
     abas.scale_init = fac ;
     if( abas.verb || fac >= 100.0 || fac <= 0.01 )
       INFO_message("Initial scale factor set to %.2f/%.2f=%.2g\n",
                     clip_baset , clip_inset , fac ) ;
   }

   /*===== do the hard work =====*/

   /*-- 05 Dec 2005: initialize shift + rotation parameters? --*/

   if( VL_coarse_rot && abas.nparam >= 6 ){
     float roll, pitch , yaw , sx , sy , sz ;
     int   dxp , dyp   , dzp ;
     MRI_IMAGE *vim ;
     THD_fvec3  dxyz , sxyz ;

     if( abas.verb ) fprintf(stderr,"++ Coarse initialize ") ;

     vim = THD_median_brick( inset ) ;
     (void)get_best_shiftrot( &abas , abas.imbase , vim ,
                              &roll,&pitch,&yaw , &dxp,&dyp,&dzp ) ;
     mri_free(vim) ;

     LOAD_FVEC3( dxyz , -dxp,-dyp,-dzp ) ;
     sxyz = MATVEC( ijk_to_xyz.mm , dxyz ) ;
     UNLOAD_FVEC3( sxyz , sx,sy,sz ) ;

     init_set    = 6 ;
     init_val[0] = sx   ; init_val[1] = sy    ; init_val[2] = sz  ;
     init_val[3] = roll ; init_val[4] = pitch ; init_val[5] = yaw ;

     if( abas.verb )
       fprintf(stderr,
               "\n++  dx=%.1f dy=%.1f dz=%.1f  roll=%.1f pitch=%.1f yaw=%.1f\n",
               sx,sy,sz , roll,pitch,yaw ) ;
   }

   if( abas.verb ) INFO_message("Beginning alignment setup\n") ;

   /* 04 Jan 2005: set up to save the computed parameters */

   parsave = (float **)malloc( sizeof(float *) * abas.nparam ) ;
   for( kpar=0 ; kpar < abas.nparam ; kpar++ )
     parsave[kpar] = (float *)calloc( sizeof(float) , nvals ) ;

   mri_warp3D_align_setup( &abas ) ;

   /*-- open summ file, if desired --*/

   if( W_summfile != NULL ){
     if( THD_is_file(W_summfile) )
       WARNING_message("Over-writing -summ file '%s'",W_summfile) ;
     if( strcmp(W_summfile,"-") == 0 ) summfp = stdout ;
     else                              summfp = fopen( W_summfile, "w" ) ;
     if( summfp == NULL )
       ERROR_message("Can't open -summ file '%s'",W_summfile) ;
   }

   if( abas.verb ) INFO_message("Beginning alignment loop\n") ;

   /**--------------- loop over input sub-bricks ---------------**/

   for( kim=0 ; kim < nvals ; kim++ ){

     for( kpar=0 ; kpar < abas.nparam ; kpar++ ){  /** init params **/
       if( abas.param[kpar].fixed )
         abas.param[kpar].val_init = abas.param[kpar].val_fixed ;
       else if( kpar < init_set )
         abas.param[kpar].val_init = init_val[kpar] ;  /* 06 Dec 2005 */
       else
         abas.param[kpar].val_init = abas.param[kpar].ident ;
     }

     /** create copy of input brick into qim
         then warp-align it, with result into tim **/

     qim = mri_scale_to_float( DSET_BRICK_FACTOR(inset,kim) ,
                               DSET_BRICK(inset,kim)         ) ;

     sdif_before = mri_scaled_diff( abas.imbase , qim , abas.imsk ) ;

     tim = mri_warp3D_align_one( &abas , qim ) ;
     mri_free( qim ) ; DSET_unload_one( inset , kim ) ;

     sdif_after = mri_scaled_diff( abas.imbase , tim , abas.imsk ) ;

#if 0
     if( abas.verb ){ DUMP_VECMAT( "end mv_for" , mv_for ) ; }
#endif

     if( abas.verb )
       INFO_message("#%d RMS_diff: before=%g  after=%g",kim,sdif_before,sdif_after) ;

     if( summfp != NULL )
       fprintf(summfp,"RMS[%d] = %g %g   ITER = %d/%d\n",
               kim , sdif_before , sdif_after , abas.num_iter , abas.max_iter ) ;

     /** save output parameters for later **/

     for( kpar=0 ; kpar < abas.nparam ; kpar++ ) {
       parsave[kpar][kim] = abas.param[kpar].val_out ;  /* 04 Jan 2005 */
      }
     /** convert output image from float to whatever **/

     if( !null_output ){
       switch( DSET_BRICK_TYPE(outset,kim) ){

         default:
           ERROR_message("Can't store bricks of type %s\n",
                         MRI_TYPE_name[DSET_BRICK_TYPE(inset,kim)] ) ;
           /* fall thru on purpose */

         case MRI_float:
           EDIT_substitute_brick( outset, kim, MRI_float, MRI_FLOAT_PTR(tim) );
           mri_fix_data_pointer( NULL , tim ) ; mri_free( tim ) ;
         break ;

         case MRI_short:{
           double fac = DSET_BRICK_FACTOR(inset,kim) ;
           fac = (fac >  0.0) ? 1.0/fac : 1.0 ;
           fim = mri_to_short(fac,tim) ; mri_free( tim ) ;
           EDIT_substitute_brick( outset, kim, MRI_short, MRI_SHORT_PTR(fim) );
           fac = (fac != 1.0) ? 1.0/fac : 0.0 ;
           EDIT_BRICK_FACTOR( outset , kim , fac ) ;
           mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         }
         break ;

         case MRI_byte:
           vp = mri_min(tim) ;
           if( vp < 0.0f ){
             WARNING_message(
              "output sub-brick #%d is byte, but has negative values\n",
              kim ) ;
           }
           fim = mri_to_byte(tim) ; mri_free( tim ) ;
           EDIT_substitute_brick( outset, kim, MRI_byte, MRI_BYTE_PTR(fim) ) ;
           mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         break ;
       }
     } else {
       mri_free(tim) ; tim = NULL ;  /* 25 Jul 2007 */
     }

   } /* end of loop over input sub-bricks */
   DSET_unload( inset ) ;

   if( summfp != NULL && summfp != stdout ){ fclose(summfp); summfp = NULL; }

   /*===== hard work is done =====*/

   /* mri_warp3D_align_cleanup( &abas ) ; ZSS: This one's moved below, just in case it messes with abas which is still in use. */

   /*-- 06 Jul 2005:
        write the affine transform matrices into the output header --*/

   THD_set_string_atr( outset->dblk , "WARPDRIVE_INPUT_IDCODE" ,
                                      inset->idcode.str         ) ;
   THD_set_string_atr( outset->dblk , "WARPDRIVE_INPUT_NAME" ,
                                      DSET_HEADNAME(inset)      ) ;
   if( base_idc != NULL )
     THD_set_string_atr( outset->dblk , "WARPDRIVE_BASE_IDCODE" , base_idc ) ;
   if( wt_idc != NULL )
     THD_set_string_atr( outset->dblk , "WARPDRIVE_WEIGHT_IDCODE" , wt_idc ) ;

   if( WARPDRIVE_IS_AFFINE(warpdrive_code) ){  /* can't do bilinear here */
     float matar[12] ; char anam[64] ;

     for( kpar=0 ; kpar < 12 ; kpar++ ) parvec[kpar] = 0.0 ;

     if( matrix_save_1D != NULL ){
       msfp = fopen(matrix_save_1D,"w") ;
       fprintf(msfp,
               "# 3dWarpDrive matrices (DICOM-to-DICOM, row-by-row):\n") ;
     }

     for( kim=0 ; kim < nvals ; kim++ ){
       for( kpar=0 ; kpar < abas.nparam ; kpar++ )  {/* load params */
         parvec[kpar] = parsave[kpar][kim] ;
       }
       parset_affine() ;                            /* compute matrices */

       UNLOAD_MAT(mv_for.mm,matar[0],matar[1],matar[2],
                            matar[4],matar[5],matar[6],
                            matar[8],matar[9],matar[10] ) ;
       UNLOAD_FVEC3(mv_for.vv,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"WARPDRIVE_MATVEC_FOR_%06d",kim) ;
       THD_set_float_atr( outset->dblk , anam , 12 , matar ) ;

       if( msfp != NULL )                            /* 25 Jul 2007 */
          fprintf(msfp,"%13.6g %13.6g %13.6g %13.6g "
                       "%13.6g %13.6g %13.6g %13.6g "
                       "%13.6g %13.6g %13.6g %13.6g\n" ,
                  matar[0],matar[1],matar[ 2],matar[ 3],
                  matar[4],matar[5],matar[ 6],matar[ 7],
                  matar[8],matar[9],matar[10],matar[11] ) ;

       UNLOAD_MAT(mv_inv.mm,matar[0],matar[1],matar[2],
                            matar[4],matar[5],matar[6],
                            matar[8],matar[9],matar[10] ) ;
       UNLOAD_FVEC3(mv_inv.vv,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"WARPDRIVE_MATVEC_INV_%06d",kim) ;
       THD_set_float_atr( outset->dblk , anam , 12 , matar ) ;

       /* redo with just the shift+rotation parameters [22 Aug 2006] */

       for( kpar=0 ; kpar < 3           ; kpar++ ) parvec[kpar] = 0.0 ;
       for( kpar=6 ; kpar < abas.nparam ; kpar++ ) parvec[kpar] = 0.0 ;
       parset_affine() ;

       UNLOAD_MAT(mv_for.mm,matar[0],matar[1],matar[2],
                            matar[4],matar[5],matar[6],
                            matar[8],matar[9],matar[10] ) ;
       UNLOAD_FVEC3(mv_for.vv,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"WARPDRIVE_ROTMAT_FOR_%06d",kim) ;
       THD_set_float_atr( outset->dblk , anam , 12 , matar ) ;

       UNLOAD_MAT(mv_inv.mm,matar[0],matar[1],matar[2],
                            matar[4],matar[5],matar[6],
                            matar[8],matar[9],matar[10] ) ;
       UNLOAD_FVEC3(mv_inv.vv,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"WARPDRIVE_ROTMAT_INV_%06d",kim) ;
       THD_set_float_atr( outset->dblk , anam , 12 , matar ) ;
     }
     if( msfp != NULL ) fclose(msfp) ;
   }

   /*-- write the results to disk for all of history to see --*/

   if( !null_output ){
     DSET_write( outset ) ;  DSET_unload( outset ) ;
     if( abas.verb ) WROTE_DSET(outset) ;
   } else {
     INFO_message("-prefix NULL was given ==> no output dataset written") ;
   }

   if( W_1Dfile != NULL ){
     FILE *fp ;
     if( abas.verb ) INFO_message("Writing 1Dfile: %s\n",W_1Dfile) ;
     if( THD_is_file(W_1Dfile) )
       WARNING_message("Overwriting file %s\n",W_1Dfile) ;

     fp = fopen( W_1Dfile , "w" ) ;
     if( fp != NULL ){

       fprintf(fp,"#") ;
       for( kim=0 ; kim < argc ; kim++ ) fprintf(fp," %s",argv[kim]) ;
       fprintf(fp,"\n") ;

       fprintf(fp,"#") ;
       for( kpar=0 ; kpar < abas.nparam ; kpar++ )
         fprintf(fp," %-13.13s",abas.param[kpar].name) ;
       fprintf(fp,"\n") ;

       fprintf(fp,"#") ;
       for( kpar=0 ; kpar < abas.nparam ; kpar++ )
         fprintf(fp," -------------") ;
       fprintf(fp,"\n") ;

       for( kim=0 ; kim < nvals ; kim++ ){
         for( kpar=0 ; kpar < abas.nparam ; kpar++ )
           fprintf(fp," %13.6g",parsave[kpar][kim]) ;
         fprintf(fp,"\n") ;
       }
       fclose(fp) ;
     }

   }

   if( abas.verb ){
     double tt = (NI_clock_time()-ctstart)*0.001 ;
     INFO_message("Total elapsed time = %.2f s\n",tt) ;
   }

   mri_warp3D_align_cleanup( &abas ) ;

   exit(0) ;
}

/*==========================================================================*/
/*======= Code to find best initial shift_rotate parameters, crudely =======*/
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

static float voldif( int nx, int ny, int nz, float *b,
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

static float get_best_shift( int nx, int ny, int nz,
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
       sum = voldif( nx,ny,nz , b , dx,dy,dz , v , edge , w ) ;
       if( sum < bsum ){ bsum = sum; bdx = dx; bdy = dy; bdz = dz; }
   }}}

   *dxp = bdx ; *dyp = bdy ; *dzp = bdz ; return bsum ;
}

/*----------------------------------------------------------------------*/
/* Find best angles AND best shifts all at once't.  Will only be
   called if VL_coarse_rot is nonzero.
------------------------------------------------------------------------*/

#define DANGLE 9.0f
#define NROLL  1
#define NPITCH 2
#define NYAW   1

static float get_best_shiftrot( MRI_warp3D_align_basis *bas ,
                                MRI_IMAGE *base , MRI_IMAGE *vol ,
                                float *roll , float *pitch , float *yaw ,
                                int   *dxp  , int   *dyp   , int   *dzp  )
{
   int ii,jj,kk ;
   float r,p,y , br=0.0f , bp=0.0f , by=0.0f ;
   float bsum=1.e+38 , sum ;
   MRI_IMAGE *tim , *wim=NULL , *bim, *vim , *msk ;
   float *bar , *tar , *var , dif , *www=NULL , wtop , param[12] ;
   byte *mmm ;
   int nx,ny,nz , sx,sy,sz , bsx=0,bsy=0,bsz=0 , nxy,nxyz , subd=0 ;
   THD_vecmat ijk_to_xyz_save , xyz_to_ijk_save ;
   THD_mat33  mat ;

   *roll = *pitch = *yaw = 0.0f ;   /* in case of sudden death */
   *dxp  = *dyp   = *dzp = 0    ;

   if( bas->nparam < 6 ) return 0.0f ;   /* no rotations allowed? */

   for( ii=0 ; ii < 12 ; ii++ ) param[ii] = 0.0f ;
   param[6] = param[7] = param[8] = 1.0f ;

   nx = base->nx ; ny = base->ny ; nz = base->nz ; nxy = nx*ny ;

   /** if image volume is big enough, sub-sample by 2 for speedup **/

   bim = base ; vim = vol ;

#if 1
   if( nx >= 120 && ny >= 120 && nz >= 120 ){
     int hnx=(nx+1)/2 , hny=(ny+1)/2 , hnz=(nz+1)/2 , hnxy=hnx*hny ;

     if( bas->verb ) fprintf(stderr,"x") ;

     /* copy and blur base, then subsample it into new image bim */

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

     /* scale up the index-to-coord transformation */

     ijk_to_xyz_save = ijk_to_xyz ;
     xyz_to_ijk_save = xyz_to_ijk ;
     mat             = ijk_to_xyz.mm ;
     ijk_to_xyz.mm   = MAT_SCALAR( mat , 2.0f ) ;
     xyz_to_ijk      = INV_VECMAT( ijk_to_xyz ) ;
   }
#endif

   /* make a weighting image (blurred & masked copy of base) */

   if( bas->verb ) fprintf(stderr,"w") ;

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
   msk = mri_empty_conforming( bim , MRI_byte ) ;
   mri_fix_data_pointer( mmm , msk ) ;

   if( bas->verb )
     fprintf(stderr,"[%.1f%%]" , (100.0*THD_countmask(nxyz,mmm))/nxyz ) ;

   /* prepare to rotate and shift the night away */

   mri_warp3D_set_womask( msk ) ;
   mri_warp3D_method( MRI_NN ) ;
   bar = MRI_FLOAT_PTR(bim) ;

   for( kk=-NROLL  ; kk <= NROLL  ; kk++ ){
    for( jj=-NPITCH ; jj <= NPITCH ; jj++ ){
     for( ii=-NYAW   ; ii <= NYAW   ; ii++ ){
       r = kk*DANGLE ; p = jj*DANGLE ; y = ii*DANGLE ;

       if( r == 0.0f && p == 0.0f && y == 0.0f ){  /* no rotate */
         tim = vim ;
       } else {                                    /* rotate vim */
         param[3] = r ; param[4] = p ; param[5] = y ;
         bas->vwset( 12 , param ) ;
         tim = mri_warp3D( vim , 0,0,0 , bas->vwfor ) ;
       }
       tar = MRI_FLOAT_PTR(tim) ;
       sum = get_best_shift( nx,ny,nz, bar, tar, www, &sx,&sy,&sz ) ;
       if( bas->verb ) fprintf(stderr,"%s",(sum<bsum)?"*":".") ;
       if( sum < bsum ){
         br=r ; bp=p ; by=y ; bsx=sx ; bsy=sy; bsz=sz ; bsum=sum ;
       }
       if( tim != vim ) mri_free(tim) ;
   }}}

   /* cleanup and exeunt */

   mri_free(wim) ; mri_free(msk) ; mri_warp3D_set_womask(NULL) ;
   if( subd ){
     mri_free(bim); mri_free(vim);
     bsx *= 2; bsy *= 2; bsz *= 2; VL_coarse_del *=2;
     ijk_to_xyz = ijk_to_xyz_save ;
     xyz_to_ijk = xyz_to_ijk_save ;
   }

   *roll = br ; *pitch = bp ; *yaw = by ;
   *dxp  = bsx; *dyp   = bsy; *dzp = bsz ;

   return bsum ;
}
