/*------------------------------------------------------------------------
   ***  This program does something, but nobody is quite sure what.  ***
  ***  But what it does is very important; nobody doubts that, either. ***
--------------------------------------------------------------------------*/

#include "mrilib.h"

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

#define MATORDER_SDU  1
#define MATORDER_SUD  2
#define MATORDER_DSU  3
#define MATORDER_DUS  4
#define MATORDER_USD  5
#define MATORDER_UDS  6

static int matorder = MATORDER_SDU ;
static int dcode    = DELTA_AFTER  ;  /* cf. 3ddata.h */

void parset_affine(void)
{
   THD_mat33 ss,dd,uu,aa,bb ;
   THD_fvec3 vv ;

#if 0
{ int ii;fprintf(stderr,"\nparset:");
  for(ii=0;ii<12;ii++)fprintf(stderr," %g",parvec[ii]); fprintf(stderr,"\n");}
#endif

   /* rotation */

   uu = rot_matrix( 2, D2R*parvec[3] , 0, D2R*parvec[4] , 1, D2R*parvec[5] ) ;

   /* scaling */

   LOAD_DIAG_MAT( dd , parvec[6] , parvec[7] , parvec[8] ) ;

   /* shear */

   LOAD_MAT( ss , 1.0        , 0.0        , 0.0 ,
                  parvec[9]  , 1.0        , 0.0 ,
                  parvec[10] , parvec[11] , 1.0  ) ;

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
     case DELTA_AFTER:  mv_for.vv = vv ; break ;
     case DELTA_BEFORE: mv_for.vv = MATVEC( mv_for.mm , vv ) ; break ;
   }

   mv_inv = INV_VECMAT( mv_for ) ;

#if 0
DUMP_VECMAT("mv_for",mv_for) ; DUMP_VECMAT("mv_inv",mv_inv) ;
#endif
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset=NULL, *outset=NULL, *baset=NULL, *wtset=NULL ;
   MRI_warp3D_align_basis abas ;
   char *prefix="warpdriven" ;
   int warpdrive_code=-1 , nerr , nx,ny,nz , nopt=1 ;
   float dx,dy,dz , vp ;
   int kim , nvals , kpar , np , nfree ;
   MRI_IMAGE *qim , *tim , *fim ;
   float clip_baset=0.0f , clip_inset=0.0f ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
            "Usage: 3dWarpDrive [options] dataset\n"
            "Warp a dataset to match another one (the base).\n"
            "\n"
            "This program is a generalization of 3dvolreg.  It tries to find\n"
            "a transformation that warps a given dataset to match an input\n"
            "dataset (given by the -base option).  It will be slow.\n"
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
            "-------------\n"
            "Other Options:\n"
            "-------------\n"
            "  -linear   }\n"
            "  -cubic    } = Chooses spatial interpolation method.\n"
            "  -NN       } =   [default = linear; inaccurate but fast]\n"
            "  -quintic  }\n"
            "\n"
            "  -base bbb   = Load dataset 'bbb' as the base to which the\n"
            "                  input dataset will be matched.\n"
            "\n"
            "  -verb       = Print out some information along the way.\n"
            "  -prefix ppp = Sets the prefix of the output dataset.\n"
            "\n"
            "-----------------\n"
            "Technical Options:\n"
            "-----------------\n"
            "  -maxite    m  = Allow up to 'm' iterations for convergence.\n"
            "  -delta     d  = Distance, in voxel size, used to compute\n"
            "                   image derivatives using finite differences.\n"
            "  -weight  wset = Set the weighting applied to each voxel\n"
            "                   proportional to the brick specified here.\n"
            "  -thresh t     = Set the convergence parameter to be 't' voxels\n"
            "                   voxel movement.  [Default=0.03]\n"
            "  -twopass      = Do the parameter estimation in two passes,\n"
            "                   coarse-but-fast first, then fine-but-slow second\n"
            "                   (much like the same option in program 3dvolreg).\n"
            "  -parfix n v   = Fix the n'th parameter of the warp model to\n"
            "                   the value 'v'.  More than one -parfix option\n"
            "                   can be used, to fix multiple parameters.\n"
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
            "  -USD or -UDS }=   S = lower triangular shear (params #10-12)\n"
            "                    D = diagonal scaling matrix (params #7-9)\n"
            "                    U = rotation matrix (params #4-6)\n"
            "                  Default order is '-SDU', which means that\n"
            "                  the U matrix is applied first, then the\n"
            "                  D matrix, then the S matrix.\n"
            "\n"
            "The matrices are specified in DICOM-ordered (x,y,z) coordinates as:\n"
            "\n"
            "  [U] = [Rotate_y(param#6)] [Rotate_x(param#5)] [Rotate_z(param #4)]\n"
            "        (angles are in degrees)\n"
            "\n"
            "  [D] = diag( param#7 , param#8 , param#9 )\n"
            "\n"
            "        [    1        0     0 ]\n"
            "  [S] = [ param#10    1     0 ]\n"
            "        [ param#11 param#12 1 ]\n"
            "\n"
            "  -ashift OR   }= Apply the shift parameters (#1-3) after OR\n"
            "  -bshift      }= before the matrix transformation.\n"
            "\n"
            " For example, the default (-SDU/-ashift) has the transformation\n"
            " specified as [x]_warp = [S] [D] [U] [x]_in + [shift].\n"
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
     exit(0) ;
   }

   /*-- startup mechanics --*/

   mainENTRY("3dWarpDrive main"); machdep(); AFNI_logger("3dWarpDrive",argc,argv);

   abas.nparam     = 0 ;
   abas.param      = NULL ;
   abas.scale_init = 1.0f ;
   abas.delfac     = 1.0f ;
   abas.tolfac     = 0.03f ;
   abas.twoblur    = 0.0f ;
   abas.regmode    = MRI_LINEAR ;
   abas.verb       = 0 ;
   abas.max_iter   = 0 ;
   abas.wtproc     = 1 ;
   abas.imbase     = NULL ;
   abas.imwt       = NULL ;
   abas.vwfor      = ijk_warp_for ;
   abas.vwinv      = ijk_warp_inv ;
   abas.vwset      = load_parvec ;

   abas.xedge = abas.yedge = abas.zedge = -1 ;
   abas.imww  = abas.imap  = abas.imps  = abas.imsk = NULL ;

   nparfix = 0 ;

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     /*-----*/

     if( strcmp(argv[nopt],"-twopass") == 0 ){
       fprintf(stderr,"** WARNING: -twopass not implemented yet!\n") ;
       abas.twoblur = 3.0f ; nopt++ ; continue ;
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

     /*-----*/

     if( strcmp(argv[nopt],"-parfix") == 0 ){
       int np , ip ; float vp ;
       if( ++nopt >= argc-1 ){
         fprintf(stderr,"** ERROR: need 2 parameters afer -parfix!\n"); exit(1);
       }
       np = strtol( argv[nopt] , NULL , 10 ) ; nopt++ ;
       vp = strtod( argv[nopt] , NULL ) ;
       if( np <= 0 || np > MAXPAR ){
         fprintf(stderr,"** ERROR: param #%d after -parfix is illegal!\n",np) ;
         exit(1) ;
       }
       for( ip=0 ; ip < nparfix ; ip++ ){
         if( parfix[ip].np == np ){
           fprintf(stderr,
                   "++ WARNING: ignoring later -parfix option for param #%d\n" ,
                   ip ) ;
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

     if( strcmp(argv[nopt],"-bilinear_general") == 0 ){
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
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -prefix!\n"); exit(1);
       }
       if( !THD_filename_ok(argv[nopt]) ){
         fprintf(stderr,"** ERROR: -prefix argument is invalid!\n"); exit(1);
       }
       prefix = argv[nopt] ; nopt++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[nopt],"-verbose",5) == 0 ){
       abas.verb++ ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-base") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -base!\n"); exit(1);
       }
       baset = THD_open_dataset( argv[nopt] ) ;
       if( baset == NULL ){
         fprintf(stderr,"** ERROR: can't open -base dataset %s\n",argv[nopt]);
         exit(1) ;
       }
       if( DSET_NVALS(baset) > 1 ){
         fprintf(stderr,
           "++ WARNING: -base dataset %s has %d sub-bricks; will only use #0\n",
           argv[nopt],DSET_NVALS(baset) ) ;
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-weight") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -weight!\n"); exit(1);
       }
       wtset = THD_open_dataset( argv[nopt] ) ;
       if( wtset == NULL ){
         fprintf(stderr,"** ERROR: can't open -weight dataset %s\n",argv[nopt]);
         exit(1) ;
       }
       if( DSET_NVALS(wtset) > 1 ){
         fprintf(stderr,
           "++ WARNING: -weight dataset %s has %d sub-bricks; will only use #0\n",
           argv[nopt],DSET_NVALS(wtset) ) ;
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-input") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -input!\n"); exit(1);
       }
       inset = THD_open_dataset( argv[nopt] ) ;
       if( inset == NULL ){
         fprintf(stderr,"** ERROR: can't open -input dataset %s\n",argv[nopt]);
         exit(1) ;
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-maxite") == 0 ){
       int ival ;
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -maxite!\n"); exit(1);
       }
       ival = strtol( argv[nopt] , NULL , 10 ) ;
       if( ival > 1 ) abas.max_iter = ival ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-delta") == 0 ){
       float val ;
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -delta!\n"); exit(1);
       }
       val = strtod( argv[nopt] , NULL ) ;
       if( val > 0.0499 && val < 49.99 ) abas.delfac = val ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-thresh") == 0 ){
       float val ;
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -thresh!\n"); exit(1);
       }
       val = strtod( argv[nopt] , NULL ) ;
       if( val > 0.001 && val < 3.01 ) abas.tolfac = val ;
       nopt++ ; continue ;
     }

     /*-----*/

     fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
     exit(1) ;

   } /*--- end of loop over command line options ---*/

   if( abas.verb ) fprintf(stderr,"++ Checking inputs\n") ;

   /*-- parameterize the warp model --*/

   /*! Add a parameter to the warp3D model.
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
     fprintf(stderr,"** ERROR: need a transform-specifying option!\n");
     nerr++ ;
   } else if( warpdrive_code >= WARPDRIVE_SHIFT &&
              warpdrive_code <= WARPDRIVE_AFFINE  ){

       ADDPAR( "x-shift" , -100.0 , 100.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-shift" , -100.0 , 100.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z-shift" , -100.0 , 100.0 , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "z-angle" , -180.0 , 180.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "x-angle" , -180.0 , 180.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-angle" , -180.0 , 180.0 , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "x-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;

       ADDPAR( "y/x-shear" , -0.3333 , 0.3333 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z/x-shear" , -0.3333 , 0.3333 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z/y-shear" , -0.3333 , 0.3333 , 0.0 , 0.0 , 0.0 ) ;

       for( kpar=0 ; kpar < 12 ; kpar++ )
         parvec[kpar] = abas.param[kpar].ident ;

       if( warpdrive_code == WARPDRIVE_SHIFT ){
         warp_parset = parset_shift ;
         warp_for    = warper_shift_for ;
         warp_inv    = warper_shift_inv ;
       } else {
         warp_parset = parset_affine ;
         warp_for    = warper_affine_for ;
         warp_inv    = warper_affine_inv ;
       }

       switch( warpdrive_code ){
         case WARPDRIVE_SHIFT:  abas.nparam =  3 ; break ;
         case WARPDRIVE_ROTATE: abas.nparam =  6 ; break ;
         case WARPDRIVE_SCALE:  abas.nparam =  9 ; break ;
         case WARPDRIVE_AFFINE: abas.nparam = 12 ; break ;
       }
   } else {
     fprintf(stderr,"** ERROR: unimplemented transform model!\n") ;
     nerr++ ;
   }

   nfree = abas.nparam ;
   for( kpar=0 ; kpar < nparfix ; kpar++ ){
     np = parfix[kpar].np - 1 ; vp = parfix[kpar].vp ;
     if( np >= 0 && np < abas.nparam ){
       if( vp >= abas.param[np].min && vp <= abas.param[np].max ){
         abas.param[np].fixed     = 1  ;
         abas.param[np].val_fixed = vp ;
         nfree -- ;
       } else {        /* bad value */
         fprintf(stderr,
                 "** ERROR: -parfix for param #%d has illegal value!\n",np+1) ;
         nerr++ ;
       }
     } else {          /* bad index */
       fprintf(stderr,
               "++ WARNING: -parfix for param #%d is out of range 1..%d\n",
               np+1 , abas.nparam+1 ) ;
     }
   }
   if( nfree <= 0 ){
     fprintf(stderr,"** ERROR: no free parameters in transform model!\n") ;
     nerr++ ;
   }
   if( abas.max_iter <= 0 ) abas.max_iter = 9*nfree+5 ;

   /*-- other checks for good set of inputs --*/

   if( baset == NULL ){
     fprintf(stderr,"** ERROR: need to specify a base dataset!\n") ;
     nerr++ ;
   }

   /*-- 1 remaining argument should be a dataset --*/

   if( inset == NULL && nopt != argc-1 ){
     fprintf(stderr,"** ERROR: Command line should have exactly 1 dataset!\n"
                    "**        Whereas there seems to be %d of them!\n",
             argc-nopt ) ;
     exit(1) ;
   }

   /*-- input dataset header --*/

   if( inset == NULL ){
     inset = THD_open_dataset( argv[nopt] ) ;
     if( !ISVALID_DSET(inset) ){
       fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[nopt]); exit(1);
     }
   }

   if( nerr ) exit(1) ;

   /*-- more checks --*/

   nx = DSET_NX(inset) ; ny = DSET_NY(inset) ; nz = DSET_NZ(inset) ;
   dx = DSET_DX(inset) ; dy = DSET_DY(inset) ; dz = DSET_DZ(inset) ;

   if( DSET_NX(baset) != nx || DSET_NY(baset) != ny || DSET_NZ(baset) != nz ){
     fprintf(stderr,"** ERROR: base and input datasets don't match!\n") ;
     nerr++ ;
   }

   if( wtset != NULL &&
      (DSET_NX(wtset) != nx || DSET_NY(wtset) != ny || DSET_NZ(wtset) != nz) ){
     fprintf(stderr,"** ERROR: weight and input datasets don't match!\n") ;
     nerr++ ;
   }

   if( abas.verb ) fprintf(stderr,"++ Loading datasets\n") ;

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
     fprintf(stderr,"** ERROR: can't load input dataset into memory!\n") ;
     nerr++ ;
   } else {
     nvals = DSET_NVALS(inset) ;
     if( nvals == 1 ){
       clip_inset = THD_cliplevel( DSET_BRICK(inset,0) , 0.0 ) ;
       if( DSET_BRICK_FACTOR(inset,0) > 0.0f )
         clip_inset *= DSET_BRICK_FACTOR(inset,0) ;
     } else {
       qim = THD_median_brick( inset ) ;
       clip_inset = THD_cliplevel( qim , 0.0 ) ;
       mri_free(qim) ;
     }
   }

   DSET_load(baset) ;
   if( !DSET_LOADED(baset) ){
     fprintf(stderr,"** ERROR: can't load base dataset into memory!\n") ;
     nerr++ ;
   } else {
     clip_baset  = THD_cliplevel( DSET_BRICK(baset,0) , 0.0 ) ;
     abas.imbase = mri_to_float( DSET_BRICK(baset,0) ) ;
     DSET_delete(baset) ; baset = NULL ;
   }

   if( wtset != NULL ){
     DSET_load(wtset) ;
     if( !DSET_LOADED(wtset) ){
       fprintf(stderr,"** ERROR: can't load weight dataset into memory!\n") ;
       nerr++ ;
     } else {
       abas.imwt = mri_to_float( DSET_BRICK(wtset,0) ) ;
       DSET_delete(wtset) ; wtset = NULL ;
     }
   }

   if( nerr > 0 ){
     fprintf(stderr,"** 3dWarpDrive exits due to fatal errors!\n"); exit(1);
   }

   /*-- set up (x,y,z) <-> (i,j,k) transformations ---*/

   { THD_vecmat ijk_to_inset_xyz , xyz_to_dicom ;

     LOAD_DIAG_MAT( ijk_to_inset_xyz.mm ,
                    inset->daxes->xxdel ,
                    inset->daxes->yydel , inset->daxes->zzdel );

     /* define (x,y,z)=(0,0,0) at mid-point of dataset 3D array */

     LOAD_FVEC3   ( ijk_to_inset_xyz.vv ,
                    -0.5*(nx-1) , -0.5*(ny-1) , -0.5*(nz-1) ) ;

     xyz_to_dicom.mm = inset->daxes->to_dicomm ;
     LOAD_FVEC3( xyz_to_dicom.vv , 0.0,0.0,0.0 ) ;

     ijk_to_xyz = MUL_VECMAT( xyz_to_dicom , ijk_to_inset_xyz ) ;
     xyz_to_ijk = INV_VECMAT( ijk_to_xyz ) ;
   }

   /*-- make the shell of the new dataset --*/

   if( clip_baset > 0.0f && clip_inset > 0.0f ){
     float fac = clip_inset / clip_baset ;
     if( fac > 0.01 && fac < 100.0 ){
       abas.scale_init = fac ;
       if( abas.verb ) fprintf(stderr,"++ Scale factor set to %.2f/%.2f=%.2g\n",
                               clip_baset , clip_inset , fac ) ;
     }
   }

   if( abas.verb ) fprintf(stderr,"++ Creating empty output dataset\n") ;

   outset = EDIT_empty_copy( inset ) ;

   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarpDrive" , argc,argv , outset ) ;

   /*===== do the hard work =====*/

   if( abas.verb ) fprintf(stderr,"++ Beginning alignment setup\n") ;

   mri_warp3D_align_setup( &abas ) ;

   if( abas.verb ) fprintf(stderr,"++ Beginning alignment loop\n") ;
   for( kim=0 ; kim < nvals ; kim++ ){
     for( kpar=0 ; kpar < abas.nparam ; kpar++ ){
       if( abas.param[kpar].fixed )
         abas.param[kpar].val_init = abas.param[kpar].val_fixed ;
       else
         abas.param[kpar].val_init = abas.param[kpar].ident ;
     }

     qim = mri_scale_to_float( DSET_BRICK_FACTOR(inset,kim) ,
                               DSET_BRICK(inset,kim)         ) ;
     tim = mri_warp3d_align_one( &abas , qim ) ;
     mri_free( qim ) ; DSET_unload_one( inset , kim ) ;
     switch( DSET_BRICK_TYPE(inset,kim) ){

         default:
           fprintf(stderr,"\n** ERROR: Can't store bricks of type %s\n",
                    MRI_TYPE_name[DSET_BRICK_TYPE(inset,kim)] ) ;
           /* fall thru on purpose */

         case MRI_float:
           EDIT_substitute_brick( outset, kim, MRI_float, MRI_FLOAT_PTR(tim) );
           mri_fix_data_pointer( NULL , tim ) ; mri_free( tim ) ;
         break ;

         case MRI_short:
           fim = mri_to_short(1.0,tim) ; mri_free( tim ) ;
           EDIT_substitute_brick( outset, kim, MRI_short, MRI_SHORT_PTR(fim) );
           mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         break ;

         case MRI_byte:
           vp = mri_min(tim) ;
           if( vp < 0.0f ){
             fprintf(stderr,
              "++ WARNING: output sub-brick #%d is byte, but has negative values\n",
              kim ) ;
           }
           fim = mri_to_byte(tim) ; mri_free( tim ) ;
           EDIT_substitute_brick( outset, kim, MRI_byte, MRI_BYTE_PTR(fim) ) ;
           mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         break ;
      }
   }

   /*===== hard work is done =====*/

   /*-- write the results to disk for all of history to see --*/

   if( abas.verb )
     fprintf(stderr,"++ Writing dataset: %s\n",DSET_FILECODE(outset));
   DSET_write( outset ) ;
   exit(0) ;
}
