/*------------------------------------------------------------------------
   ***  This program does something, but nobody is quite sure what.  ***
  ***  But what it does is very important, nobody doubts that, either. ***
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

#define WARPDRIVE_ROTATE   2
#define WARPDRIVE_SCALE    3
#define WARPDRIVE_AFFINE   4

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

#define D2R (PI/180.0)

void parset_rotate(void)
{
   mv_for.mm = rot_matrix( 2,D2R*parvec[3] , 0,D2R*parvec[4] , 1,D2R*parvec[5] ) ;
   LOAD_FVEC3( mv_for.vv , parvec[0] , parvec[1] , parvec[2] ) ;
   mv_inv = INV_VECMAT( mv_for ) ;
}

/*--------------------------------------------------------------------------*/

void parset_rotate_scale(void)
{
   THD_mat33 q,p ;
   q = rot_matrix( 2,D2R*parvec[3] , 0,D2R*parvec[4] , 1,D2R*parvec[5] ) ;
   LOAD_DIAG_MAT( p , parvec[6] , parvec[7] , parvec[8] ) ;
   mv_for.mm = MAT_MUL( p , q ) ;
   LOAD_FVEC3( mv_for.vv , parvec[0] , parvec[1] , parvec[2] ) ;
   mv_inv = INV_VECMAT( mv_for ) ;
}

/*--------------------------------------------------------------------------*/

void parset_affine(void)
{
   LOAD_FVEC3( mv_for.vv , parvec[0] , parvec[1] , parvec[2] ) ;
   LOAD_MAT( mv_for.mm , parvec[3] , parvec[ 4] , parvec[ 5] ,
                         parvec[6] , parvec[ 7] , parvec[ 8] ,
                         parvec[9] , parvec[10] , parvec[11]  ) ;
   mv_inv = INV_VECMAT( mv_for ) ;
}

/*--------------------------------------------------------------------------*/
#define WARPDRIVE_BILINEAR 5
/*--------------------------------------------------------------------------*/

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
            "  -NN       } =   [default = linear]\n"
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
            "  -maxite    m  = Allow up to 'm' iterations for convergence\n"
            "  -delta     d  = Distance, in voxel size, used to compute\n"
            "                   image derivatives using finite differences\n"
            "  -weight  wset = Set the weighting applied to each voxel\n"
            "                   proportional to the brick specified here\n"
            "  -parfix n v   = Fix the n'th parameter of the warp model to\n"
            "                   the value 'v'.  More than one -parfix option\n"
            "                   can be used.\n"
            "\n"
           ) ;
     exit(0) ;
   }

   /*-- startup mechanics --*/

   mainENTRY("3dWarpDrive main"); machdep(); AFNI_logger("3dWarpDrive",argc,argv);

   abas.nparam     = 0 ;
   abas.param      = NULL ;
   abas.scale_init = 1.0 ;
   abas.delfac     = 1.0 ;
   abas.regmode    = MRI_LINEAR ;
   abas.verb       = 0 ;
   abas.max_iter   = 19 ;
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
                   "++ WARNING: multiple -parfix options for param #%d\n",np) ;
           break ;
         }
       }
       if( ip == nparfix ) nparfix++ ;
       parfix[ip].np = np ; parfix[ip].vp = vp ;
       nopt++ ; continue ;
     }

     /*-----*/

  /*! Add a parameter to a warp3D model.
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
     abas.param[p].ident = abas.param[p].val_init = (dd) ;      \
     strcpy( abas.param[p].name , (nm) ) ;                      \
     abas.param[p].fixed = 0 ;                                  \
     abas.nparam = p+1 ;                                        \
 } while(0)

     /*-----*/

     if( strcmp(argv[nopt],"-shift_only") == 0 ){
       warp_parset = parset_shift ;
       warp_for    = warper_shift_for ;
       warp_inv    = warper_shift_inv ;
       warpdrive_code = WARPDRIVE_SHIFT ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-shift_rotate") == 0 ){
       warp_parset = parset_rotate ;
       warp_for    = warper_affine_for ;
       warp_inv    = warper_affine_inv ;

       ADDPAR( "x-shift" , -100.0 , 100.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-shift" , -100.0 , 100.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "z-shift" , -100.0 , 100.0 , 0.0 , 0.0 , 0.0 ) ;

       ADDPAR( "z-angle" , -180.0 , 180.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "x-angle" , -180.0 , 180.0 , 0.0 , 0.0 , 0.0 ) ;
       ADDPAR( "y-angle" , -180.0 , 180.0 , 0.0 , 0.0 , 0.0 ) ;

       warpdrive_code = WARPDRIVE_ROTATE ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-shift_rotate_scale") == 0 ){
       warp_parset = parset_rotate_scale ;
       warp_for    = warper_affine_for ;
       warp_inv    = warper_affine_inv ;
       warpdrive_code = WARPDRIVE_SCALE ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-affine_general") == 0 ){
       warp_parset = parset_affine ;
       warp_for    = warper_affine_for ;
       warp_inv    = warper_affine_inv ;
       warpdrive_code = WARPDRIVE_AFFINE ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-bilinear_general") == 0 ){
#if 1
       fprintf(stderr,"** ERROR: -bilinear_general not implemented yet!\n") ;
       exit(1) ;
#else
       warpdrive_code = WARPDRIVE_BILINEAR ; nopt++ ; continue ;
#endif
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

     fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
     exit(1) ;

   } /*--- end of loop over command line options ---*/

   /*-- check for good set of inputs --*/

   if( abas.verb ) fprintf(stderr,"++ Checking inputs\n") ;

   nfree = abas.nparam ;
   for( kpar=0 ; kpar < nparfix ; kpar++ ){
     np = parfix[kpar].np - 1 ; vp = parfix[kpar].vp ;
     if( np >= 0 && np < abas.nparam ){
       abas.param[np].fixed     = 1  ;
       abas.param[np].val_fixed = vp ;
       nfree -- ;
     } else {
       fprintf(stderr,
               "++ WARNING: -parfix for param #%d is out of range 1..%d\n",
               np+1 , abas.nparam+1 ) ;
     }
   }
   if( nfree <= 0 ){
     fprintf(stderr,"** ERROR: no free parameters left -- too much -parfix!\n") ;
     nerr++ ;
   }

   nerr = 0 ;
   if( abs.nparam < 1 || warpdrive_code <= 0 ){
     fprintf(stderr,"** ERROR: need to input a transform-specifying option!\n");
     nerr++ ;
   }

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
   }

   DSET_load(baset) ;
   if( !DSET_LOADED(baset) ){
     fprintf(stderr,"** ERROR: can't load base dataset into memory!\n") ;
     nerr++ ;
   } else {
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

   if( nerr > 0 ) exit(1) ;

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

   if( abas.verb ) fprintf(stderr,"++ Creating empty output dataset\n") ;

   outset = EDIT_empty_copy( inset ) ;

   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarpDrive" , argc,argv , outset ) ;

   /*===== do the hard work =====*/

   if( abas.verb ) fprintf(stderr,"++ Beginning alignment setup\n") ;

   mri_warp3D_align_setup( &abas ) ;

   if( abas.verb ) fprintf(stderr,"++ Beginning alignment iterations\n") ;
   nvals = DSET_NVALS(inset) ;
   for( kim=0 ; kim < nvals ; kim++ ){
     for( kpar=0 ; kpar < abas.nparam ; kpar++ ){
       if( abas.param[kpar].fixed )
         abas.param[kpar].val_init = abas.param[kpar].val_fixed ;
       else
         abas.param[kpar].val_init = abas.param[kpar].ident ;
     }

     qim = DSET_BRICK( inset , kim ) ;
     tim = mri_warp3d_align_one( &abas , qim ) ;
     switch( qim->kind ){

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
            fim = mri_to_byte(tim) ; mri_free( tim ) ;
            EDIT_substitute_brick( outset, kim, MRI_byte, MRI_BYTE_PTR(fim) ) ;
            mri_fix_data_pointer( NULL , fim ) ; mri_free( fim ) ;
         break ;

         default:
            fprintf(stderr,"\n** ERROR: Can't align bricks of type %s\n",
                    MRI_TYPE_name[qim->kind] ) ;
            exit(1) ;
      }
      DSET_unload_one( inset , kim ) ;      /* don't need this anymore */
   }

   /*===== hard work is done =====*/

   /*-- write the results to disk for all of history to see --*/

   if( abas.verb ) fprintf(stderr,"\n++ Writing dataset\n") ;
   DSET_write( outset ) ;
   exit(0) ;
}
