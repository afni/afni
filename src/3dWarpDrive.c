/*------------------------------------------------------------------------
     ***  This program does something, but nobody is sure what.  ***
--------------------------------------------------------------------------*/

#include "mrilib.h"

/*--------------------------------------------------------------------------*/

#define MATVEC_FOR 1
#define MATVEC_BAC 2

static THD_vecmat dicom_in2out , dicom_out2in ;  /* coordinate warps */

/*--------------------------------------------------------------------------*/

void warp_dicom_in2out( float  xin , float  yin , float  zin ,
                        float *xout, float *yout, float *zout )
{
   THD_fvec3 xxx ;
   LOAD_FVEC3( xxx , xin,yin,zin ) ;
   xxx = VECMAT_VEC( dicom_in2out , xxx ) ;
   UNLOAD_FVEC3( xxx , *xout , *yout , *zout ) ;
}

/*--------------------------------------------------------------------------*/

void warp_dicom_out2in( float  xin , float  yin , float  zin ,
                        float *xout, float *yout, float *zout )
{
   THD_fvec3 xxx ;
   LOAD_FVEC3( xxx , xin,yin,zin ) ;
   xxx = VECMAT_VEC( dicom_out2in , xxx ) ;
   UNLOAD_FVEC3( xxx , *xout , *yout , *zout ) ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset , *outset=NULL , *baset ;
   MRI_warp3D_align_basis abas ;
   char *prefix="warpdriven" ;

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
            "  -shift_rotate       =  6 parameters (shifts and angles)\n"
            "  -shift_rotate_scale =  9 parameters (shifts, angles, scale factors)\n"
            "  -affine_general     = 12 parameters (3 shifts, 3x3 matrix)\n"
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
   abas.vwfor      = NULL ;
   abas.vwinv      = NULL ;
   abas.vwset      = NULL ;

   abas.xedge = abas.yedge = abas.zedge = -1 ;
   abas.imww  = abas.imap  = abas.imps  = abas.imsk = NULL ;

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

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

     /*-----*/

     fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
     exit(1) ;

   } /* end of loop over options */

   /*-- 1 remaining argument should be a dataset --*/

   if( nopt != argc-1 ){
     fprintf(stderr,"** Command line should have exactly 1 dataset!\n"
                    "** Whereas there seems to be %d of them!\n",
             argc-nopt ) ;
     exit(1) ;
   }

   /*-- input dataset header --*/

   inset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(inset) ){
     fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[nopt]); exit(1);
   }

   /*-- do the heavy lifting --*/

   /*-- polish up the new dataset info --*/

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarpDrive" , argc,argv , outset ) ;

   if( verb ) fprintf(stderr,"\n++ Writing dataset\n") ;
   DSET_delete( inset ) ;
   DSET_write( outset ) ;
   exit(0) ;
}
