#include "mrilib.h"
#include "betafit.c"

/*-----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   BFIT_data   * bfd , * nfd ;
   BFIT_result * bfr , * nfr ;

   int nvals,ival , nvox , nbin , miv , sqr=0 ;
   float pcut , eps,eps1 ;
   float *bval , *cval ;
   double aa,bb,xc,xth ;
   double chq,ccc,cdf ;
   int    ihqbot,ihqtop ;

   int mcount,mgood , ii , jj , ibot,itop ;

   int narg=1 , nboot=0 , nran=1000 ;
   float abot= 0.5 , atop=  4.0 ;
   float bbot=10.0 , btop=200.0 ;
   float pbot=50.0 , ptop= 80.0 ;
   float * aboot , * bboot ;
   float   asig  ,   bsig  ;

   THD_3dim_dataset * input_dset , * mask_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dbetafit [options] dataset\n"
             "Fits a beta distribution to the values in a brick.\n"
             "\n"
             "Options:\n"
             "  -arange abot atop = Sets the search range for parameter\n"
             "                        'a' to abot..atop.\n"
             "                        [default is 0.5 .. 4.0]\n"
             "\n"
             "  -brange bbot btop = Sets the search range for parameter\n"
             "                        'b' to bbot..btop\n"
             "                        [default is 10 .. 200]\n"
             "\n"
             "  -prange pbot ptop = Will evaluate for percent cutoffs\n"
             "                        from pbot to ptop (steps of 1%%)\n"
             "                        [default is 50 .. 80]\n"
             "\n"
             "  -bootstrap N      = Does N bootstrap evaluations to\n"
             "                        compute variance of 'a' and 'b'\n"
             "                        estimates [default is no bootstrap]\n"
             "\n"
             "  -mask mset  = A mask dataset to indicate which\n"
             "                 voxels are to be used\n"
             "  -mrange b t = Use only mask values in range from\n"
             "                 'b' to 't' (inclusive)\n"
             "\n"
             "  -sqr = Flag to square the data from the dataset\n"
         ) ;
         exit(0) ;
   }

   /* scan command-line args */

   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-sqr") == 0 ){
         sqr = 1 ; narg++ ; continue;
      }

      if( strcmp(argv[narg],"-arange") == 0 ){
         abot = strtod(argv[++narg],NULL) ;
         atop = strtod(argv[++narg],NULL) ;
         if( abot < 0.1 || abot > atop ){
            fprintf(stderr,"*** Illegal value after -arange!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-brange") == 0 ){
         bbot = strtod(argv[++narg],NULL) ;
         btop = strtod(argv[++narg],NULL) ;
         if( bbot < 0.1 || bbot > btop ){
            fprintf(stderr,"*** Illegal value after -brange!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-prange") == 0 ){
         pbot = strtod(argv[++narg],NULL) ;
         ptop = strtod(argv[++narg],NULL) ;
         if( pbot < 30.0 || pbot > ptop ){
            fprintf(stderr,"*** Illegal value after -prange!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-bootstrap") == 0 ){
         nboot = strtod(argv[++narg],NULL) ;
         if( nboot < 10 ){
            fprintf(stderr,"*** Illegal value after -bootstrap!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n");
            exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n")
;
             exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[narg]) ; exit(1) ;
   }

   if( narg >= argc ){
      fprintf(stderr,"*** No dataset argument on command line!?\n");exit(1);
   }

   input_dset = THD_open_dataset( argv[narg] ) ;
   if( input_dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[narg]); exit(1);
   }

   nvox = DSET_NVOX(input_dset) ;

   /* load data from dataset */

   DSET_load(input_dset) ;
   if( !DSET_LOADED(input_dset) ){
      fprintf(stderr,"*** Couldn't load dataset brick!\n");exit(1);
   }

   if( DSET_BRICK_STATCODE(input_dset,0) == FUNC_COR_TYPE ) sqr = 1 ;

   bfd = BFIT_prepare_dataset( input_dset , 0 , sqr ,
                               mask_dset , 0 , mask_bot , mask_top ) ;

   if( bfd == NULL ){
      fprintf(stderr,"*** Couldn't prepare data from input dataset!\n");
      exit(1) ;
   }

   DSET_delete(mask_dset) ; DSET_delete(input_dset) ;

   if( nboot > 0 ){
      aboot = (float *) malloc(sizeof(float)*nboot) ;
      bboot = (float *) malloc(sizeof(float)*nboot) ;
   }

   for( pcut=pbot ; pcut <= ptop ; pcut += 1.0 ){
      bfr = BFIT_compute( bfd ,
                          pcut , abot,atop , bbot,btop , nran,200 ) ;
      if( bfr == NULL ){
         fprintf(stderr,"*** Can't compute betafit at pcut=%f\n",pcut) ;
         exit(1) ;
      }

      itop  = bfr->itop ;
      mgood = bfr->mgood ;

      ibot   = bfd->ibot ;
      bval   = bfd->bval ;
      cval   = bfd->cval ;
      mcount = bfd->mcount ;

      xc   = bfr->xcut ;
      aa   = bfr->a ;
      bb   = bfr->b ;
      eps  = bfr->eps ;

      xth  = beta_p2t( 1.e-4 , aa,bb ) ;
      if( sqr ) xth = sqrt(xth) ;
      if( sqr ) xc  = sqrt(xc ) ;

      if( nboot > 0 ){
         asig = bsig = 0.0 ;
         for( ii=0 ; ii < nboot ; ii++ ){
            nfd = BFIT_bootstrap_sample( bfd ) ;
            nfr = BFIT_compute( nfd ,
                                pcut , abot,atop , bbot,btop , nran,200 ) ;
            aboot[ii] = nfr->a ;
            bboot[ii] = nfr->b ;
            BFIT_free_result(nfr) ;
            BFIT_free_data  (nfd) ;
            asig += SQR( aboot[ii]-aa ) ;
            bsig += SQR( bboot[ii]-bb ) ;
         }
         asig = sqrt(asig/nboot) ;
         bsig = sqrt(bsig/nboot) ;
      }

      if( nboot <= 0 ){
         printf("%3.0f%%: a=%.2f b=%.2f eps=%.2f cutoff=%.2f qfit=%8.2e thr=%.2f\n",
                pcut , aa,bb,eps , xc , bfr->q_chisq , xth ) ;
      } else {
         printf("%3.0f%%: a=%.2f[%.2f] b=%.2f[%.2f] eps=%.2f cutoff=%.2f qfit=%8.2e thr=%.2f\n",
                pcut , aa,asig,bb,bsig,eps , xc , bfr->q_chisq , xth ) ;
      }
      fflush(stdout) ;

      BFIT_free_result(bfr) ;
   }
   exit(0) ;
}
