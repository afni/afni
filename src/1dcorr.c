#include "mrilib.h"

#define NCOR 5

static char *cor_name[NCOR] = { "Pearson" , "Spearman" , "Quadrant" , "MutualInfo" , "CorrRatio" } ;
typedef float (*cfun)(int,float *,float *) ;
static cfun cor_func[NCOR] =
 { THD_pearson_corr , THD_spearman_corr_nd , THD_quadrant_corr_nd ,
   THD_mutual_info  , THD_corr_ratio } ;

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , do_one=0 , nx=0,ny , ff ;
   MRI_IMAGE *tim ;
   MRI_IMARR *tar ;
   float sum , **tvec ;
   float *far ;

   /* help? */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dcorr [options] 1Dfile 1Dfile ...\n"
            " - Prints various correlations of the 1D columns.\n"
            " - Output appears on stdout.\n"
            " - Not meant for mass consumption.\n"
            "\n"
            "Options:\n"
            " - There are no stinking options.\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* options */

   iarg = 1 ; nvec = 0 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
     ERROR_exit("There are no options available in 1dcorr!") ;
   }

   if( iarg == argc )
     ERROR_exit("No 1D files on command line!?\n") ;

   /* input 1D files */

   ff = iarg ;
   INIT_IMARR(tar) ; if( do_one ) nvec = 1 ;
   for( ; iarg < argc ; iarg++ ){
     tim = mri_read_1D( argv[iarg] ) ;
     if( tim == NULL ){
       fprintf(stderr,"** Can't read 1D file %s\n",argv[iarg]); exit(1);
     }
     if( nx == 0 ){
       nx = tim->nx ;
     } else if( tim->nx != nx ){
       ERROR_exit("1D file %s doesn't match first file in length!",argv[iarg]);
     }
     nvec += tim->ny ;
     ADDTO_IMARR(tar,tim) ;
   }

   /* create vectors from 1D files */

   tvec = (float **) malloc( sizeof(float *)*nvec ) ;
   for( jj=0 ; jj < nvec ; jj++ )
     tvec[jj] = (float *) malloc( sizeof(float)*nx ) ;

   kk = 0 ;

   for( mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( jj=0 ; jj < tim->ny ; jj++,kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) tvec[kk][ii] = far[ii+jj*nx] ;
     }
   }
   DESTROY_IMARR(tar) ;

   /* Correlations */

   for( mm=0 ; mm < NCOR ; mm++ ){
     printf("\n++ %s:\n   ",cor_name[mm]) ;
     for( kk=0 ; kk < nvec ; kk++ ) printf(" ---- %02d ----",kk) ;
     printf("\n") ;
     for( kk=0 ; kk < nvec ; kk++ ){
       printf("%02d:",kk) ;
       for( jj=0 ; jj < nvec ; jj++ ){
         sum = (cor_func[mm])( nx , tvec[kk] , tvec[jj] ) ;
         printf(" %12.3f",sum) ;
       }
       printf("\n") ;
     }
   }

   exit(0) ;
}
