#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , do_one=0 , nx=0,ny , ff ;
   MRI_IMAGE *tim ;
   MRI_IMARR *tar ;
   double *amat , *sval , *umat , *vmat ;
   float *far ;

   /* help? */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dsvd [options] 1Dfile 1Dfile ...\n"
            "- Computes SVD of the matrix formed by the 1D file(s).\n"
            "- Output appears on stdout.\n"
            "\n"
            "Options:\n"
            " -one  =  Make 1st vector be all 1's.\n"
           ) ;
     exit(0) ;
   }

   /* options */

   iarg = 1 ; nvec = 0 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-one") == 0 ){
       do_one = 1 ; iarg++ ; continue ;
     }

     fprintf(stderr,"** Unknown option: %s\n",argv[iarg]); exit(1);
   }

   if( iarg == argc ){
     fprintf(stderr,"** No 1D files on command line!?\n"); exit(1);
   }

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
       fprintf(stderr,"** 1D file %s doesn't match first file in length!\n",
               argv[iarg]); exit(1);
     }
     nvec += tim->ny ;
     ADDTO_IMARR(tar,tim) ;
   }

   printf("\n") ;
   printf("++ 1dsvd input vectors:\n") ;
   jj = 0 ;
   if( do_one ){
     printf("00..00: all ones\n") ; jj = 1 ;
   }
   for( mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     printf("%02d..%02d: %s\n", jj,jj+tim->ny-1, argv[ff+mm] ) ;
     jj += tim->ny ;
   }

   /* create matrix from 1D files */

#define A(i,j) amat[(i)+(j)*nx]
#define U(i,j) umat[(i)+(j)*nx]
#define V(i,j) vmat[(i)+(j)*nvec]

   amat = (double *)malloc( sizeof(double)*nx*nvec ) ;
   umat = (double *)malloc( sizeof(double)*nx*nvec ) ;
   vmat = (double *)malloc( sizeof(double)*nvec*nvec ) ;
   sval = (double *)malloc( sizeof(double)*nvec ) ;

   kk = 0 ;
   if( do_one ){
     for( ii=0 ; ii < nx ; ii++ ) A(ii,kk) = 1.0l ;
     kk++ ;
   }

   for( mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( jj=0 ; jj < tim->ny ; jj++,kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) A(ii,kk) = far[ii+jj*nx] ;
     }
   }
   DESTROY_IMARR(tar) ;

   svd_double( nx , nvec , amat , sval , umat , vmat ) ;

   printf("\n"
          "++ Data vectors [A]:\n   " ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nx ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",A(kk,jj)) ;
     printf("\n") ;
   }

   printf("\n"
          "++ Right Vectors [U]:\n   " ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",sval[jj]) ;
   printf("\n   ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nx ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",U(kk,jj)) ;
     printf("\n") ;
   }

   printf("\n"
          "++ Left Vectors [V]:\n   " ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",sval[jj]) ;
   printf("\n   ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",V(kk,jj)) ;
     printf("\n") ;
   }

   /* done */

   exit(0) ;
}
