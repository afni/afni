#include "mrilib.h"

int dbc( const void *ap , const void *bp )   /* for qsort */
{
   double *a=(double *)ap,  *b=(double *)bp ;
   if( *a == *b ) return 0  ;
   if( *a <  *b ) return -1 ;
                  return  1 ;
}

/*-----------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , do_one=0 , nx=0,ny , ff ;
   MRI_IMAGE *tim ;
   MRI_IMARR *tar ;
   double *amat , *sval , *umat , *vmat , smax,del,sum ;
   float *far ;
   int do_cond=0 ;  /* 08 Nov 2004 */
   int do_sing=0 ;
   int do_1Drr=0 ;  /* 05 Jan 2005 */
   int pall=1 ;

   /* help? */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dsvd [options] 1Dfile 1Dfile ...\n"
            "- Computes SVD of the matrix formed by the 1D file(s).\n"
            "- Output appears on stdout; to save it, use '>' redirection.\n"
            "\n"
            "Options:\n"
            " -one     = Make 1st vector be all 1's.\n"
            " -cond    = Only print condition number (ratio of extremes)\n"
            " -sing    = Only print singular values\n"
            " -1Dright = Only output right eigenvectors, in a .1D format\n"
            "            This can be useful for reducing the number of\n"
            "            columns in a design matrix.  The singular values\n"
            "            are printed at the top of each vector column.\n"
           ) ;
     exit(0) ;
   }

   /* options */

   iarg = 1 ; nvec = 0 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-1Dright") == 0 ){
       pall = 0 ; do_1Drr = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-one") == 0 ){
       do_one = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-cond") == 0 ){
       pall = 0 ; do_cond = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sing") == 0 ){
       pall = 0 ; do_sing = 1 ; iarg++ ; continue ;
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

   if( pall ){
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
   }

   /* create matrix from 1D files */

#define A(i,j) amat[(i)+(j)*nx]     /* nx X nvec matrix */
#define U(i,j) umat[(i)+(j)*nx]     /* ditto */
#define V(i,j) vmat[(i)+(j)*nvec]   /* nvec X nvec matrix */
#define X(i,j) amat[(i)+(j)*nvec]   /* nvec X nx matrix */

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

   if( do_cond ){
     double sbot,stop , cnum ;
     sbot = stop = MAX(sval[0],0.0) ;
     for( jj=1 ; jj < nvec ; jj++ ){
       if( sval[jj] < sbot ) sbot = sval[jj] ;
       if( sval[jj] > stop ) stop = sval[jj] ;
     }
     cnum = stop/sbot ;
     if( do_1Drr ) printf("# condition number = ") ;
     printf("%.7g\n",cnum) ;
   }

   if( do_sing && !do_1Drr ){
     qsort( sval , nvec , sizeof(double) , dbc ) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %6g",sval[jj]) ;
     printf("\n") ;
   }

   if( !pall && !do_1Drr ) exit(0) ;

   if( !do_1Drr ){
     printf("\n"
            "++ Data vectors [A]:\n   " ) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
     printf("\n") ;
     for( kk=0 ; kk < nx ; kk++ ){
       printf("%02d:",kk) ;
       for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",A(kk,jj)) ;
       printf("\n") ;
     }
   }

   if( !do_1Drr ) printf("\n++ Right Vectors [U]:\n   " ) ;

   if( do_1Drr ) printf("#") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" %12.5g",sval[jj]) ;
   printf("\n") ;
   if( do_1Drr ) printf("#") ; else printf("  ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ------------") ;
   printf("\n") ;
   for( kk=0 ; kk < nx ; kk++ ){
     if( !do_1Drr) printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %12.5g",U(kk,jj)) ;
     printf("\n") ;
   }

   if( do_1Drr ) exit(0) ;

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

   smax = sval[0] ;
   for( ii=1 ; ii < nvec ; ii++ )
     if( sval[ii] > smax ) smax = sval[ii] ;

   del = 1.e-12 * smax*smax ;
   for( ii=0 ; ii < nvec ; ii++ )
     sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

   /* create pseudo-inverse */

   for( ii=0 ; ii < nvec ; ii++ ){
     for( jj=0 ; jj < nx ; jj++ ){
       sum = 0.0l ;
       for( kk=0 ; kk < nvec ; kk++ )
         sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
       X(ii,jj) = sum ;
     }
   }

   printf("\n"
          "++ Pseudo-inverse:\n   " ) ;
   for( jj=0 ; jj < nx ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nx ; jj++ ) printf(" %9.5f",X(kk,jj)) ;
     printf("\n") ;
   }

   /* done */

   exit(0) ;
}
