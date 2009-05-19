#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , do_one=0 , nx=0,ny , ff, doterse = 0 ;
   MRI_IMAGE *tim ;
   MRI_IMARR *tar ;
   double sum , *eval , *amat , **tvec , *bmat , *svec ;
   float *far ;
   int demean=0 , docov=0 ;
   char *matname ;
   int okzero = 0;
   
   /* help? */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1ddot [options] 1Dfile 1Dfile ...\n"
            "- Prints out correlation matrix of the 1D files and\n"
            "  their inverse correlation matrix.\n"
            "- Output appears on stdout.\n"
            "\n"
            "Options:\n"
            " -one  =  Make 1st vector be all 1's.\n"
            " -dem  =  Remove mean from all vectors (conflicts with '-one')\n"
            " -cov  =  Compute with covariance matrix instead of correlation\n"
            " -inn  =  Computed with inner product matrix instead\n"
            " -terse=  Output only the correlation or covariance matrix\n"
            "          and without any of the garnish. \n"
            " -okzero= Do not quit if a vector is all zeros.\n"
            "          The correlation matrix will have 0 where NaNs ought to go.\n"
            "          Expect rubbish in the inverse matrices if all zero \n"
            "          vectors exist.\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* options */

   iarg = 1 ; nvec = 0 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-one") == 0 ){
       demean = 0 ; do_one = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-okzero") == 0 ){
       okzero = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-dem",4) == 0 ){
       demean = 1 ; do_one = 0 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-cov",4) == 0 ){
       docov=1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-inn",4) == 0 ){
       docov=2 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-terse",4) == 0 ){
       doterse = 1 ; iarg++ ; continue ;
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

   if (!doterse) {
      printf("\n") ;
      printf("++ 1ddot input vectors:\n") ;
   }
   jj = 0 ;
   if( do_one ){
     if (!doterse) printf("00..00: all ones\n") ;
     jj = 1 ;
   }
   for( mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     if (!doterse) printf("%02d..%02d: %s\n", jj,jj+tim->ny-1, argv[ff+mm] ) ;
     jj += tim->ny ;
   }

   /* create vectors from 1D files */

   tvec = (double **) malloc( sizeof(double *)*nvec ) ;
   svec = (double * ) malloc( sizeof(double  )*nvec ) ;
   for( jj=0 ; jj < nvec ; jj++ )
     tvec[jj] = (double *) malloc( sizeof(double)*nx ) ;

   kk = 0 ;
   if( do_one ){
     svec[0] = 1.0 / sqrt((double)nx) ;
     for( ii=0 ; ii < nx ; ii++ ) tvec[0][ii] = 1.0 ;
     kk = 1 ;
   }

   for( mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( jj=0 ; jj < tim->ny ; jj++,kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) tvec[kk][ii] = far[ii+jj*nx] ;
       if( demean ){
         sum = 0.0 ;
         for( ii=0 ; ii < nx ; ii++ ) sum += tvec[kk][ii] ;
         sum /= nx ;
         for( ii=0 ; ii < nx ; ii++ ) tvec[kk][ii] -= sum ;
       }
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += tvec[kk][ii] * tvec[kk][ii] ;
       if( sum == 0.0 ) {
         if (okzero) svec[kk] = 0.0; 
         else ERROR_exit("Input column %02d is all zero!",kk) ;
       } else {
         svec[kk] = 1.0 / sqrt(sum) ;
       }
     }
   }
   DESTROY_IMARR(tar) ;

   /* normalize vectors? */

   if( !docov ){
     for( kk=0 ; kk < nvec ; kk++ ){
       sum = svec[kk] ;
       for( ii=0 ; ii < nx ; ii++ ) tvec[kk][ii] *= sum ;
     }
   }

   switch(docov){
     default:
     case 2:  matname = "InnerProduct" ; break ;
     case 1:  matname = "Covariance"   ; break ;
     case 0:  matname = "Correlation"  ; break ;
   }

   /* create matrix from dot product of vectors */

   amat = (double *) calloc( sizeof(double) , nvec*nvec ) ;

   for( kk=0 ; kk < nvec ; kk++ ){
     for( jj=0 ; jj <= kk ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += tvec[jj][ii] * tvec[kk][ii] ;
       amat[jj+nvec*kk] = sum ;
       if( jj < kk ) amat[kk+nvec*jj] = sum ;
     }
   }

   /* normalize */
   if (docov==1) {
      for( kk=0 ; kk < nvec ; kk++ ){
         for( jj=0 ; jj <= kk ; jj++ ){
            sum = amat[jj+nvec*kk] / (double) (nx-1);
            amat[jj+nvec*kk] = sum;
            if( jj < kk ) amat[kk+nvec*jj] = sum ;
         }
      }
   }

   /* print matrix out */

   if (!doterse) {
      printf("\n"
             "++ %s Matrix:\n   ",matname) ;
      for( jj=0 ; jj < nvec ; jj++ ) printf("    %02d    ",jj) ;
      printf("\n   ") ;
      for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
      printf("\n") ;
   }
   for( kk=0 ; kk < nvec ; kk++ ){
     if (!doterse) printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",amat[jj+kk*nvec]) ;
     printf("\n") ;
   }

   if (doterse) exit(0) ; /* au revoir */

   /* compute eigendecomposition */

   eval = (double *) malloc( sizeof(double)*nvec ) ;
   symeig_double( nvec , amat , eval ) ;

   printf("\n"
          "++ Eigensolution of %s Matrix:\n   " , matname ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",eval[jj]) ;
   printf("\n   ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",amat[kk+jj*nvec]) ;
     printf("\n") ;
   }

   /* compute matrix inverse */
   if ( eval[0]/eval[nvec-1] < 1.0e-10) {
      printf("\n"
             "-- WARNING: Matrix is near singular,\n"
             "            rubbish likely for inverses ahead.\n");
   }
   for( jj=0 ; jj < nvec ; jj++ ) eval[jj] = 1.0 / eval[jj] ;

   bmat = (double *) calloc( sizeof(double) , nvec*nvec ) ;

   for( ii=0 ; ii < nvec ; ii++ ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < nvec ; kk++ )
         sum += amat[ii+kk*nvec] * amat[jj+kk*nvec] * eval[kk] ;
       bmat[ii+jj*nvec] = sum ;
     }
   }

   printf("\n") ;
   printf("++ %s Matrix Inverse:\n   " , matname ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf("    %02d    ",jj) ;
   printf("\n   ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",bmat[jj+kk*nvec]) ;
     printf("\n") ;
   }

   /* square roots of diagonals of the above */

   printf("\n") ;
   printf("++ %s sqrt(diagonal)\n   ",matname) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",sqrt(bmat[jj+jj*nvec])) ;
   printf("\n") ;

   /* normalize matrix inverse */

   for( ii=0 ; ii < nvec ; ii++ ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = bmat[ii+ii*nvec] * bmat[jj+jj*nvec] ;
       if( sum > 0.0 )
         amat[ii+jj*nvec] = bmat[ii+jj*nvec] / sqrt(sum) ;
       else
         amat[ii+jj*nvec] = 0.0 ;
     }
   }

   printf("\n") ;
   printf("++ %s Matrix Inverse Normalized:\n   " , matname ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf("    %02d    ",jj) ;
   printf("\n   ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ---------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %9.5f",amat[jj+kk*nvec]) ;
     printf("\n") ;
   }

   /* done */

   exit(0) ;
}
