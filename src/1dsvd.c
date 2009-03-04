#include "mrilib.h"

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
   int do_1Dll=0 ;  /* 05 Jan 2005 */
   int do_vmean=0 , do_vnorm=0 ; /* 25 Feb 2008 */
   int pall=1 , nev=0 ;

   /*---------- help? ----------*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 1dsvd [options] 1Dfile 1Dfile ...\n"
       "- Computes SVD of the matrix formed by the 1D file(s).\n"
       "- Output appears on stdout; to save it, use '>' redirection.\n"
       "\n"
       "OPTIONS:\n"
       " -one    = Make 1st vector be all 1's.\n"
       " -vmean  = Remove mean from each vector (can't be used with -one).\n"
       " -vnorm  = Make L2-norm of each vector = 1 before SVD.\n"
       "           * The above 2 options mirror those in 3dpc.\n"
       " -cond   = Only print condition number (ratio of extremes)\n"
       " -sing   = Only print singular values\n"
       " -sort   = Sort singular values (descending) [the default]\n"
       " -nosort = Don't bother to sort the singular values\n"
       " -asort  = Sort singular values (ascending)\n"
       " -1Dleft = Only output left eigenvectors, in a .1D format\n"
       "           This might be useful for reducing the number of\n"
       "           columns in a design matrix.  The singular values\n"
       "           are printed at the top of each vector column,\n"
       "           as a '#...' comment line.\n"
       " -nev n  = If -1Dleft is used, '-nev' specifies only to output\n"
       "           the first 'n' eigenvectors, rather than all of them.\n"
       "EXAMPLE:\n"
       " 1dsvd -vmean -vnorm -1Dleft fred.1D'[1..6]' | 1dplot -stdin\n"
       "NOTES:\n"
       "* Call the input n X m matrix [A] (n rows, m columns).  The SVD\n"
       "  is the factorization [A] = [U] [S] [V]' ('=transpose), where\n"
       "  - [U] is an n x m matrix (whose columns are the 'Left vectors')\n"
       "  - [S] is a diagonal m x m matrix (the 'singular values')\n"
       "  - [V] is an m x m matrix (whose columns are the 'Right vectors')\n"
       "* The default output of the program is\n"
       "  - An echo of the input [A]\n"
       "  - The [U] matrix, each column headed by its singular value\n"
       "  - The [V] matrix, each column headed by its singular value\n"
       "    (please note that [V] is output, not [V]')\n"
       "  - The pseudo-inverse of [A]\n"
       "* This program was written simply for some testing purposes,\n"
       "  but is distributed with AFNI because it might be useful-ish.\n"
       "* Recall that you can transpose a .1D file on input by putting\n"
       "  an escaped ' character after the filename.  For example,\n"
       "    1dsvd fred.1D\\'\n"
       "  You can use this feature to get around the fact that there\n"
       "  is no '-1Dright' option.  If you understand.\n"
       "* For more information on the SVD, you can start at\n"
       "  http://en.wikipedia.org/wiki/Singular_value_decomposition\n"
       "* Author: Zhark the Algebraical (Linear).\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---------- options ----------*/

   iarg = 1 ; nvec = 0 ; set_svd_sort(-1) ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-nev") == 0 ){   /* 04 Mar 2009 */
       nev = (int)strtod(argv[++iarg],NULL) ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-sort") == 0 ){
#if 0
       INFO_message("1dsvd: '-sort' option is now the default");
#endif
       set_svd_sort(-1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-nosort") == 0 ){
       set_svd_sort(0) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-asort") == 0 ){
       set_svd_sort(1) ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-1Dright") == 0 ){
       ERROR_exit("-1Dright has been replaced by -1Dleft") ;
     }

     if( strcasecmp(argv[iarg],"-1Dleft") == 0 ){
       pall = 0 ; do_1Dll = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-one") == 0 ){
       do_one = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-vmean") == 0 ){
       do_vmean = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-vnorm") == 0 ){
       do_vnorm = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-cond") == 0 ){
       pall = 0 ; do_cond = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-sing") == 0 ){
       pall = 0 ; do_sing = 1 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s",argv[iarg]) ;
   }

   if( iarg == argc ) ERROR_exit("No 1D files on command line!?\n") ;

   if( do_vmean && do_one )
     ERROR_exit("Can't use -vmean and -one in the same run!") ;

   /* input 1D files */

   ff = iarg ;
   INIT_IMARR(tar) ; if( do_one ) nvec = 1 ;
   for( ; iarg < argc ; iarg++ ){
     tim = mri_read_1D( argv[iarg] ) ;
     if( tim == NULL ) ERROR_exit("Can't read 1D file %s\n",argv[iarg]) ;
     if( nx == 0 )
       nx = tim->nx ;
     else if( tim->nx != nx )
       ERROR_exit("1D file %s doesn't match first file in length!",argv[iarg]);
     nvec += tim->ny ;
     ADDTO_IMARR(tar,tim) ;
   }

   if( !do_1Dll || nev <= 0 || nev > nvec ) nev = nvec ;

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
     for( ii=0 ; ii < nx ; ii++ ) A(ii,kk) = 1.0 ;
     kk++ ;
   }

   for( mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( jj=0 ; jj < tim->ny ; jj++,kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) A(ii,kk) = far[ii+jj*nx] ;
     }
   }
   DESTROY_IMARR(tar) ;  /* done with input data images */

   if( do_vmean ){  /* 25 Feb 2008 */
     double sum ;
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj) ;
       sum /= nx ;
       for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) -= sum ;
     }
   }
   if( do_vnorm ){  /* 25 Feb 2008 */
     double sum ;
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
       if( sum > 0.0 ){
         sum = 1.0 / sqrt(sum) ;
         for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) *= sum ;
       }
     }
   }

   /**----- the core of the program -----**/

   svd_double( nx , nvec , amat , sval , umat , vmat ) ;

   /*-- various outputs now go to stdout --*/

   if( do_cond ){
     double sbot,stop , cnum ;
     sbot = stop = MAX(sval[0],0.0) ;
     for( jj=1 ; jj < nvec ; jj++ ){
       if( sval[jj] < sbot ) sbot = sval[jj] ;
       if( sval[jj] > stop ) stop = sval[jj] ;
     }
     cnum = stop/sbot ;
     if( do_1Dll ) printf("# condition number = ") ;
     printf("%.7g\n",cnum) ;
   }

   if( do_sing && !do_1Dll ){
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %6g",sval[jj]) ;
     printf("\n") ;
   }

   if( !pall && !do_1Dll ) exit(0) ;

   if( !do_1Dll ){
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

   if( !do_1Dll ) printf("\n++ Left Vectors [U]:\n   " ) ;
   else           printf("# Left Vectors\n#") ;
   for( jj=0 ; jj < nev ; jj++ ) printf(" %12.6g",sval[jj]) ;
   printf("\n") ;
   if( do_1Dll ) printf("#") ; else printf("   ") ;
   for( jj=0 ; jj < nev ; jj++ ) printf(" ------------") ;
   printf("\n") ;
   for( kk=0 ; kk < nx ; kk++ ){
     if( !do_1Dll) printf("%02d:",kk) ;
     for( jj=0 ; jj < nev ; jj++ ) printf(" %12.6g",U(kk,jj)) ;
     printf("\n") ;
   }

   if( do_1Dll ) exit(0) ;

   printf("\n"
          "++ Right Vectors [V]:\n   " ) ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" %12.6g",sval[jj]) ;
   printf("\n   ") ;
   for( jj=0 ; jj < nvec ; jj++ ) printf(" ------------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nvec ; jj++ ) printf(" %12.6g",V(kk,jj)) ;
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
       sum = 0.0 ;
       for( kk=0 ; kk < nvec ; kk++ )
         sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
       X(ii,jj) = sum ;
     }
   }

   printf("\n"
          "++ Pseudo-inverse:\n   " ) ;
   for( jj=0 ; jj < nx ; jj++ ) printf(" ------------") ;
   printf("\n") ;
   for( kk=0 ; kk < nvec ; kk++ ){
     printf("%02d:",kk) ;
     for( jj=0 ; jj < nx ; jj++ ) printf(" %12.6g",X(kk,jj)) ;
     printf("\n") ;
   }

   /* done */

   exit(0) ;
}
