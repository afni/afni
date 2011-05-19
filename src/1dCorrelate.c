#include "mrilib.h"

#define NCOR 4

static char *cor_name[NCOR] = { "Pearson" , "Spearman" , "Quadrant" , "K-Tau-b" } ;

typedef float (*cfun)(int,float *,float *) ;
static cfun cor_func[NCOR] =
 { THD_pearson_corr , THD_spearman_corr , THD_quadrant_corr , THD_ktaub_corr } ;

static float_quad Corrboot( int n , float *x , float *y ,
                            float (*cfun)(int,float *,float *) ) ;

#undef  NBOOT
#define NBOOT 4000  /* must be a integral multiple of 40 */

static int   nboot = NBOOT ;
static float alpha = 0.05f ;

/*--------------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , nx=0,ny , ff , vlen=4 ;
   MRI_IMAGE *tim ;
   MRI_IMARR *tar ;
   float sum , **tvec ;
   char **vecnam , *tnam ;
   float *far ;
   int cormeth = 0 ;
   float (*corfun)(int,float *,float *) = NULL ;
   float_quad qcor ; float corst , cor025, cor500 , cor975 ;
   char fmt[256] ;

   /* help? */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dCorrelate [options] 1Dfile 1Dfile ...\n"
            " * Computes correlation coefficient of 1D column pairs.\n"
            " * Minimum column length is 7.\n"
            " * At least 2 columns are needed [in 1 or more .1D files].\n"
            " * Output appears on stdout; redirect as needed.\n"
            " * There is no allowance made for any serial correlation in the data.\n"
            " * Only one correlation method can be used in one run of this program.\n"
            "\n"
            "-------\n"
            "Options [actually, only the first letter is needed to choose a method]\n"
            "------- [and the case doesn't matter: '-P' and '-p' both = '-pearson']\n"
            " -pearson  = Pearson correlation              [the default method]\n"
            " -spearman = Spearman (rank) correlation      [more robust vs. outliers]\n"
            " -quadrant = Quadrant (binarized) correlation [most robust, but weaker]\n"
            " -ktaub    = Kendall's tau_b 'correlation'    [popular somewhere, maybe]\n"
            "\n"
            " -nboot B  = Set the number of bootstrap replicates to 'B'.\n"
            "             * The default (and minimum allowed) value of B is %d.\n"
            "             * A larger number will give more accurate confidence\n"
            "               intervals, at the cost of more CPU time.\n"
            "\n"
            " -alpha A  = Set the 2-sided confidence interval width to '100-A' percent.\n"
            "             * The default value of A is 5, giving the 2.5..97.5%% interval.\n"
            "             * The smallest allowed A is 1 and the largest is 20.\n"
            "\n"
            "-----\n"
            "Notes\n"
            "-----\n"
            "* For each pair of columns, the output include the correlation value\n"
            "  as directly calculated, plus the bias-corrected bootstrap value, and\n"
            "  the desired confidence interval [also via bootstrap].\n"
            "\n"
            "* The primary purpose of this program is to provide an easy way to get\n"
            "  the bootstrap bias-corrected confidence intervals, since people always\n"
            "  seem to use the asymptotic normal theory to decide if a correlation is\n"
            "  'significant', and this often seems misleading to me [for short columns].\n"
            "\n"
            "-------------\n"
            "Sample output [2 columns with 64 data points: '1dCorrelate qqq.1D']\n"
            "-------------\n"
            "# Pearson correlation [n=64]\n"
            "# Name       Name        Value   BiasCorr   2.5%%    97.5%%\n"
            "# ---------  ---------  -------- -------- -------- --------\n"
            "  qqq.1D[0]  qqq.1D[1]  +0.19523 +0.19255 +0.01738 +0.36059\n"
            "\n"
            "showing that the bias correction of the correlation had\n"
            "little effect, and also showing that the correlation is\n"
            "significant at the 5%% level [just barely].\n"
            "\n"
            "* If there are N input columns, there will be N*(N-1)/2 output rows.\n"
            "\n"
            "* RWCox -- 19 May 2011\n"

           , NBOOT ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* options */

   iarg = 1 ; nvec = 0 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( toupper(argv[iarg][1]) == 'P' ){ cormeth = 0 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'S' ){ cormeth = 1 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'Q' ){ cormeth = 2 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'K' ){ cormeth = 3 ; iarg++ ; continue ; }

     if( strcasecmp(argv[iarg],"-nboot") == 0 ){
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-nboot'") ;
       nboot = (int)strtod(argv[iarg],NULL) ;
       if( nboot < NBOOT ){
         WARNING_message("Replacing -nboot %d with %d",nboot,NBOOT) ;
         nboot = NBOOT ;
       }
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-alpha") == 0 ){
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-alpha'") ;
       alpha = (float)strtod(argv[iarg],NULL) ;
       if( alpha < 1.0f || alpha > 20.0f ){
         WARNING_message("Replacing -alpha %.1f with 5",alpha) ;
         alpha = 0.05f ;
       } else {
         alpha *= 0.01f ;  /* convert from percent to fraction */
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Monstrously illegal option '%s'",argv[iarg]) ;
   }
   corfun = cor_func[cormeth] ;

   if( iarg == argc )
     ERROR_exit("No 1D files on command line!?\n") ;

   /* input 1D files */

   ff = iarg ;
   INIT_IMARR(tar) ;
   for( ; iarg < argc ; iarg++ ){
     tim = mri_read_1D( argv[iarg] ) ;
     if( tim == NULL ) ERROR_exit("Can't read 1D file %s",argv[iarg]) ;
     if( nx == 0 ){
       nx = tim->nx ;
       if( nx < 7 )
         ERROR_exit("1D file %s length=%d is less than 7",argv[iarg],nx) ;
     } else if( tim->nx != nx ){
       ERROR_exit("Length of 1D file %s [%d] doesn't match first file [%d]",
                   argv[iarg] , tim->nx , nx );
     }
     nvec += tim->ny ;
     ADDTO_IMARR(tar,tim) ;
   }

   if( nvec < 2 ) ERROR_exit("Must have at least 2 input columns!") ;

   /* create vectors from 1D files */

   tvec = (float **)malloc( sizeof(float *)*nvec ) ;
   vecnam = (char **)malloc( sizeof(char *)*nvec ) ;
   for( jj=0 ; jj < nvec ; jj++ ){
     tvec[jj] = (float *)malloc( sizeof(float)*nx ) ;
     vecnam[jj] = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
   }

   for( kk=mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     far = MRI_FLOAT_PTR(tim) ;
     tnam = tim->name ; if( tnam == NULL ) tnam = "File" ;
     for( jj=0 ; jj < tim->ny ; jj++,kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) tvec[kk][ii] = far[ii+jj*nx] ;
       THD_const_detrend( nx , tvec[kk] , NULL ) ;
       sum = THD_normalize( nx , tvec[kk] ) ;
       if( sum == 0.0f )
         ERROR_exit("Column #%d in file #%d is constant!",jj,mm+1) ;
       sprintf(vecnam[kk],"%s[%d]",THD_trailname(tnam,0),jj) ;
       iarg = strlen(vecnam[kk]) ; vlen = MAX(vlen,iarg) ;
     }
   }
   DESTROY_IMARR(tar) ;

   /* A Bunch of Correlations */

   srand48((long)time(NULL)+(long)getpid()) ; /* initialize rand48 seed */

   printf("# %s correlation [n=%d]\n",cor_name[cormeth],nx) ;
   sprintf(fmt,"# %%-%ds  %%-%ds",vlen,vlen) ;
   printf(fmt,"Name","Name") ;
   printf("   Value   BiasCorr  %5.2f%%   %5.2f%% \n",50.0f*alpha,100.0f-50.0f*alpha) ;
   printf("# ") ;
   for( ii=0 ; ii < vlen ; ii++ ) printf("-") ;
   printf("  ") ;
   for( ii=0 ; ii < vlen ; ii++ ) printf("-") ;
   printf(" ") ;
   printf(" --------") ;
   printf(" --------") ;
   printf(" --------") ;
   printf(" --------") ;
   printf("\n") ;
   sprintf(fmt,"  %%-%ds  %%-%ds  %%+8.5f %%+8.5f %%+8.5f %%+8.5f\n",vlen,vlen) ;
   for( jj=0 ; jj < nvec ; jj++ ){
     for( kk=jj+1 ; kk < nvec ; kk++ ){

       qcor = Corrboot( nx , tvec[jj] , tvec[kk] , corfun ) ;

       corst = qcor.a ; cor025 = qcor.b ; cor500 = qcor.c ; cor975 = qcor.d ;

       printf(fmt , vecnam[jj] , vecnam[kk] , corst , cor500 , cor025 , cor975 ) ;
     }
   }

   exit(0) ;
}

/*----------------------------------------------------------------------------*/

/* Return values:
    .a = standard correlation estimate
    .b = lower edge (2.5%) of confidence interval
    .c = middle (50%) of confidence interval
    .d = upper edge (97.5%) of confidence interval
*/

static float_quad Corrboot( int n , float *x , float *y ,
                            float (*cfun)(int,float *,float *) )
{
   float *xar , *yar , *cbb , corst ;
   float_quad res = {0.0f,0.0f,0.0f,0.0f} ;
   float_triple bci ;
   int ii,jj,kk , nn=n ;

   if( nn < 7 || x == NULL || y == NULL || cfun == NULL ) return res ;

   xar = (float *)malloc(sizeof(float)*nn) ;
   yar = (float *)malloc(sizeof(float)*nn) ;
   cbb = (float *)malloc(sizeof(float)*nboot) ;

   for( ii=0 ; ii < nn ; ii++ ){ xar[ii] = x[ii] ; yar[ii] = y[ii] ; }

   corst = cfun(nn,xar,yar) ;  /* standard result */

   /* compute bootstrap results */

   for( kk=0 ; kk < nboot ; kk++ ){
     for( ii=0 ; ii < nn ; ii++ ){
       jj = lrand48() % nn ; xar[ii] = x[jj] ; yar[ii] = y[jj] ;
     }
     cbb[kk] = cfun(nn,xar,yar) ;
   }

   bci = THD_bootstrap_confinv( corst , alpha , nboot , cbb ) ;

   free(cbb) ; free(yar) ; free(xar) ;

   if( bci.a == 0.0f && bci.b == 0.0f && bci.c == 0.0f ) return res ;

   res.a = corst ; res.b = bci.a ; res.c = bci.b ; res.d = bci.c ;
   return res ;
}
