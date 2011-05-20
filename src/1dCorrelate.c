#include "mrilib.h"

#define NCOR 4

static char *cor_name[NCOR] = { "Pearson" , "Spearman" , "Quadrant" , "K-Tau-b" } ;

typedef float (*cfun)(int,float *,float *) ;
static cfun cor_func[NCOR] =
 { THD_pearson_corr , THD_spearman_corr , THD_quadrant_corr , THD_ktaub_corr } ;

static float_quad Corrboot( int n , float *x , float *y ,
                            float (*cfun)(int,float *,float *) ) ;

static float_pair PCorrCI( int ndof , float cor , float alpha ) ;

#undef  NBOOT
#define NBOOT 4000  /* must be a integral multiple of 40 */

static int   nboot = NBOOT ;
static float alpha = 0.05f ;
static int   doblk = 0 ;

/*--------------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , nx=0,ny , ff , vlen=4 ;
   MRI_IMAGE *tim ;
   MRI_IMARR *tar ;
   float sum , **tvec ;
   char **vecnam , *tnam ;
   float *far ;
   int cormeth=0 ;
   float (*corfun)(int,float *,float *) = NULL ;
   float_quad qcor ; float_pair pci ; float corst , cor025, cor500 , cor975 ;
   char fmt[256] ;

   /* help? */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dCorrelate [options] 1Dfile 1Dfile ...\n"
            " * Computes correlation coefficient of 1D column pairs.\n"
            " * Minimum column length is 7.\n"
            " * At least 2 columns are needed [in 1 or more .1D files].\n"
            " * If there are N input columns, there will be N*(N-1)/2 output rows.\n"
            " * Output appears on stdout; redirect as needed.\n"
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
            " -block    = Attempt to allow for serial correlation in the data by doing\n"
            "  *OR*       variable-length block resampling, rather than completely\n"
            " -blk        random resampling as in the usual bootstrap.\n"
            "             * You should NOT do this unless you believe that serial\n"
            "               correlation is present and significant.\n"
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
            "Sample output ['1dCorrelate -alpha 10 A.1D B.1D']\n"
            "-------------\n"
            "# Pearson correlation [n=12]\n"
            "# Name      Name       Value   BiasCorr   5.00%%   95.00%%  N: 5.00%% N:95.00%%\n"
            "# --------  --------  -------- -------- -------- -------- -------- --------\n"
            "  A2.1D[0]  B2.1D[0]  +0.57254 +0.57225 -0.03826 +0.86306 +0.10265 +0.83353\n"
            "\n"
            "* The bias correction of the correlation had little effect.\n"
            "\n"
            "* The correlation is not significant at this level, since the CI (confidence\n"
            "  interval) includes 0 in its range.\n"
            "\n"
            "* For the Pearson method ONLY, the last two columns ('N:', as above) also\n"
            "  show the widely used asymptotic normal theory confidence interval.  As in\n"
            "  the example, the bootstrap interval is often (but not always) wider than\n"
            "  the theoretical interval.\n"
            "\n"
            "* In this example, the normal theory might indicate that the correlation is\n"
            "  significant (less than a 5%% chance that the CI goes below 0), but the\n"
            "  bootstrap CI shows that is not in fact a reasonable statistical result.\n"
            "\n"
            "* Using the same data with the '-S' option gives the table below, again\n"
            "  indicating that there is no significant correlation between the columns\n"
            "  (note the lack of the 'N:' results for Spearman correlation):\n"
            "\n"
            "# Spearman correlation [n=12]\n"
            "# Name      Name       Value   BiasCorr   5.00%%   95.00%%\n"
            "# --------  --------  -------- -------- -------- --------\n"
            "  A2.1D[0]  B2.1D[0]  +0.46154 +0.42756 -0.23063 +0.86078\n"
            "\n"
            "* RWCox (AKA Zhark the Correlator) -- 19 May 2011\n"

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

     if( strcasecmp(argv[iarg],"-blk") == 0 || strcasecmp(argv[iarg],"-block") == 0 ){
       doblk = 1 ; iarg++ ; continue ;
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
   printf("   Value   BiasCorr  %5.2f%%   %5.2f%%",50.0f*alpha,100.0f-50.0f*alpha) ;
   if( cormeth == 0 ) printf("  N:%5.2f%% N:%5.2f%%",50.0f*alpha,100.0f-50.0f*alpha) ;
   printf("\n") ;
   printf("# ") ;
   for( ii=0 ; ii < vlen ; ii++ ) printf("-") ;
   printf("  ") ;
   for( ii=0 ; ii < vlen ; ii++ ) printf("-") ;
   printf(" ") ;
   printf(" --------") ;
   printf(" --------") ;
   printf(" --------") ;
   printf(" --------") ;
   if( cormeth == 0 ){ printf(" --------") ; printf(" --------") ; }
   printf("\n") ;
   if( cormeth != 0 )
     sprintf(fmt,"  %%-%ds  %%-%ds  %%+8.5f %%+8.5f %%+8.5f %%+8.5f\n",vlen,vlen) ;
   else
     sprintf(fmt,"  %%-%ds  %%-%ds  %%+8.5f %%+8.5f %%+8.5f %%+8.5f %%+8.5f %%+8.5f\n",vlen,vlen) ;
   for( jj=0 ; jj < nvec ; jj++ ){
     for( kk=jj+1 ; kk < nvec ; kk++ ){

       qcor = Corrboot( nx , tvec[jj] , tvec[kk] , corfun ) ;

       corst = qcor.a ; cor025 = qcor.b ; cor500 = qcor.c ; cor975 = qcor.d ;

       if( cormeth == 0 ){
         pci = PCorrCI( nx , corst , alpha ) ;
         printf(fmt, vecnam[jj], vecnam[kk], corst, cor500, cor025, cor975, pci.a,pci.b ) ;
       } else {
         printf(fmt, vecnam[jj], vecnam[kk], corst, cor500, cor025, cor975 ) ;
       }
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
     if( !doblk ){
       for( ii=0 ; ii < nn ; ii++ ){
         jj = lrand48() % nn ; xar[ii] = x[jj] ; yar[ii] = y[jj] ;
       }
     } else {
       int jold = lrand48() % nn ;
       xar[0] = x[jold] ; yar[0] = y[jold] ;
       for( ii=1 ; ii < nn ; ii++ ){
         if( lrand48()%4 == 0 ){     /* 25% chance of random jump */
           jj = lrand48() % nn ;
         } else {
           jj = jold+1 ; if( jj == nn ) jj = 0 ;
         }
         xar[ii] = x[jj] ; yar[ii] = y[jj] ; jold = jj ;
       }
     }
     cbb[kk] = cfun(nn,xar,yar) ;
   }

   bci = THD_bootstrap_confinv( corst , alpha , nboot , cbb ) ;

   free(cbb) ; free(yar) ; free(xar) ;

   if( bci.a == 0.0f && bci.b == 0.0f && bci.c == 0.0f ) return res ;

   res.a = corst ; res.b = bci.a ; res.c = bci.b ; res.d = bci.c ;
   return res ;
}

/*----------------------------------------------------------------------------*/

#undef  PHI
#define PHI(x) (1.0-qg(x))       /* CDF of N(0,1) */

#undef  PHINV
#define PHINV(x) qginv(1.0-(x))  /* inverse CDF of N(0,1) */

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

static INLINE float MYtanh( float x )
{
  register float ex , exi ;
       if( x >  7.0f ) return  1.0f ;  /* 03 Sep: check for stupid inputs */
  else if( x < -7.0f ) return -1.0f ;
  ex = exp(x) ; exi = 1.0f/ex ;
  return (ex-exi)/(ex+exi) ;
}

static float_pair PCorrCI( int ndof , float cor , float alph )
{
   float_pair ci = {-1.0f,1.0f} ;
   float zc , zb,zt , dz ;

   if( ndof < 3 || cor <= -1.0f || cor >= 1.0f ) return ci ;

   zc = MYatanh(cor) ;
   dz = qginv(0.5*alph) / sqrt(ndof-3.0) ;
   ci.a = MYtanh(zc-dz) ;
   ci.b = MYtanh(zc+dz) ;
   return ci ;
}
