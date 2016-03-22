#include "mrilib.h"
#include "zgaussian.c"

#define NCOR 6

static char *cor_name[NCOR] = { "Pearson" , "Spearman" , "Quadrant" , "K-Tau-b" , "TicTacToe" , "Quantile:9" } ;

/* list of functions to compute correlations [cf. thd_correlate.c] */

typedef float (*ccfun)(int,float *,float *) ;
static ccfun cor_func[NCOR] =
 { THD_pearson_corr, THD_spearman_corr, THD_quadrant_corr, THD_ktaub_corr, THD_tictactoe_corr , THD_quantile_corr } ;

/* prototype for function to bootstrap the correlations */

static float_quad Corrboot( int n , float *x , float *y , float xsig , float ysig ,
                            float (*cfun)(int,float *,float *) ) ;

/* prototype for function to get normal theory confidence intervals */

static float_pair PCorrCI( int npt , float cor , float alpha ) ;

/* default number of bootstrap repetitions */

#undef  NBOOT
#define NBOOT 4000
#undef  NBMIN
#define NBMIN 400

static int   nboot = NBOOT ;   /* changed by the -nboot option */
static float alpha = 0.05f ;   /* changed by the -alpha option */
static int   doblk = 0 ;       /* changed by the -block option */

/*----------------------------------------------------------------------------*/

void usage_1dCorrelate( int detail ){
     printf(
      "Usage: 1dCorrelate [options] 1Dfile 1Dfile ...\n"
      "------\n"
      " * Each input 1D column is a collection of data points.\n"
      " * The correlation coefficient between each column pair is computed, along\n"
      "   with its confidence interval (via a bias-corrected bootstrap procedure).\n"
      " * The minimum sensible column length is 7.\n"
      " * At least 2 columns are needed [in 1 or more .1D files].\n"
      " * If there are N input columns, there will be N*(N-1)/2 output rows.\n"
      " * Output appears on stdout; redirect ('>' or '>>') as needed.\n"
      " * Only one correlation method can be used in one run of this program.\n"
      " * This program is basically the basterd offspring of program 1ddot.\n"
      " * Also see http://en.wikipedia.org/wiki/Confidence_interval\n"
      "\n"
      "-------\n"
      "Methods   [actually, only the first letter is needed to choose a method]\n"
      "-------   [and the case doesn't matter: '-P' and '-p' both = '-Pearson']\n"
      "\n"
      " -Pearson  = Pearson correlation                    [the default method]\n"
      " -Spearman = Spearman (rank) correlation      [more robust vs. outliers]\n"
      " -Quadrant = Quadrant (binarized) correlation  [most robust, but weaker]\n"
      " -Ktaub    = Kendall's tau_b 'correlation'    [popular somewhere, maybe]\n"
      "\n"
      "-------------\n"
      "Other Options  [these options cannot be abbreviated!]\n"
      "-------------\n"
      "\n"
      " -nboot B  = Set the number of bootstrap replicates to 'B'.\n"
      "             * The default value of B is %d.\n"
      "             * A larger number will give somewhat more accurate\n"
      "               confidence intervals, at the cost of more CPU time.\n"
      "\n"
      " -alpha A  = Set the 2-sided confidence interval width to '100-A' percent.\n"
      "             * The default value of A is 5, giving the 2.5..97.5%% interval.\n"
      "             * The smallest allowed A is 1 (0.5%%..99.5%%) and the largest\n"
      "               allowed value of A is 20 (10%%..90%%).\n"
      "             * If you are interested assessing if the 'p-value' of a\n"
      "               correlation is smaller than 5%% (say), then you should use\n"
      "               '-alpha 10' and see if the confidence interval includes 0.\n"
      "\n"
      " -block    = Attempt to allow for serial correlation in the data by doing\n"
      "   *OR*      variable-length block resampling, rather than completely\n"
      " -blk        random resampling as in the usual bootstrap.\n"
      "             * You should NOT do this unless you believe that serial\n"
      "               correlation (along each column) is present and significant.\n"
      "             * Block resampling requires at least 20 data points in each\n"
      "               input column.  Fewer than 20 will turn off this option.\n"
#if 0
      "\n"
      " -vsig ss  = With this option, you specify that each individual measurement\n"
      "             variable has a nonzero standard deviation ('sig') -- that data\n"
      "             might come from some calibration studies, for example.  In the\n"
      "             bootstrap analysis, the random replicates are perturbed by\n"
      "             independent zero-mean normal deviates with standard deviations\n"
      "             given in the file 'ss' -- this file should have 1 row, and that\n"
      "             row should have 1 non-negative number for every column entered\n"
      "             into the correlation analysis.\n"
      "             * In the case of the Pearson coefficient, the 'N:' normal theory\n"
      "               results do NOT include the effect of this extra randomness.\n"
#endif
      "-----\n"
      "Notes\n"
      "-----\n"
      "* For each pair of columns, the output include the correlation value\n"
      "  as directly calculated, plus the bias-corrected bootstrap value, and\n"
      "  the desired (100-A)%% confidence interval [also via bootstrap].\n"
      "\n"
      "* The primary purpose of this program is to provide an easy way to get\n"
      "  the bootstrap confidence intervals, since people almost always seem to use\n"
      "  the asymptotic normal theory to decide if a correlation is 'significant',\n"
      "  and this often seems misleading to me [especially for short columns].\n"
      "\n"
      "* Bootstrapping confidence intervals for the inverse correlations matrix\n"
      "  (i.e., partial correlations) would be interesting -- anyone out there\n"
      "  need this ability?\n"
      "\n"
      "-------------\n"
      "Sample output  [command was '1dCorrelate -alpha 10 A2.1D B2.1D']\n"
      "-------------\n"
      "# Pearson correlation [n=12 #col=2]\n"
      "# Name      Name       Value   BiasCorr   5.00%%   95.00%%  N: 5.00%% N:95.00%%\n"
      "# --------  --------  -------- -------- -------- -------- -------- --------\n"
      "  A2.1D[0]  B2.1D[0]  +0.57254 +0.57225 -0.03826 +0.86306 +0.10265 +0.83353\n"
      "\n"
      "* Bias correction of the correlation had little effect; this is very common.\n"
      "  ++ To be clear, the bootstrap bias correction is to allow for potential bias\n"
      "     in the statistical estimate of correlation when the sample size is small.\n"
      "  ++ It cannot correct for biases that result from faulty data (or faulty\n"
      "     assumptions about the data).\n"
      "\n"
      "* The correlation is NOT significant at this level, since the CI (confidence\n"
      "  interval) includes 0 in its range.\n"
      "\n"
      "* For the Pearson method ONLY, the last two columns ('N:', as above) also\n"
      "  show the widely used asymptotic normal theory confidence interval.  As in\n"
      "  the example, the bootstrap interval is often (but not always) wider than\n"
      "  the theoretical interval.\n"
      "\n"
      "* In the example, the normal theory might indicate that the correlation is\n"
      "  significant (less than a 5%% chance that the CI includes 0), but the\n"
      "  bootstrap CI shows that is not a reasonable statistical conclusion.\n"
      "  ++ The principal reason that I wrote this program was to make it easy\n"
      "     to check if the normal (Gaussian) theory for correlation significance\n"
      "     testing is reasonable in any given case -- for small samples, it often\n"
      "     is NOT reasonable!\n"
      "\n"
      "* Using the same data with the '-S' option gives the table below, again\n"
      "  indicating that there is no significant correlation between the columns\n"
      "  (note also the lack of the 'N:' results for Spearman correlation):\n"
      "\n"
      "# Spearman correlation [n=12 #col=2]\n"
      "# Name      Name       Value   BiasCorr   5.00%%   95.00%%\n"
      "# --------  --------  -------- -------- -------- --------\n"
      "  A2.1D[0]  B2.1D[0]  +0.46154 +0.42756 -0.23063 +0.86078\n"
      "\n"
      "-------------\n"
      "SAMPLE SCRIPT\n"
      "-------------\n"
      "This script generates random data and correlates it until it is\n"
      "statistically significant at some level (default=2%%).  Then it\n"
      "plots the data that looks correlated.  The point is to show what\n"
      "purely random stuff that appears correlated can look like.\n"
      "(Like most AFNI scripts, this is written in tcsh, not bash.)\n"
      "\n"
      "#!/bin/tcsh\n"
      "set npt = 20\n"
      "set alp = 2\n"
      "foreach fred ( `count -dig 1 1 1000` )\n"
      "  1dcat jrandom1D:${npt},2 > qqq.1D\n"
      "  set aabb = ( `1dCorrelate -spearman -alpha $alp qqq.1D  | grep qqq.1D | colrm 1 42` )\n"
      "  set ab = `ccalc -form rint \"1000 * $aabb[1] * $aabb[2]\"`\n"
      "  echo $fred $ab\n"
      "  if( $ab > 1 )then\n"
      "    1dplot -one -noline -x qqq.1D'[0]' -xaxis -1:1:20:5 -yaxis -1:1:20:5            \\\n"
      "           -DAFNI_1DPLOT_BOXSIZE=0.012                                              \\\n"
      "           -plabel \"N=$npt trial#=$fred \\alpha=${alp}%% => r\\in[$aabb[1],$aabb[2]]\"  \\\n"
      "           qqq.1D'[1]'\n"
      "    break\n"
      "  endif\n"
      "end\n"
      "\\rm qqq.1D\n"
      "\n"
      "----------------------------------------------------------------------\n"
      "*** Written by RWCox (AKA Zhark the Mad Correlator) -- 19 May 2011 ***\n"

      , NBOOT ) ;

     PRINT_COMPILE_DATE ;
}

/*============================================================================*/

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk,mm , nvec , nx=0,ny , ff , vlen=4 ;
   MRI_IMAGE *tim , *vsim=NULL ;
   MRI_IMARR *tar ;
   char **vecnam , *tnam ;
   float *far , **tvec , *vsig=NULL , xsig,ysig ;
   float_quad qcor ; float_pair pci ; float corst, cor025, cor500, cor975 ;
   char fmt[256] ;
   int cormeth=0 ;    /* 0=Pearson, 1=Spearman, 2=Quadrant, 3=Kendall tau_b */
   float (*corfun)(int,float *,float *) ;

   /*-- start the AFNI machinery --*/

   mainENTRY("1dCorrelate main") ; machdep() ;

   /* check for options */

   iarg = 1 ; nvec = 0 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
      /* I get by with a little help from my friends? */

      if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         usage_1dCorrelate(strlen(argv[iarg])>3 ? 2:1);
         exit(0) ;
      }

     /*--- methods ---*/

     if( toupper(argv[iarg][1]) == 'P' ){ cormeth = 0 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'S' ){ cormeth = 1 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'Q' ){ cormeth = 2 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'K' ){ cormeth = 3 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'T' ){ cormeth = 4 ; iarg++ ; continue ; }
     if( toupper(argv[iarg][1]) == 'U' ){ cormeth = 5 ; iarg++ ; continue ; }

     /*--- set nboot ---*/

     if( strcasecmp(argv[iarg],"-nboot") == 0 || strcasecmp(argv[iarg],"-num") == 0 ){
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-nboot'") ;
       nboot = (int)strtod(argv[iarg],NULL) ;
       if( nboot < NBMIN ){
         WARNING_message("Replacing -nboot %d with %d",nboot,NBMIN) ;
         nboot = NBMIN ;
       }
       iarg++ ; continue ;
     }

     /*--- set alpha ---*/

     if( strcasecmp(argv[iarg],"-alpha") == 0 ){
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-alpha'") ;
       alpha = (float)strtod(argv[iarg],NULL) ;
       if( alpha < 1.0f ){
         WARNING_message("Replacing -alpha %.1f with 1",alpha) ;
         alpha = 0.01f ;
       } else if( alpha > 20.0f ){
         WARNING_message("Replacing -alpha %.1f with 20",alpha) ;
         alpha = 0.20f ;
       } else {
         alpha *= 0.01f ;  /* convert from percent to fraction */
       }
       iarg++ ; continue ;
     }

     /*--- block resampling ---*/

     if( strcasecmp(argv[iarg],"-blk") == 0 || strcasecmp(argv[iarg],"-block") == 0 ){
       doblk = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-vsig") == 0 ){
       if( vsim != NULL ) ERROR_exit("Can't use -vsig twice!") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after -vsig") ;
       vsim = mri_read_1D(argv[iarg]) ;
       if( vsim == NULL ) ERROR_exit("Can't read -vsig file '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*--- user should be flogged ---*/

     ERROR_message("Monstrously illegal option '%s'",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);
   }

   /*--- user should be flogged twice ---*/

   if( argc < 2 ){
     usage_1dCorrelate(1) ; exit(0) ; 
   }

   if( iarg == argc )
     ERROR_exit("No 1D files on command line!?\n") ;

   /* the function to compute the correlation */

   corfun = cor_func[cormeth] ;

   /* check and assemble list of input 1D files */

   ff = iarg ;
   INIT_IMARR(tar) ;
   for( ; iarg < argc ; iarg++ ){
     tim = mri_read_1D( argv[iarg] ) ;
     if( tim == NULL ) ERROR_exit("Can't read 1D file '%s'",argv[iarg]) ;
     if( nx == 0 ){
       nx = tim->nx ;
       if( nx < 3 )
         ERROR_exit("1D file '%.77s' length=%d is less than 3",argv[iarg],nx) ;
       else if( nx < 7 )
         WARNING_message("1D file '%.77s' length=%d is less than 7",argv[iarg],nx) ;
     } else if( tim->nx != nx ){
       ERROR_exit("Length of 1D file '%.77s' [%d] doesn't match first file [%d]",
                   argv[iarg] , tim->nx , nx );
     }
     nvec += tim->ny ;
     ADDTO_IMARR(tar,tim) ;
   }

   /* user is really an idiot -- flogging's too good for him */

   if( nvec < 2 ) ERROR_exit("Must have at least 2 input columns!") ;

   if( nx < 20 && doblk ){
     doblk = 0 ;
     WARNING_message("Column length %d < 20 ==> cannot use block resampling",nx) ;
   }

   if( vsim != NULL ){
     if( vsim->nvox < nvec )
       ERROR_exit("-vsig file only has %d entries, but needs at least %d",vsim->nvox,nvec) ;
     vsig = MRI_FLOAT_PTR(vsim) ;
   }

   /* create vectors from 1D files */

   tvec = (float **)malloc( sizeof(float *)*nvec ) ;
   vecnam = (char **)malloc( sizeof(char *)*nvec ) ;
   for( jj=0 ; jj < nvec ; jj++ ){
     tvec[jj] = (float *)malloc( sizeof(float)*nx ) ;
     vecnam[jj] = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
   }

   /* copy data into new space, create output labels, check for stoopiditees */

   for( kk=mm=0 ; mm < IMARR_COUNT(tar) ; mm++ ){
     tim = IMARR_SUBIM(tar,mm) ;
     far = MRI_FLOAT_PTR(tim) ;
     tnam = tim->name ; if( tnam == NULL ) tnam = "File" ;
     for( jj=0 ; jj < tim->ny ; jj++,kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) tvec[kk][ii] = far[ii+jj*nx] ;
       sprintf(vecnam[kk],"%s[%d]",THD_trailname(tnam,0),jj) ; /* vector name */
       iarg = strlen(vecnam[kk]) ; vlen = MAX(vlen,iarg) ;
       if( THD_is_constant(nx,tvec[kk]) )
         ERROR_exit("Column %s is constant!",vecnam[kk]) ;
     }
   }
   DESTROY_IMARR(tar) ;

   /*--- Print a beeyootiful header ---*/

   printf("# %s correlation [n=%d #col=%d]\n",cor_name[cormeth],nx,nvec) ;
   sprintf(fmt,"# %%-%ds  %%-%ds",vlen,vlen) ;
   printf(fmt,"Name","Name") ;
   printf("   Value   BiasCorr  %5.2f%%   %5.2f%%",50.0f*alpha,100.0f-50.0f*alpha) ;
   if( cormeth == 0 )  /* Pearson */
     printf("  N:%5.2f%% N:%5.2f%%",50.0f*alpha,100.0f-50.0f*alpha) ;
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
   if( cormeth != 0 )  /* non-Pearson */
     sprintf(fmt,"  %%-%ds  %%-%ds  %%+8.5f %%+8.5f %%+8.5f %%+8.5f\n",vlen,vlen) ;
   else                /* Pearson */
     sprintf(fmt,"  %%-%ds  %%-%ds  %%+8.5f %%+8.5f %%+8.5f %%+8.5f %%+8.5f %%+8.5f\n",vlen,vlen) ;

   /*--- Do some actual work for a suprising change ---*/

   for( jj=0 ; jj < nvec ; jj++ ){       /* loops over column pairs */
     for( kk=jj+1 ; kk < nvec ; kk++ ){

       if( vsig != NULL ){ xsig = vsig[jj]; ysig = vsig[kk]; } else { xsig = ysig = 0.0f; }

       qcor = Corrboot( nx, tvec[jj], tvec[kk], xsig, ysig, corfun ) ;  /* outsourced */

       corst = qcor.a ; cor025 = qcor.b ; cor500 = qcor.c ; cor975 = qcor.d ;

       if( cormeth == 0 ){                      /* Pearson */
         pci = PCorrCI( nx , corst , alpha ) ;
         printf(fmt, vecnam[jj], vecnam[kk], corst, cor500, cor025, cor975, pci.a,pci.b ) ;
       } else {                                 /* all other methods */
         printf(fmt, vecnam[jj], vecnam[kk], corst, cor500, cor025, cor975 ) ;
       }

     }
   }

   /* Finished -- go back to watching Star Trek reruns -- Tribbles ahoy, Cap'n! */

   exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* Correlate 2 vectors of length n, using the function 'cfun'.
   Return values are in a float_quad with four elements:
     .a = standard correlation estimate
     .b = lower edge (alpha/2) of bias-corrected bootstrap confidence interval
     .c = middle (50%) of bootstrap confidence interval
     .d = upper edge (1-alpha/2) of bootstrap confidence interval
*//*--------------------------------------------------------------------------*/

static float_quad Corrboot( int n , float *x   , float *y   ,
                                    float xsig , float ysig ,
                            float (*cfun)(int,float *,float *) )
{
   float *xar , *yar , *cbb , corst ;
   float_quad res = {0.0f,-1.0f,0.0f,1.0f} ;
   float_triple bci ;
   int ii,jj,kk , nn=n ;

ENTRY("Corrboot") ;

   if( nn < 3 || x == NULL || y == NULL || cfun == NULL ) RETURN(res) ;

   if( xsig < 0.0f ) xsig = 0.0f ;
   if( ysig < 0.0f ) ysig = 0.0f ;

   /* workspaces */

   xar = (float *)malloc(sizeof(float)*nn) ;     /* resampled x vector */
   yar = (float *)malloc(sizeof(float)*nn) ;     /* resampled y vector */
   cbb = (float *)malloc(sizeof(float)*nboot) ;  /* saved correlations */

   /* compute the un-resampled result */

   for( ii=0 ; ii < nn ; ii++ ){ xar[ii] = x[ii] ; yar[ii] = y[ii] ; }

   corst = cfun(nn,xar,yar) ;  /* standard result */

   /* compute bootstrap results */

   for( kk=0 ; kk < nboot ; kk++ ){
     if( !doblk ){                    /* simple resampling */
       for( ii=0 ; ii < nn ; ii++ ){
         jj = lrand48() % nn ; xar[ii] = x[jj] ; yar[ii] = y[jj] ;
       }
     } else {                         /* block resampling */
       int jold = lrand48() % nn ;
       xar[0] = x[jold] ; yar[0] = y[jold] ;
       for( ii=1 ; ii < nn ; ii++ ){
         if( lrand48()%8 == 0 ){     /* 12.5% chance of random jump */
           jj = lrand48() % nn ;
         } else {
           jj = jold+1 ; if( jj == nn ) jj = 0 ;
         }
         xar[ii] = x[jj] ; yar[ii] = y[jj] ; jold = jj ;
       }
     }
     if( xsig > 0.0f || ysig > 0.0f ){
       for( ii=0; ii < nn; ii++ ){
         xar[ii] += zgaussian()*xsig; yar[ii] += zgaussian()*ysig;
       }
     }
     cbb[kk] = cfun(nn,xar,yar) ;  /* bootstrap result */
   }

   /* get the actual bias-corrected results [cf. thd_correlate.c] */

   bci = THD_bootstrap_confinv( corst , alpha , nboot , cbb ) ;

   /* empty the dustbin */

   free(cbb) ; free(yar) ; free(xar) ;

   /* this is bad */

   if( bci.a == 0.0f && bci.b == 0.0f && bci.c == 0.0f ) RETURN(res) ;

   /* this is good */

   res.a = corst ; res.b = bci.a ; res.c = bci.b ; res.d = bci.c ;

   RETURN(res) ;
}

/*----------------------------------------------------------------------------*/
/* Utility functions for PCorrCI (infra). */

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

static float MYtanh( float x )
{
  register float ex , exi ;
       if( x >  7.0f ) return  1.0f ;  /* check for stupid inputs */
  else if( x < -7.0f ) return -1.0f ;
  ex = exp(x) ; exi = 1.0f/ex ;
  return (ex-exi)/(ex+exi) ;
}

/*----------------------------------------------------------------------------*/
/* Get the bivariate normal theory confidence interval for Pearson */

static float_pair PCorrCI( int npt , float cor , float alph )
{
   float_pair ci = {-1.0f,1.0f} ;
   float zc , zb,zt , dz ;

   if( npt < 4 || cor <= -1.0f || cor >= 1.0f ) return ci ;    /* bad inputs */

   zc   = MYatanh(cor) ;                       /* Piscatorial transformation */
   dz   = qginv(0.5*alph) / sqrt(npt-3.0) ;   /* cf. mri_stats.c for qginv() */
   ci.a = MYtanh(zc-dz) ;                                     /* lower bound */
   ci.b = MYtanh(zc+dz) ;                                     /* upper bound */
   return ci ;
}
