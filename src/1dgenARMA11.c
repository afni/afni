#include "mrilib.h"
#include "zgaussian.c"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#undef  MTYPE
#undef  MPAIR
#define MTYPE double
#define MPAIR double_pair

#undef  BIGVAL
#define BIGVAL 1.e+38

#undef  LAMBDA
#define LAMBDA(a,b) ((b+a)*(1.0+a*b)/(1.0+2.0*a*b+b*b))

static MTYPE corcut = 0.0001 ;

#undef  TAU
#define TAU(i) ((tau==NULL) ? (i) : tau[i])

#define ALLOW_ARMA51

/*--------------------------------------------------------------------------*/
/*! Setup sparse banded correlation matrix (as an rcmat struct):
      [ 1 lam lam*rho lam*rho^2 lam*rho^3 ... ]
    which is the ARMA(1,1) model with the AR parameter a = rho,
    and the MA parameter b such that (b+a)*(1+a*b)/(1+2*a*b+b*b) = lam.
    * For reasonable models of FMRI noise, 0 < lam < rho < 0.9.
    * The maximum bandwidth of the matrix is chosen so that the last
      correlation element is about 0.01.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
    * If tau==NULL, tau[i] is taken to be i -- that is, no censoring/gaps.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma11( int nt, int *tau, MTYPE rho, MTYPE lam )
{
   rcmat  *rcm ;
   LENTYP *len ;
   MTYPE **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 ) return NULL ;

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.99 ) rho =  0.99 ;  /* max allowed NN correlation */
   else if( rho < -0.99 ) rho = -0.99 ;

   /* set maximum bandwidth */

   alam = fabs(lam) ;
   if( alam >= corcut ){
     if( rho != 0.0 ) /* bmax is such that last element is about 'corcut' */
       bmax = 1 + (int)ceil( log(corcut/alam) / log(fabs(rho)) ) ;
     else
       bmax = 1 ;     /* pure MA(1) case */
   } else {
     bmax = 0 ;       /* identity matrix case */
   }

   /* special and trivial case: identity matrix */

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   /* First row/column has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = malloc(sizeof(MTYPE)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwidth */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){      /* a purely diagonal row/column (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = calloc(sizeof(MTYPE),len[ii]) ;      /* space for the entries */
     rii     = rc[ii] - jbot ;         /* shifted pointer to this row/column */
     rii[ii] = 1.0 ;                                       /* diagonal entry */
     for( jj=jbot ; jj < ii ; jj++ ){        /* compute off diagonal entries */
       jtt = itt - TAU(jj) ;                      /* 'time' difference again */
            if( jtt == 1 ) rii[jj] = lam ;               /* lag==1 means lam */
       else if( jtt >  1 ) rii[jj] = lam * pow( rho , jtt-1.0 ) ;
     }
   }

   return rcm ;
}

/*--------------------------------------------------------------------------*/
#include "armacor.c"  /* ARMA(p,1) models for p=3 and 5 [01 Jul 2020] */
/*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*/
/*! Setup sparse banded correlation matrix (as an rcmat struct) for a
    general shift-invariate structure, whose lagged correlations are given
    in corvec.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
    * If tau==NULL, tau[i] is taken to be i -- that is, no censoring/gaps.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma_gen( int nt, int *tau, doublevec *corvec )
{
   rcmat  *rcm ;
   LENTYP *len ;
   MTYPE **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 || corvec == NULL ) return NULL ;

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

   /* set maximum bandwidth */

   bmax = corvec->nar - 1 ;  /* we have lags 0 .. bmax */

   /* special and trivial case: identity matrix */

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   /* First row/column has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = malloc(sizeof(MTYPE)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwidth */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){      /* a purely diagonal row/column (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = calloc(sizeof(MTYPE),len[ii]) ;      /* space for the entries */
     rii     = rc[ii] - jbot ;         /* shifted pointer to this row/column */
     rii[ii] = 1.0 ;                                       /* diagonal entry */
     for( jj=jbot ; jj < ii ; jj++ ){        /* compute off diagonal entries */
       jtt = itt - TAU(jj) ;                      /* 'time' difference again */
       rii[jj] = corvec->ar[jtt] ;        /* extract correlation from corvec */
     }
   }

   return rcm ;
}

/*---------------------------------------------------------------------------*/

rcmat * rcmat_arma31( int nt , int *tau ,
                      double a, double r1, double t1, double vrt )
{
  rcmat *rcm ;
  doublevec *corvec ;

  corvec = arma31_correlations( a , r1 , t1 , vrt , corcut , nt ) ;
{ int ii ; double ssum=0.0 ;
  INFO_message("Correlation count: %d",corvec->nar) ;
  for( ii=0 ; ii < corvec->nar ; ii++ ){
    fprintf(stderr," %g",corvec->ar[ii]) ; if( ii > 0 ) ssum += fabs(corvec->ar[ii]) ;
  }
  fprintf(stderr," : Gsum = %g\n",ssum) ;
}
  rcm = rcmat_arma_gen( nt, tau, corvec ) ;
  KILL_doublevec( corvec ) ;
  return rcm ;
}

#ifdef ALLOW_ARMA51
/*---------------------------------------------------------------------------*/

rcmat * rcmat_arma51( int nt , int *tau ,
                      double a, double r1, double t1,
                                double r2, double t2, double vrt )
{
  rcmat *rcm ;
  doublevec *corvec ;

  corvec = arma51_correlations( a , r1 , t1 , r2 , t2 , vrt , corcut , nt ) ;
{ int ii ; double ssum=0.0 ;
  INFO_message("Correlation count: %d",corvec->nar) ;
  for( ii=0 ; ii < corvec->nar ; ii++ ){
    fprintf(stderr," %g",corvec->ar[ii]) ; if( ii > 0 ) ssum += fabs(corvec->ar[ii]) ;
  }
  fprintf(stderr," : Gsum = %g\n",ssum) ;
}
  rcm = rcmat_arma_gen( nt, tau, corvec ) ;
  KILL_doublevec( corvec ) ;
  return rcm ;
}
#endif

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nlen=0 , nvec=1 , iarg=1 , nbad=0 , kk,ii , do_norm=0 ;
   double aa=-666.0, bb=-666.0 , lam=-666.0 , sig=1.0 ;
   double *rvec ;
   rcmat *rcm=NULL ;
   MRI_IMAGE *outim ;
   float     *outar , *vv ;
   long seed=0 ;

   int do_arma31 = 0 , do_arma51 = 0 , do_arma11 = 1 ;
   double r1=0.0,t1=0.0 , r2=0.0,t2=0.0 , vrt=0.0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Program to generate an ARMA(1,1) time series, for simulation studies.\n"
      "Results are written to stdout.\n"
      "\n"
      "Usage: 1dgenARMA11 [options]\n"
      "\n"
      "Options:\n"
      "========\n"
      " -num N  }  These equivalent options specify the length of the time\n"
      " -len N  }  series vector to generate.\n"
      "\n"
      " -nvec M  = The number of time series vectors to generate;\n"
      "            if this option is not given, defaults to 1.\n"
      "\n"
      " -a a     = Specify ARMA(1,1) parameters 'a'.\n"
      " -b b     = Specify ARMA(1,1) parameter 'b' directly.\n"
      " -lam lam = Specify ARMA(1,1) parameter 'b' indirectly.\n"
      "\n"
      " -sig ss  = Set standard deviation of results [default=1].\n"
      " -norm    = Normalize time series so sum of squares is 1.\n"
      " -seed dd = Set random number seed.\n"
      "\n"
      "  * The correlation coefficient r(k) of noise samples k units apart in time,\n"
      "     for k >= 1, is given by r(k) = lam * a^(k-1)\n"
      "     where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b)\n"
      "     (N.B.: lam=a when b=0 -- AR(1) noise has r(k)=a^k for k >= 0)\n"
      "     (N.B.: lam=b when a=0 -- MA(1) noise has r(k)=b for k=1, r(k)=0 for k>1)\n"
      "  * lam can be bigger or smaller than a, depending on the sign of b:\n"
      "     b > 0 means lam > a;  b < 0 means lam < a.\n"
      "  * What I call (a,b) here is sometimes called (p,q) in the ARMA literature.\n"
      "  * For a noise model which is the sum of AR(1) and white noise, 0 < lam < a\n"
      "     (i.e., a > 0  and  -a < b < 0 ).\n"
      "\n"
      " -CORcut cc = The exact ARMA(1,1) correlation matrix (for a != 0)\n"
      "              has no non-zero entries.  The calculations in this\n"
      "              program set correlations below a cutoff to zero.\n"
      "              The default cutoff is %.5f, but can be altered with\n"
      "              this option.  The usual reason to use this option is\n"
      "              to test the sensitivity of the results to the cutoff.\n"
      "\n"
      "-----------------------------\n"
      "A restricted ARMA(3,1) model:\n"
      "-----------------------------\n"
      "Skip the '-a', '-b', and '-lam' options, and use a model with 3 roots\n"
      "\n"
      " -arma31 a r theta vrat\n"
      "\n"
      " where the roots are z = a, z = r*exp(I*theta), z = r*exp(-I*theta)\n"
      " and vrat = s^2/(s^2+w^2) [so 0 < vrat < 1], where s = variance\n"
      " of the pure AR(3) component and w = variance of extra white noise\n"
      " added to the AR(3) process -- this is the 'restricted' ARMA(3,1).\n"
      "\n"
      " If the data has given TR, and you want a frequency of f Hz, in\n"
      " the noise model, then theta = 2 * PI * TR * f. If theta > PI,\n"
      " then you are modeling noise beyond the Nyquist frequency and\n"
      " the gods (and this program) won't be happy.\n"
      "\n"
      "  # csh syntax for 'set' variable assignment commands\n"
      "  set nt = 500\n"
      "  set tr = 1\n"
      "  set df = `ccalc \"1/($nt*$tr)\"`\n"
      "  set f1 = 0.10\n"
      "  set t1 = `ccalc \"2*PI*$tr*$f1\"`\n"
      "  1dgenARMA11 -nvec 500 -len $nt -arma31 0.8 0.9 $t1 0.9 -CORcut 0.0001 \\\n"
      "       | 1dfft -nodetrend stdin: > qqq.1D\n"
      "  3dTstat -mean -prefix stdout: qqq.1D \\\n"
      "       | 1dplot -stdin -num 201 -dt $df -xlabel 'frequency' -ylabel '|FFT|'\n"
      "---------------------------------------------------------------------------\n"
#ifdef ALLOW_ARMA51
      "A similar option is now available for a restricted ARMA(5,1) model:\n"
      " -arma51 a r1 theta1 r2 theta2 vrat\n"
      "where now the roots are\n"
      " z = a  z = r1*exp(I*theta1)  z = r1*exp(-I*theta1)\n"
      "        z = r2*exp(I*theta2)  z = r2*exp(-I*theta2)\n"
      "This model allows the simulation of two separate frequencies in the 'noise'.\n"
      "---------------------------------------------------------------------------\n"
#endif
      "\n"
      "Author: RWCox [for his own demented and deranged purposes]\n"
      "\n"
      "Examples:\n"
      "  1dgenARMA11 -num 200 -a .8 -lam 0.7 | 1dplot -stdin\n"
      "  1dgenARMA11 -num 2000 -a .8 -lam 0.7 | 1dfft -nodetrend stdin: stdout: | 1dplot -stdin\n"
      "\n" ,
      corcut
     ) ;
     exit(0) ;
   }

   while( iarg < argc ){

     if( strcasecmp(argv[iarg],"-ARMA31") == 0 ){ /* 01 Jul 2020 */
       if( iarg+4 >= argc ) ERROR_exit("Need 4 arguments after option '%s'",argv[iarg]) ;
       do_arma31 = 1 ; do_arma51 = do_arma11 = 0 ;
       aa  = strtod( argv[++iarg] , NULL ) ;
       r1  = strtod( argv[++iarg] , NULL ) ;
       t1  = strtod( argv[++iarg] , NULL ) ;
       vrt = strtod( argv[++iarg] , NULL ) ;
       iarg++ ; continue ;
     }

#ifdef ALLOW_ARMA51
     if( strcasecmp(argv[iarg],"-ARMA51") == 0 ){ /* 01 Jul 2020 */
       if( iarg+6 >= argc ) ERROR_exit("Need 6 arguments after option '%s'",argv[iarg]) ;
       do_arma11 = do_arma31 = 0 ; do_arma51 = 1 ;
       aa  = strtod( argv[++iarg] , NULL ) ;
       r1  = strtod( argv[++iarg] , NULL ) ;
       t1  = strtod( argv[++iarg] , NULL ) ;
       r2  = strtod( argv[++iarg] , NULL ) ;
       t2  = strtod( argv[++iarg] , NULL ) ;
       vrt = strtod( argv[++iarg] , NULL ) ;
       iarg++ ; continue ;
     }
#endif

     if( strcmp(argv[iarg],"-norm") == 0 ){
       do_norm = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-num") == 0 || strcmp(argv[iarg],"-len") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       nlen = (int)strtod(argv[iarg],NULL) ;
       if( nlen < 3 ) ERROR_exit("Time series length must be at least 3!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nvec") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       nvec = (int)strtod(argv[iarg],NULL) ;
       if( nvec < 1 ) ERROR_exit("Number of vectors must be at least 1!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-seed") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       seed = (long)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-sig") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       sig = strtod(argv[iarg],NULL) ;
       if( sig <= 0.0 ) ERROR_exit("Value after '-sig' should be positive!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-a") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       aa = strtod(argv[iarg],NULL) ;
       if( fabs(aa) >= 0.95 ) ERROR_exit("Largest allowed value of a = 0.95") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-b") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       bb = strtod(argv[iarg],NULL) ;
       if( fabs(bb) >= 0.95 ) ERROR_exit("Largest allowed value of b = 0.95") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-lam") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       lam = strtod(argv[iarg],NULL) ;
       if( fabs(lam) >= 0.95 ) ERROR_exit("Largest allowed value of lam = 0.95") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-CORcut") == 0 ){
       iarg++ ;
       if( iarg >= argc ) ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       corcut = strtod(argv[iarg],NULL) ;
       if( corcut <= 0.0 || corcut > 0.1 ) ERROR_exit("Illegal value after '-CORcut'") ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unrecognized option: '%s'",argv[iarg]) ;
   }

   /*-- check errors --*/

   if( nlen <= 3  ){ ERROR_message("Didn't you give the -num/-len option?"); nbad++; }

   if( do_arma11 ){
     if( aa <= -1.0 ){ ERROR_message("Didn't you give the -a option?"); nbad++; }
     if( bb <= -1.0 && lam <= -1.0 ){
       ERROR_message("Didn't you give the -b or -lam option?"); nbad++;
     }
     if( nbad > 0 ) ERROR_exit("Can't continue past the above problem%s" ,
                               (nbad==1)? "\0" : "s" ) ;

     if( bb > -1.0 ){
       lam = LAMBDA(aa,bb) ; INFO_message("lam(a=%.3f,b=%.3f) = %.3f",aa,bb,lam) ;
     }

     /* setup */

     rcm = rcmat_arma11( nlen , NULL , aa , lam ) ;

   } else if( do_arma31 ){

     rcm = rcmat_arma31( nlen , NULL , aa,r1,t1,vrt ) ;

#ifdef ALLOW_ARMA51
   } else if( do_arma51 ){

     rcm = rcmat_arma51( nlen , NULL , aa,r1,t1,r2,t2,vrt ) ;
#endif

   }

   if( rcm == NULL ) ERROR_exit("Can't setup matrix?!") ;  /* should be impossible */

   kk = rcmat_choleski( rcm ) ;
   if( kk > 0 ) ERROR_exit("Choleski fails at row %d",kk) ;

   /* simulate */

   outim = mri_new( nlen , nvec , MRI_float ) ; outar = MRI_FLOAT_PTR(outim) ;
   rvec  = (double *)malloc(sizeof(double)*nlen) ;

   if( seed == 0 ) seed = (long)time(NULL)+(long)getpid() ;
   srand48(seed) ;

   for( kk=0 ; kk < nvec ; kk++ ){
     for( ii=0 ; ii < nlen ; ii++ ) rvec[ii] = zgaussian() ;
     rcmat_lowert_vecmul( rcm , rvec ) ;
     vv = outar + kk*nlen ;
     if( do_norm ){
       sig = 0.0 ;
       for( ii=0 ; ii < nlen ; ii++ ) sig += rvec[ii]*rvec[ii] ;
       sig = 1.0 / sqrt(sig) ;
     }
     for( ii=0 ; ii < nlen ; ii++ ) vv[ii] = sig * rvec[ii] ;
   }

   mri_write_1D(NULL,outim) ;
   exit(0) ;
}
