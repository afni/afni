#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <cs.h>
#include "f2c.h"
#include <time.h>

/****************************/
/** cf. powell_newuoa.[fc] **/

extern int newuoa_(integer *n, integer *npt, doublereal *x,
                   doublereal *rhobeg, doublereal *rhoend, integer *maxfun,
                   doublereal *w, integer *icode) ;

/*---------------------------------------------------------------------------*/
/* Macro to periodically reduce a variable into the range 0..1:
   for example: PRED01(1.2) == 0.8, PRED01(1.8) == 0.2, et cetera;
   graphically
               PRED01(x)|
                        | /\      /\      /\      /\      /\
                        |/  \    /  \    /  \    /  \    /
                        |    \  /    \  /    \  /    \  /
                        |     \/      \/      \/      \/
                        +------------------------------------> x
                          -3  -2  -1   0  +1  +2  +3  +4  +5
-----------------------------------------------------------------------------*/

#undef  PRED01
#define PRED01(x) fabs( (x) - 2.0*floor(0.5*((x)+1.0)) )

static int     scalx = 0    ;  /* whether to use scaling and constraints */
static double *sxmin = NULL ;
static double *sxsiz = NULL ;
static double *sx    = NULL ;

/*---------------------------------------------------------------------------*/

static double (*userfun)( int n , double *x ) = NULL ;

int calfun_(integer *n, doublereal *x, doublereal *fun)
{
   double val ;
   if( scalx ){            /* in this case, inputs x[] are in range 0..1,  */
     int ii ;              /* and need to be scaled to their 'true' values */
     for( ii=0 ; ii < *n ; ii++ )
       sx[ii] = sxmin[ii] + sxsiz[ii]*PRED01(x[ii]) ;
     val = userfun( (int)(*n) , sx ) ;
   } else {
     val = userfun( (int)(*n) , (double *)x ) ;
   }
   *fun = (doublereal)val ;
   return ;
}

/*---------------------------------------------------------------------------*/

static float mfac = 2.0f ;   /* default number of      */
static float afac = 3.0f ;   /* sample points is 2*n+3 */

void powell_set_mfac( float mm , float aa )
{
  if( mm >= 2.0f ) mfac = mm ;
  if( aa >= 0.0f ) afac = aa ;
}

/*---------------------------------------------------------------------------*/
/*! Driver for Powell's general purpose minimization newuoa function:
    - ndim   = number of variables in function to be minimized
    - x      = array [ndim] of variables (input and output)
    - rstart = size of initial search region around initial value of x
    - rend   = size of final search region = desired accuracy
    - maxcall = max number of times to call ufunc
    - ufunc   = function to minimize: inputs are number of variables (n)
                and current array of variables (x) at which to evaluate

    Return value is number of function calls to ufunc actually done.
    If return is negative, something bad happened.

    MJD Powell, "The NEWUOA software for unconstrained optimization without
    derivatives", Technical report DAMTP 2004/NA08, Cambridge University
    Numerical Analysis Group -- http://www.damtp.cam.ac.uk/user/na/reports.html

    For constrained optimization (via trickery), see powell_newuoa_int().
------------------------------------------------------------------------------*/

int powell_newuoa( int ndim , double *x ,
                   double rstart , double rend ,
                   int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun ;
   doublereal rhobeg , rhoend , *w ;

   if( ndim < 1                         ) return -2 ;
   if( x == NULL                        ) return -3 ;
   if( rstart < rend || rstart <= 1.e-4 ) return -4 ;
   if( ufunc == NULL                    ) return -5 ;

   if( rend    <= 0.0       ) rend    = 1.e-4 * rstart ;
   if( maxcall <  10+5*ndim ) maxcall = 10+5*ndim ;

   n      = ndim ;
   npt    = (int)(mfac*n+afac) ;
   icode  = (n+1)*(n+2)/2 ; if( npt > icode ) npt = icode ;
   maxfun = maxcall ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 666 ;
   w       = (doublereal *)malloc(sizeof(doublereal)*icode) ; /* workspace */
   icode   = 0 ;
   userfun = ufunc ;
   scalx   = 0 ;

   (void)newuoa_( &n , &npt , (doublereal *)x ,
                  &rhobeg , &rhoend , &maxfun , w , &icode ) ;

   free((void *)w) ;
   return icode ;
}

/*---------------------------------------------------------------------------*/
/*! Similar to powell_newuoa(), but with constraints on the variables,
    and (if nrand > 0) a random search for the starting point.
-----------------------------------------------------------------------------*/

int powell_newuoa_con( int ndim , double *x , double *xbot , double *xtop ,
                       int nrand ,
                       double rstart , double rend ,
                       int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun ;
   doublereal rhobeg , rhoend , *w ;
   int ii ; double *x01 ;

   if( ndim < 1                         ) return -2 ;
   if( x == NULL                        ) return -3 ;
   if( rstart < rend || rstart <= 1.e-4 ) return -4 ;
   if( ufunc == NULL                    ) return -5 ;
   if( xbot == NULL || xtop == NULL     ) return -6 ;

   if( rend    <= 0.0       ) rend    = 1.e-4 * rstart ;
   if( maxcall <  10+5*ndim ) maxcall = 10+5*ndim ;

   n      = ndim ;
   npt    = (int)(mfac*n+afac) ;
   icode  = (n+1)*(n+2)/2 ; if( npt > icode ) npt = icode ;
   maxfun = maxcall ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 666 ;
   w       = (doublereal *)malloc(sizeof(doublereal)*icode) ; /* workspace */
   icode   = 0 ;
   userfun = ufunc ;

   /*-- To enforce constraints:
        (a) scale each variable to be in the range 0..1, in x01[] array
        (b) in calfun_(), if an input variable drifts outside the 0..1
            range, bring it back into that range with the PRED01() macro
        (c) then scale that 0..1 value back up to the 'true' value
            before calling ufunc to evaluate the objective function. -------*/

   scalx   = 1 ;
   sxmin = (double *)malloc(sizeof(double)*ndim) ;
   sxsiz = (double *)malloc(sizeof(double)*ndim) ;
   sx    = (double *)malloc(sizeof(double)*ndim) ;
   x01   = (double *)malloc(sizeof(double)*ndim) ;
   for( ii=0 ; ii < ndim ; ii++ ){
     sxmin[ii] = xbot[ii] ;
     sxsiz[ii] = xtop[ii] - xbot[ii]; if( sxsiz[ii] <= 0.0 ) sxsiz[ii] = 1.0;
     x01[ii]   = (x[ii] - sxmin[ii]) / sxsiz[ii] ;
   }

   /*-- random search for the best starting point? --*/

   if( nrand > 0 ){
     double *xbest , *xtest , fbest , ftest ; int qq ;
     static int seed=1 ;
     if( seed ){ srand48((long)time(NULL)); seed=0; }
     xbest = (double *)malloc(sizeof(double)*ndim) ;
     xtest = (double *)malloc(sizeof(double)*ndim) ;
     memcpy(xbest,x01,sizeof(double)*ndim) ;
     (void)calfun_(&n,xbest,&fbest) ;
     for( qq=0 ; qq < nrand ; qq++ ){
       for( ii=0 ; ii < ndim ; ii++ ) xtest[ii] = drand48() ;
       (void)calfun_(&n,xtest,&ftest) ;
       if( ftest < fbest ){
         fbest = ftest; memcpy(xbest,xtest,sizeof(double)*ndim);
       }
     }
     memcpy(x01,xbest,sizeof(double)*ndim) ;
     free((void *)xtest); free((void *)xbest);
   }

   /****** optimize the scaled variables ******/

   (void)newuoa_( &n , &npt , (doublereal *)x01 ,
                  &rhobeg , &rhoend , &maxfun , w , &icode ) ;

   /*-- Rescale output back to 'true' range --*/

   for( ii=0 ; ii < ndim ; ii++ )
     x[ii] = sxmin[ii] + PRED01(x01[ii]) * sxsiz[ii] ;

   free((void *)x01); free((void *)sx); free((void *)sxsiz); free((void *)sxmin);
   sx = sxmin = sxsiz = NULL ; scalx = 0 ;
   free((void *)w) ;
   return icode ;
}
