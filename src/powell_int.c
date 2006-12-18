#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <cs.h>
#include "f2c.h"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#undef  BIGVAL
#define BIGVAL 1.e+38

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

/*! Pointer to user-supplied function. */

static double (*userfun)( int n , double *x ) = NULL ;

/*! Function called by newuoa_();
    goal is to minimize this as a function of x[0..n-1] */

int calfun_(integer *n, doublereal *x, doublereal *fun)
{
   double val ;

   if( scalx ){            /* in this case, inputs x[] are in range 0..1,  */
     int ii ;              /* and need to be scaled to their 'true' values */
     for( ii=0 ; ii < *n ; ii++ ) sx[ii] = sxmin[ii] + sxsiz[ii]*PRED01(x[ii]);
     val = userfun( (int)(*n) , sx ) ;           /* scaled x[] */
   } else {
     val = userfun( (int)(*n) , (double *)x ) ;  /* unscaled x[] */
   }
   *fun = (doublereal)val ;
   return ;
}

/*---------------------------------------------------------------------------*/

static float mfac = 2.0f ;   /* default number of      */
static float afac = 3.0f ;   /* sample points is 2*n+3 */

void powell_set_mfac( float mm , float aa )
{
  if( mm >= 1.0f ){ mfac = mm   ; afac = aa   ; }  /* set */
  else            { mfac = 2.0f ; afac = 3.0f ; }  /* reset */
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

    For constrained optimization (via trickery), see powell_newuoa_con() [OLD]
    and powell_newuoa_constrained() [NEW].
------------------------------------------------------------------------------*/

int powell_newuoa( int ndim , double *x ,
                   double rstart , double rend ,
                   int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun ;
   doublereal rhobeg , rhoend , *w ;

   /* check inputs */

   if( ndim < 1                         ) return -2 ;
   if( x == NULL                        ) return -3 ;
   if( rstart < rend || rstart <= 1.e-4 ) return -4 ;
   if( ufunc == NULL                    ) return -5 ;

   if( rend    <= 0.0       ) rend    = 1.e-4 * rstart ;
   if( maxcall <  10+5*ndim ) maxcall = 10+5*ndim ;

   n      = ndim ;
   npt    = (int)(mfac*n+afac) ; if( npt < n+2   ) npt = n+2 ;
   icode  = (n+1)*(n+2)/2      ; if( npt > icode ) npt = icode ;
   maxfun = maxcall ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 666 ;
   w       = (doublereal *)malloc(sizeof(doublereal)*icode) ; /* workspace */
   icode   = 0 ;
   userfun = ufunc ;
   scalx   = 0 ;

   /* do the work: best params are put back into x[] */

   (void)newuoa_( &n , &npt , (doublereal *)x ,
                  &rhobeg , &rhoend , &maxfun , w , &icode ) ;

   free((void *)w) ;
   return icode ;  /* number of function calls */
}

/*---------------------------------------------------------------------------*/
/*! Similar to powell_newuoa(), but with constraints on the variables,
    and (if nrand > 0) a random search for the starting point (the initial
    point on input in x[] is also tested to see if it is 'best' for starting).
    Note the units on rstart and rend are relative (between 0 and 1).

    Also see the newer powell_newuoa_constrained(), which has more options!
-----------------------------------------------------------------------------*/

int powell_newuoa_con( int ndim , double *x , double *xbot , double *xtop ,
                       int nrand ,
                       double rstart , double rend ,
                       int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun ;
   doublereal rhobeg , rhoend , *w ;
   int ii ; double *x01 ;

   /* check inputs */

   if( ndim < 1                         ) return -2 ;
   if( x == NULL                        ) return -3 ;
   if( rstart < rend || rstart <= 1.e-4 ) return -4 ;
   if( ufunc == NULL                    ) return -5 ;
   if( xbot == NULL || xtop == NULL     ) return -6 ;

   if( rend    <= 0.0       ) rend    = 1.e-4 * rstart ;
   if( maxcall <  10+5*ndim ) maxcall = 10+5*ndim ;

   n      = ndim ;
   npt    = (int)(mfac*n+afac) ; if( npt < n+2   ) npt = n+2 ;
   icode  = (n+1)*(n+2)/2      ; if( npt > icode ) npt = icode ;
   maxfun = maxcall ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 666 ;
   w       = (doublereal *)malloc(sizeof(doublereal)*icode) ; /* workspace */
   icode   = 0 ;
   userfun = ufunc ;

   /*-- To enforce constraints:
        (a) scale each variable to be in the range 0..1, in x01[] array;
        (b) in calfun_(), if an input variable drifts outside the 0..1
            range, bring it back into that range with the PRED01() macro;
        (c) then scale that 0..1 value back to the 'true' value
            before calling ufunc() to evaluate objective function. -------*/

   scalx   = 1 ;
   sxmin = (double *)malloc(sizeof(double)*ndim) ;  /* copy of xbot */
   sxsiz = (double *)malloc(sizeof(double)*ndim) ;  /* xtop - xbot */
   sx    = (double *)malloc(sizeof(double)*ndim) ;
   x01   = (double *)malloc(sizeof(double)*ndim) ;  /* scaled x[] */
   for( ii=0 ; ii < ndim ; ii++ ){
     sxmin[ii] = xbot[ii] ;
     sxsiz[ii] = xtop[ii] - xbot[ii]; if( sxsiz[ii] <= 0.0 ) sxsiz[ii] = 1.0;
     x01[ii]   = (x[ii] - sxmin[ii]) / sxsiz[ii] ;
     x01[ii]   = PRED01(x01[ii]) ;           /* make sure is in range 0..1 */
   }

   /*-- do a random search for the best starting point? --*/

   if( nrand > 0 ){
     double *xbest , *xtest , fbest , ftest ; int qq ;
     static int seed=1 ;
     if( seed ){ srand48((long)time(NULL)+(long)getpid()); seed=0; }
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

/*---------------------------------------------------------------------------*/
/*! Driver for Powell's general purpose minimization newuoa function:

    - ndim   = number of variables in function to be minimized
    - x      = array [ndim] of variables (input and output)
    - cost   = pointer to value to hold output cost function: ufunc(output x[])
    - xbot   = array [ndim] of minimum values for the x values
    - xtop   = array [ndim] of maximum values for the x values
    - nrand  = if nrand > 0, then it is the number of random points
               inside the constraint xbot..xtop region to evaluate
               as potential starting vectors, in addition to the vector x[]
    - nkeep  = the number of random vectors to keep for the first round
               of optimization (in addition to the initial x[] vector)
    - ntry   = the number of vectors to keep for the second round of
               optimization
    - To optimize ONLY starting at input x[] vector: input nrand=nkeep=ntry=0
    - rstart = size of initial search region around initial value of x
               units are relative to the search size xbot..xtop
               (if input as 0, 0.10 will be used)
    - rend   = size of final search region = desired accuracy
               (if input as 0, 0.0001 will be used)
    - maxcall= max number of times to call ufunc in any one newuoa() call
               in the second round of optimization
    - ufunc  = function to minimize: inputs are number of variables (n)
               and current array of variables (x) at which to evaluate

    Return value is number of function calls to ufunc actually done.
    If return is negative, something bad happened.

    The best vector round in the second round of optimization is returned
    in the x array.

    MJD Powell, "The NEWUOA software for unconstrained optimization without
    derivatives", Technical report DAMTP 2004/NA08, Cambridge University
    Numerical Analysis Group -- http://www.damtp.cam.ac.uk/user/na/reports.html
------------------------------------------------------------------------------*/

int powell_newuoa_constrained( int ndim, double *x, double *cost ,
                               double *xbot, double *xtop ,
                               int nrand, int nkeep, int ntry ,
                               double rstart , double rend ,
                               int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun ;
   doublereal rhobeg , rhoend , *w ;
   int ii,tt , tbest , ntot=0 , nx01 ;
   double **x01 , *x01val , vbest ;

   /*--- check inputs for stupidity ---*/

   if( ndim < 1                     ) return -2 ;
   if( x == NULL                    ) return -3 ;
   if( ufunc == NULL                ) return -5 ;
   if( xbot == NULL || xtop == NULL ) return -6 ;

   /*--- edit inputs for idiocy ---*/

   if( rstart <= rend || rstart <= 1.e-4 || rstart > 0.333 ){
     rstart = 0.1 ; rend = 1.e-4 ;
   }

   if( maxcall < 10+5*ndim ) maxcall = 10+5*ndim ;

   if( ntry  < 1 )                     ntry  = 1 ;
   if( nkeep < 1 && nrand > 0 )        nkeep = 1 ;
   if( nkeep > 0 && nrand <= 2*nkeep ) nrand = 2*nkeep+1 ;

   /*--- set up newuoa parameters and workspace ---*/

   n      = ndim ;
   npt    = (int)(mfac*n+afac) ; if( npt < n+2   ) npt = n+2 ;
   icode  = (n+1)*(n+2)/2      ; if( npt > icode ) npt = icode ;
   maxfun = maxcall ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 666 ;
   w       = (doublereal *)malloc(sizeof(doublereal)*icode) ; /* workspace */
   icode   = 0 ;
   userfun = ufunc ;

   /*-- To enforce constraints:
        (a) scale each variable to be in the range 0..1, in x01[] array;
        (b) in calfun_(), if an input variable drifts outside the 0..1
            range, bring it back into that range with the PRED01() macro;
        (c) then scale that 0..1 value back to the 'true' value
            before calling ufunc() to evaluate objective function. -------*/

   scalx = 1 ;                       /* signal to calfun_() to apply scaling */
   sxmin = (double *)malloc(sizeof(double)*ndim) ;  /* copy xbot for calfun_ */
   sxsiz = (double *)malloc(sizeof(double)*ndim) ;  /* = xtop - xbot */
   sx    = (double *)malloc(sizeof(double)*ndim) ;  /* also for calfun_ */
   for( ii=0 ; ii < ndim ; ii++ ){
     sxmin[ii] = xbot[ii] ;
     sxsiz[ii] = xtop[ii] - xbot[ii]; if( sxsiz[ii] <= 0.0 ) sxsiz[ii] = 1.0 ;
   }

   /*-- set up the first starting point from x[] array --*/

   nx01 = nkeep+2 ;
   x01  = (double **)malloc(sizeof(double *)*nx01) ;   /* array of arrays */
   for( tt=0 ; tt < nx01 ; tt++ )
     x01[tt] = (double *)malloc(sizeof(double)*ndim) ; /* tt-th keeper */
   x01val = (double *)malloc(sizeof(double)*nx01) ;    /* cost func values */
   for( tt=0 ; tt < nx01 ; tt++ ) x01val[tt] = BIGVAL; /* mark as unready */

   for( ii=0 ; ii < ndim ; ii++ ){
     x01[0][ii] = (x[ii] - sxmin[ii]) / sxsiz[ii] ;
     x01[0][ii] = PRED01(x01[0][ii]) ;  /* make sure is in range 0..1 */
   }
   (void)calfun_(&n,x01[0],x01val+0) ;  /* value of keeper #0 = input point */
   ntot++ ;

   /*-- do a random search for the best starting point? --*/

   if( nrand > 0 ){
     double *xtest , ftest , rb,re ;
     int qq,jj ; integer mf ;
     static int seed=1 ;

     if( seed ){ srand48((long)time(NULL)+(long)getpid()); seed=0; }
     xtest = (double *)malloc(sizeof(double)*ndim) ;

     /* Step 1: search nrand start points, keeping the
                nkeep-th best values we find on the way.
                N.B.: we do NOT displace the input vector in x01[0];
                      therefore, there are nkeep+1 vectors being kept */

     for( qq=0 ; qq < nrand ; qq++ ){
       for( ii=0 ; ii < ndim ; ii++ ) xtest[ii] = drand48() ;    /* random pt */
       (void)calfun_(&n,xtest,&ftest) ; ntot++ ;            /* eval cost func */
       for( tt=1 ; tt <= nkeep ; tt++ ){          /* is this better than what */
         if( ftest < x01val[tt] ){                    /* we've seen thus far? */
           for( jj=nkeep-1 ; jj >= tt ; jj-- ){    /* push those above #tt up */
             memcpy( x01[jj+1] , x01[jj] , sizeof(double)*ndim ) ; /* in list */
             x01val[jj+1] = x01val[jj] ;
           }
           memcpy( x01[tt] , xtest , sizeof(double)*ndim ) ;  /* save in list */
           x01val[tt] = ftest ;
           break ;     /* breaking out of 'tt' loop, having put xtest in list */
         }
       }
     } /* end of random search loop */

     for( tt=0 ; tt <= nkeep && x01val[tt] < BIGVAL ; tt++ ) ; /* nada */
     nkeep = tt ;   /** from now on, nkeep = actual number of keepers **/

#if 0
     /* add central point in for balance */
     for( ii=0 ; ii < ndim ; ii++ ) x01[nkeep][ii] = 0.5 ;
     (void)calfun_(&n,x01[nkeep],x01val+nkeep) ; ntot++ ; nkeep++ ;
#endif

     /* Step 2: do a little optimization on each of the keepers */

     rb = 0.05 ; re = 0.005 ; mf = 9*ndim+7 ;
     for( tt=0 ; tt < nkeep ; tt++ ){
       (void)newuoa_( &n, &npt, (doublereal *)x01[tt], &rb,&re,&mf,w,&icode ) ;
       for( ii=0 ; ii < ndim ; ii++ ) x01[tt][ii] = PRED01(x01[tt][ii]) ;
       (void)calfun_(&n,x01[tt],x01val+tt) ; ntot += icode+1 ;
     }

     /* Step 2a: sort results by new x01val costs */

     qsort_doublestuff( nkeep , x01val , (void **)x01 ) ;
     free((void *)xtest) ;
     if( ntry > nkeep ) ntry = nkeep ;  /* should not happen */
   }

   /****** fully optimize each of the first ntry-th vectors in x01[] ******/

   tbest = 0 ; vbest = BIGVAL ;
   for( tt=0 ; tt < ntry ; tt++ ){
     (void)newuoa_( &n , &npt , (doublereal *)x01[tt] ,
                    &rhobeg , &rhoend , &maxfun , w , &icode ) ;
     (void)calfun_(&n,x01[tt],x01val+tt) ; ntot += icode+1 ;
     if( x01val[tt] < vbest ){ vbest = x01val[tt]; tbest = tt; }
   }

   /*-- Rescale output back to 'true' range --*/

   for( ii=0 ; ii < ndim ; ii++ )
     x[ii] = sxmin[ii] + PRED01(x01[tbest][ii]) * sxsiz[ii] ;
   if( cost != NULL ) *cost = vbest ;    /* save cost func */

   /*-- toss the trash, and vamoose the ranch --*/

   free((void *)sx); free((void *)sxsiz); free((void *)sxmin);
   sx = sxmin = sxsiz = NULL ; scalx = 0 ;
   for( tt=0 ; tt < nx01 ; tt++ ) free((void *)x01[tt]) ;
   free((void *)x01val); free((void *)x01); free((void *)w) ;

   return ntot ;
}
