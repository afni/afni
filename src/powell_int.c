#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <cs.h>
#include "mrilib.h"
#include "f2c.h"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#undef  BIGVAL
#define BIGVAL 1.e+38   /* this is a big number */

#undef MAX
#define MAX(a,b) (((a)<(b)) ? (b) : (a))    /* not von Sydow, not Mad */


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

#define SC_BOX  1
#define SC_BALL 2
#define SC_L4   4  /* L4 ball [27 Oct 2015] */

static int     scalx = 0    ;  /* whether to use scaling and constraints */
static double *sxmin = NULL ;  /* smallest allowed value */
static double *sxsiz = NULL ;  /* sxmin+sxsize = largest allowed value */
static double *sx    = NULL ;  /* temp space to store scaled values */

static int      mapx = 0    ;  /* if > 0, number of total parameters */
static double *mapar = NULL ;  /* array holding all parameters */
static int    *mapin = NULL ;  /* index array for mapping */

/*---------------------------------------------------------------------------*/

/*! Pointer to user-supplied function that does actual work in calfun_(). */

static double (*userfun)( int n , double *x ) = NULL ;

/*---------------------------------------------------------------------------*/
/*! Reduce x[ii] (all in 0..1) to be inside the ball [08 Jan 2015] */

static void xreduce( int n , double *x )
{
   int ii ;

   for( ii=0 ; ii < n ; ii++ ){
     if( !isfinite(x[ii]) || x[ii] < -9.9f || x[ii] > 9.9f ) x[ii] = 0.5f ;
     else                                                    x[ii] = PRED01(x[ii]) ;
   }
   if( scalx == SC_BALL ){
     double rad=0.0 ;
     for( ii=0 ; ii < n ; ii++ ) rad += (x[ii]-0.5)*(x[ii]-0.5) ;
     if( rad > 0.25 ){
       rad = 0.25 / rad ;
       for( ii=0 ; ii < n ; ii++ ) x[ii] = 0.5 + (x[ii]-0.5)*rad ;
     }
   }
#if 0
   if( scalx == SC_L4 ){
     double rad=0.0 , xx ;
     for( ii=0 ; ii < n ; ii++ ){ xx = x[ii]-0.5; rad += xx*xx*xx*xx; }
     if( rad > 0.0625 ){
       rad = 0.0625 / rad ;
       for( ii=0 ; ii < n ; ii++ ) x[ii] = 0.5 + (x[ii]-0.5)*rad ;
     }
   }
#endif
   return ;
   return ;
}

/*---------------------------------------------------------------------------*/
/*! Function called by newuoa_();
    goal is to minimize this as a function of x[0..n-1] */

int calfun_(integer *n, doublereal *x, doublereal *fun)
{
   double val ;

   if( scalx == SC_BOX ){  /* in this case, inputs x[] are in range 0..1,  */
     int ii ;              /* and need to be scaled to their 'true' values */

     for( ii=0 ; ii < *n ; ii++ ){
       if( !isfinite(x[ii]) || x[ii] < -9.9 || x[ii] > 9.9 ){
         fprintf(stderr,"** ERROR: calfun[%d]=%g --> 0\n",ii,x[ii]) ; x[ii] = 0.5 ;
       }
       sx[ii] = sxmin[ii] + sxsiz[ii]*PRED01(x[ii]);
     }

     val = userfun( (int)(*n) , sx ) ;           /* input = scaled x[] */

   } else if( scalx == SC_BALL ){  /* scale into a ball, not a box [08 Jan 2015] */
     int ii ; double rad=0.0 ;

     for( ii=0 ; ii < *n ; ii++ ){
       if( !isfinite(x[ii]) || x[ii] < -9.9 || x[ii] > 9.9 ){
         fprintf(stderr,"** ERROR: calfun[%d]=%g --> 0\n",ii,x[ii]) ; x[ii] = 0.5 ;
       }
       rad += (x[ii]-0.5)*(x[ii]-0.5) ;
     }
     if( rad <= 0.25 ){             /* inside the ball */
       for( ii=0 ; ii < *n ; ii++ )
         sx[ii] = sxmin[ii] + sxsiz[ii]*x[ii] ;
     } else {                       /* outside the ball */
       rad = 0.25 / rad ;
       for( ii=0 ; ii < *n ; ii++ )
         sx[ii] = sxmin[ii] + sxsiz[ii]*(0.5 + (x[ii]-0.5)*rad) ;
     }

     val = userfun( (int)(*n) , sx ) ;           /* input = scaled x[] */

#if 0
   } else if( scalx == SC_L4 ){
     int ii ; double rad=0.0 , xx ;

     for( ii=0 ; ii < *n ; ii++ ){
       if( !isfinite(x[ii]) || x[ii] < -9.9 || x[ii] > 9.9 ){
         fprintf(stderr,"** ERROR: calfun[%d]=%g --> 0\n",ii,x[ii]) ; x[ii] = 0.5 ;
       }
       xx = x[ii]-0.5 ; rad += xx*xx*xx*xx ;
     }
     if( rad <= 0.0625 ){             /* inside the ball */
       for( ii=0 ; ii < *n ; ii++ )
         sx[ii] = sxmin[ii] + sxsiz[ii]*x[ii] ;
     } else {                         /* outside the ball */
       rad = 0.0625 / rad ;
       for( ii=0 ; ii < *n ; ii++ )
         sx[ii] = sxmin[ii] + sxsiz[ii]*(0.5 + (x[ii]-0.5)*rad) ;
     }

     val = userfun( (int)(*n) , sx ) ;           /* input = scaled x[] */
#endif

   } else if( mapx ){      /* in this case, the parameters given as input */
     int ii ;              /* are just a subset of all the parameters, so */
                           /* we must expand the array to the full size.  */
     for( ii=0 ; ii < *n ; ii++ ) mapar[mapin[ii]] = x[ii] ;
     val = userfun( mapx , mapar ) ;
   } else {

     val = userfun( (int)(*n) , (double *)x ) ;  /* input = unscaled x[] */
   }

   *fun = (doublereal)val ;
   return 0 ;
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

static int verb = 0 ;
void powell_set_verbose( int v ){ verb = v; }

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

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 6666 ;
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

static int con_meth = SC_BOX ;

void powell_newuoa_set_con_box (void){ con_meth = SC_BOX ; }
void powell_newuoa_set_con_ball(void){ con_meth = SC_BALL; }
void powell_newuoa_set_con_L4  (void){ con_meth = SC_L4  ; }
int  powell_newuoa_get_con     (void){ return con_meth ;   }
void powell_newuoa_set_con     (int c){ con_meth = c ;     }

/*---------------------------------------------------------------------------*/
/*! Similar to powell_newuoa(), but with constraints on the variables,
    and (if nrand > 0) a random search for the starting vector (the initial
    vector on input in x[] is also tested to see if it is 'best' for starting).
    Note the units on rstart and rend are relative (between 0 and 1).

    Also see the newer powell_newuoa_constrained(), which has more options!
-----------------------------------------------------------------------------*/

int powell_newuoa_con( int ndim , double *x , double *xbot , double *xtop ,
                       int nrand ,
                       double rstart , double rend ,
                       int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun , ncall=0 ;
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

   /*** The above comment describes the original SC_BOX method of
        dealing with constraints.  The newer SC_BALL method keeps the
        scaled parameters inside the ball of radius 0.5 centered at
        x=0.5 -- this is the L2-ball, rather than the 'Linfinity-ball'
        that SC_BOX enforces, and this ball is inside the SC_BOX bounds. ***/

   scalx = con_meth ;
   sxmin = (double *)malloc(sizeof(double)*ndim) ;  /* copy of xbot */
   sxsiz = (double *)malloc(sizeof(double)*ndim) ;  /* xtop - xbot */
   sx    = (double *)malloc(sizeof(double)*ndim) ;
   x01   = (double *)malloc(sizeof(double)*ndim) ;  /* scaled x[] */
   for( ii=0 ; ii < ndim ; ii++ ){
     sxmin[ii] = xbot[ii] ;
     sxsiz[ii] = xtop[ii] - xbot[ii]; if( sxsiz[ii] <= 0.0 ) sxsiz[ii] = 1.0;
     x01[ii]   = (x[ii] - sxmin[ii]) / sxsiz[ii] ;
   }
   xreduce( ndim , x01 ) ;  /* make sure is in the legal range */

   /*-- do a random search for the best starting vector? --*/

   if( nrand > 0 ){
     double *xbest , *xtest , fbest , ftest ; int qq ;
     static int seed=1 ;
     if( seed ){ srand48((long)time(NULL)+(long)getpid()); seed=0; }
     xbest = (double *)malloc(sizeof(double)*ndim) ;
     xtest = (double *)malloc(sizeof(double)*ndim) ;
     memcpy(xbest,x01,sizeof(double)*ndim) ;
     (void)calfun_(&n,xbest,&fbest) ; ncall++ ;
     for( qq=0 ; qq < nrand ; qq++ ){
       for( ii=0 ; ii < ndim ; ii++ ) xtest[ii] = drand48() ;
       if( scalx != SC_BOX ) xreduce( ndim, xtest ) ;
       (void)calfun_(&n,xtest,&ftest) ; ncall++ ;
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

#if 0
   if( icode > 0 ) icode += ncall ;
#endif

   /*-- Rescale output back to 'true' range --*/

   xreduce( ndim , x01 ) ;
   for( ii=0 ; ii < ndim ; ii++ )
     x[ii] = sxmin[ii] + x01[ii] * sxsiz[ii] ;

   if( verb ){
     fprintf(stderr," +   output param:") ;
     for( ii=0 ; ii < ndim ; ii++ ) fprintf(stderr," %g",x[ii]) ;
     fprintf(stderr,"\n") ;
   }

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
    - nrand  = if nrand > 0, then it is the number of random vectors
               inside the constraint xbot..xtop region to evaluate
               as potential starting vectors, in addition to the vector x[]
    - nkeep  = the number of random vectors to keep for the first round
               of optimization (in addition to the initial x[] vector)
    - ntry   = the number of vectors to keep for the second round of
               optimization
    - rstart = size of initial search region around initial value of x
               units are relative to the search size xbot..xtop
               (if input as 0, 0.10 will be used)
    - rend   = size of final search region = desired accuracy
               (if input as 0, 0.0001 will be used)
    - maxcall= max number of times to call ufunc in any one newuoa() call
               in the second round of optimization
    - ufunc  = function to minimize: inputs are number of variables (n)
               and current array of variables (x) at which to evaluate
    - Sequence of calculations:
      - Evaluate ufunc() at the input x[] vector
      - Also evaluate ufunc() at nrand random vectors in the constraint
        domain xbot..xtop
      - Keep the nkeep+1 best random vectors plus the input x[] vector,
        and do a little bit of NEWUOA optimization on each of them
        (this is the first round optimization) -- however, vectors
        that are too close to each other will not be used (cf. DTHRESH)
      - From those nkeep+1 vectors output by the first round, do the
        full optimization (this is the second round) on the ntry best
        starting vectors
      - The output vector of the second round that has the smallest value
        of ufunc() is returned in x[]; the corresponding value of ufunc()
        is returned in *cost
      - To optimize ONLY starting at input x[] vector: input nrand=nkeep=ntry=0;
        this will actually just call powell_newuoa_con() to do the work.
      - Tentative suggestion: nrand=13*ndim nkeep=15 ntry=5; however, it
        is hard to have a general rule, since cost functions can be funky.

    Return value is number of function calls to ufunc actually done.
    If return is negative, something bad happened.

    The best vector round in the second round of optimization is returned
    in the x[] array.

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

   /* if this call is totally vanilla, then call the older function */

   if( nrand == 0 && nkeep == 0 && ntry < 2 )
     return powell_newuoa_con( ndim , x,xbot,xtop ,
                               0 , rstart,rend , maxcall , ufunc ) ;

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
   userfun = ufunc ;  /* store pointer to user's function in global variable */

   /*-- To enforce constraints:
        (a) scale each variable to be in the range 0..1, in x01[] array;
        (b) in calfun_(), if an input variable drifts outside the 0..1
            range, bring it back into that range with the PRED01() macro;
        (c) then scale that 0..1 value back to the 'true' value
            before calling ufunc() to evaluate objective function. -------*/

   scalx = con_meth ;                /* signal to calfun_() to apply scaling */
   sxmin = (double *)malloc(sizeof(double)*ndim) ;  /* copy xbot for calfun_ */
   sxsiz = (double *)malloc(sizeof(double)*ndim) ;  /* = xtop - xbot */
   sx    = (double *)malloc(sizeof(double)*ndim) ;  /* workspace for calfun_ */
   for( ii=0 ; ii < ndim ; ii++ ){
     sxmin[ii] = xbot[ii] ;
     sxsiz[ii] = xtop[ii] - xbot[ii]; if( sxsiz[ii] <= 0.0 ) sxsiz[ii] = 1.0 ;
   }

   /*-- set up the first starting vector from x[] array --*/

   nx01 = nkeep+2 ;
   x01  = (double **)malloc(sizeof(double *)*nx01) ;   /* array of vectors */
   for( tt=0 ; tt < nx01 ; tt++ )
     x01[tt] = (double *)malloc(sizeof(double)*ndim) ; /* tt-th keeper vector */
   x01val = (double *)malloc(sizeof(double)*nx01) ;    /* cost func values */
   for( tt=0 ; tt < nx01 ; tt++ ) x01val[tt] = BIGVAL; /* mark as unready */

   /* copy x[] into x01[0], scaling to 0..1 range */

   for( ii=0 ; ii < ndim ; ii++ ){
     x01[0][ii] = (x[ii] - sxmin[ii]) / sxsiz[ii] ;
     x01[0][ii] = PRED01(x01[0][ii]) ;  /* make sure is in range 0..1 */
   }
   if( scalx != SC_BOX ) xreduce( ndim, x01[0] ) ;
   (void)calfun_(&n,x01[0],x01val+0) ;  /* value of keeper #0 = input vector */
   ntot++ ;                           /* number of times calfun_() is called */

   /*-- do a random search for the best starting vector? --*/

   if( nrand > 0 ){
     double *xtest , *qpar,*cpar , ftest , rb,re , dist ;
     int qq,jj ; integer mf ;
     static int seed=1 ;

     if( seed ){ srand48((long)time(NULL)+(long)getpid()); seed=0; }
     xtest = (double *)malloc(sizeof(double)*ndim) ;

     /* Step 1: search nrand start vectors, keeping the
                nkeep-th best values we find on the way.
                N.B.: we do NOT displace the input vector in x01[0];
                      therefore, there are nkeep+1 vectors being kept */

     if( verb )
       INFO_message("Powell: random search of %d vectors in %d-dim space",nrand,ndim);

     for( qq=0 ; qq < nrand ; qq++ ){
       for( ii=0 ; ii < ndim ; ii++ ) xtest[ii] = drand48() ;    /* random pt */
       if( scalx != SC_BOX ) xreduce( ndim, xtest ) ;
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
     free((void *)xtest) ;  /* don't need this no more */

     /* count number that have valid cost function results */

     for( tt=0 ; tt <= nkeep && x01val[tt] < BIGVAL ; tt++ ) ; /* nada */
     nkeep = tt ;   /** from now on, nkeep = actual number of keepers **/

     /* Step 2a: do a little first round optimization on each of the keepers */

     if( verb )
       INFO_message("Powell: 1st round optimization on %d vectors",nkeep);

     rb = 0.05 ; re = 0.005 ; mf = 9*ndim+7 ; tbest = 0 ; vbest = BIGVAL ;
     for( tt=0 ; tt < nkeep ; tt++ ){
       (void)newuoa_( &n, &npt, (doublereal *)x01[tt], &rb,&re,&mf,w,&icode ) ;
       for( ii=0 ; ii < ndim ; ii++ ) x01[tt][ii] = PRED01(x01[tt][ii]) ;
       if( scalx != SC_BOX ) xreduce(ndim,x01[tt]) ;
       (void)calfun_(&n,x01[tt],x01val+tt) ; ntot += icode+1 ;
       if( x01val[tt] < vbest ){ vbest = x01val[tt]; tbest = tt; }
       if( verb > 1 )
         ININFO_message("%2d: cost = %g %c nfunc=%d",tt,x01val[tt],(tbest==tt)?'*':' ',icode) ;
     }

     /* Step 2b: sort results by new x01val costs */

     qsort_doublestuff( nkeep , x01val , (void **)x01 ) ;

     /* Step 2c: cast out those that are too close to better vectors
                 in the max-norm (we always keep the best vector in x01[0]) */

#undef  DTHRESH
#define DTHRESH 0.05    /* max-norm distance threshold for vector reject */

     for( tt=1 ; tt < nkeep ; tt++ ){
       qpar = x01[tt] ;                       /* do we keep this vector? */
       for( jj=0 ; jj < tt ; jj++ ){       /* loop over previous keepers */
         if( x01val[jj] >= BIGVAL ) continue ;       /* already rejected */
         cpar = x01[jj] ;                        /* compare qpar to cpar */
         for( dist=0.0,ii=0 ; ii < ndim ; ii++ ){
           re = fabs(cpar[ii]-qpar[ii]) ; dist = MAX(dist,re) ;
         }
         if( dist < DTHRESH ){              /* qpar is too close to cpar */
           x01val[tt] = BIGVAL ; break ;           /* reject qpar vector */
         }
       }
     }

     /* Step 2d: sort again (so that the rejected ones rise to the top) */

     qsort_doublestuff( nkeep , x01val , (void **)x01 ) ;

     for( tt=0 ; tt <= nkeep && x01val[tt] < BIGVAL ; tt++ ) ; /* nada */
     nkeep = tt ;  /* number of keepers that weren't rejected above */

     if( ntry > nkeep ) ntry = nkeep ;

   } else {       /*------ didn't do random search -----*/

     ntry = 1 ;   /* can only try the input x[] vector! */

   }

   /****** fully optimize each of the first ntry-th vectors in x01[] ******/

   if( verb )
     INFO_message("Powell: 2nd round optimization on %d vectors",ntry) ;

   tbest = 0 ; vbest = BIGVAL ;
   for( tt=0 ; tt < ntry ; tt++ ){
     (void)newuoa_( &n , &npt , (doublereal *)x01[tt] ,
                    &rhobeg , &rhoend , &maxfun , w , &icode ) ;
     for( ii=0 ; ii < ndim ; ii++ ) x01[tt][ii] = PRED01(x01[tt][ii]) ;
     if( scalx != SC_BOX ) xreduce(ndim,x01[tt]) ;
     (void)calfun_(&n,x01[tt],x01val+tt) ; ntot += icode+1 ;
     if( x01val[tt] < vbest ){ vbest = x01val[tt]; tbest = tt; }
     if( verb > 1 )
       ININFO_message("%2d: cost = %g %c  nfunc=%d",tt,x01val[tt],(tbest==tt)?'*':' ',icode) ;
   }

   /*-- Rescale bestest output vector back to 'true' range --*/

   if( scalx != SC_BOX ) xreduce(ndim,x01[tbest]) ;
   for( ii=0 ; ii < ndim ; ii++ )
     x[ii] = sxmin[ii] + x01[tbest][ii] * sxsiz[ii] ;
   if( cost != NULL ) *cost = vbest ;    /* save cost func */

   /*-- toss the trash, and vamoose the ranch --*/

   free((void *)sx); free((void *)sxsiz); free((void *)sxmin);
   sx = sxmin = sxsiz = NULL ; scalx = 0 ;
   for( tt=0 ; tt < nx01 ; tt++ ) free((void *)x01[tt]) ;
   free((void *)x01val); free((void *)x01); free((void *)w) ;

   return ntot ;
}

/*---------------------------------------------------------------------------*/

#undef  NCUT
#define NCUT 37
#undef  MLEV
#define MLEV 4

double minimize_in_1D( double xin, double xbot, double xtop,
                       double (*ufunc)(int,double *)        )
{
  double x1, x2, dx ;
  double xv, val, vmin ; int imin, nlev, ii;

  if( ufunc == NULL || xbot >= xtop ) return -666.0f ;

  x1 = xbot ; x2 = xtop ;

  for( nlev=0 ; nlev < MLEV ; nlev++ ){
    dx = (x2-x1)/NCUT ; vmin = 1.0e+38 ;
    for( ii=0 ; ii <= NCUT ; ii++ ){
      xv  = x1 + ii*dx ;
      val = ufunc(1,&xv) ;
      if( ii == 0 || val < vmin ){ imin = ii ; vmin = val ; }
    }
#if 0
INFO_message("x1=%g x2=%g imin=%d xmin=%g vmin=%g",x1,x2,imin,x1+imin*dx,vmin) ;
#endif
    if( nlev == MLEV-1 ) return (x1+imin*dx) ;

    if( imin == 0 ){
      x2 = x1 + 1.5*dx ;
    } else if( imin == 20 ){
      x1 = x2 - 1.5*dx ;
    } else {
      x1 = x1 + imin*dx ; x2 = x1 + 2.0*dx ;
    }
  }

  return (0.5*(x1+x2)) ; /* should never be reached */
}

#if 0
/*---------------------------------------------------------------------------*/

static int POW_gcd( int m , int n )    /* Euclid's Greatest Common Denominator */
{
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n ;
  }
  return n ;
}

/*---------------------------------------------------------------------------*/

static int POW_find_relprime_fixed( int n )  /* find number relatively prime to n */
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   for( dj=3 ; POW_gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}

/*---------------------------------------------------------------------------*/
/*! Driver for Powell's general purpose minimization newuoa function:
    - ndim   = number of variables in function to be minimized
    - nper   = number to be minimized per round of nesting
    - pstep  = step in parameter index to use
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
------------------------------------------------------------------------------*/

int powell_newuoa_nested( int ndim , int nper , int pstep , double *x ,
                          double rstart , double rend ,
                          int maxcall , double (*ufunc)(int,double *) )
{
   integer n , npt , icode , maxfun ;
   doublereal rhobeg , rhoend , rfac , *w , *xloc ;
   int ii , qnum , qloop ;

   /* check inputs */

   if( ndim < 1                         ) return -2 ;
   if( x == NULL                        ) return -3 ;
   if( rstart < rend || rstart <= 1.e-4 ) return -4 ;
   if( ufunc == NULL                    ) return -5 ;

   if( nper < 2 ){
     if( ndim <= 100 ){
       nper = ndim ;
     } else {
       nper = ndim / 5 ; if( nper > 100 ) nper = 100 ;
     }
   }
   if( nper >= ndim ){
     icode = powell_newuoa( ndim,x,rstart,rend,maxcall,ufunc ) ;
     return icode ;
   }
   if( pstep <= 0 || pstep >= ndim ){
     pstep = POW_find_relprime_fixed(ndim) ;
   } else if( pstep > 1 ){
     for( ; POW_gcd(pstep,ndim) > 1 ; pstep++ ) ; /*nada*/
   }

   if( rend    <= 0.0        ) rend    = 1.e-2 * rstart ;
   if( maxcall <  99+27*ndim ) maxcall = 99+27*ndim ;

   mapx  = ndim ;
   mapar = (double *)malloc(sizeof(double)*ndim) ;
   mapin = (int *)   malloc(sizeof(int)   *nper) ;
   xloc  = (double *)malloc(sizeof(double)*nper) ;
   for( ii=0 ; ii < ndim ; ii++ ) mapar[ii] = x[ii] ;

   n      = nper ;
   npt    = (int)(mfac*n+afac) ; if( npt < n+2   ) npt = n+2 ;
   icode  = (n+1)*(n+2)/2      ; if( npt > icode ) npt = icode ;
   maxfun = 13*nper+1 ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode   = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 6666 ;
   w       = (doublereal *)malloc(sizeof(doublereal)*icode) ; /* workspace */
   icode   = 0 ;
   userfun = ufunc ;
   scalx   = 0 ;

   qnum = 0 ; mapin[nper-1] = -pstep ;
   rfac = pow( 0.8 , nper/(double)ndim ) ;

   do{

     mapin[0] = (mapin[nper-1] + pstep) % ndim ;
     for( ii=1 ; ii < nper ; ii++ ) mapin[ii] = (mapin[ii-1]+pstep) % ndim ;
     for( ii=0 ; ii < nper ; ii++ ) xloc[ii] = mapar[mapin[ii]] ;

     /* do the local work: best params are put back into xloc[] */

     (void)newuoa_( &n , &npt , (doublereal *)xloc ,
                    &rhobeg , &rhoend , &maxfun , w , &icode ) ;

     for( ii=0 ; ii < nper ; ii++ ) mapar[mapin[ii]] = xloc[ii] ;
     qnum += icode ; rhobeg *= rfac ;

   } while( qnum < maxcall ) ;

   for( ii=0 ; ii < ndim ; ii++ ) x[ii] = mapar[ii] ;

   free(w) ; free(xloc) ; free(mapin) ; free(mapar) ;

   return qnum ;  /* number of function calls */
}
#endif
