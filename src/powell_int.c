#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <cs.h>
#include "f2c.h"

extern int newuoa_(integer *n, integer *npt, doublereal *x,
                   doublereal *rhobeg, doublereal *rhoend, integer *maxfun,
                   doublereal *w, integer *icode) ;

/*---------------------------------------------------------------------------*/

static double (*userfun)( int n , double *x ) = NULL ;

int calfun_(integer *n, doublereal *x, doublereal *fun)
{
   double val ;
   val = userfun( (int)(*n) , (double *)x ) ;
   *fun = (doublereal)val ;
   return ;
}

/*---------------------------------------------------------------------------*/

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
   npt    = (int)(2.5*n+1); icode = (n+1)*(n+2)/2; if( npt > icode ) npt=icode;
   maxfun = maxcall ;

   rhobeg = (doublereal)rstart ;
   rhoend = (doublereal)rend   ;

   icode = (npt+14)*(npt+n) + 3*n*(n+3)/2 + 666 ;
   w = (doublereal *)malloc(sizeof(doublereal)*icode) ;
   icode = 0 ;
   userfun = ufunc ;

   (void)newuoa_( &n , &npt , (doublereal *)x ,
                  &rhobeg , &rhoend , &maxfun , w , &icode ) ;

   free((void *)w) ;
   return icode ;
}
