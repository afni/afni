#include "mrilib.h"

#undef  MAX_PARAMETERS
#define MAX_PARAMETERS 10          /* maximum number of model parameters */

typedef void void_func() ;

typedef struct {
  int  numpar;                     /* number of parameters */
  void_func * func_func ;
  void_func * grad_func ;
  float pinit[MAX_PARAMETERS] ;
} HUBER_interface ;

/*--------------------------------------------------------------------*/

static void sin_func( float * abc, int nt, float * t, float * val )
{
   float a=abc[0], b=abc[1], c=abc[2], d=abc[3], e=abc[4] ;
   int ii ;

   for( ii=0 ; ii < nt ; ii++ ) val[ii] = a * sin(b*t[ii]+c) + d + e*t[ii] ;

   return ;
}

/*--------------------------------------------------------------------*/

static void sin_grad( int k, float * abc, int nt, float * t, float * gval )
{
   float a=abc[0], b=abc[1], c=abc[2], d=abc[3], e=abc[4] ;
   int ii ;

   switch( k ){

      case 0:
         for( ii=0 ; ii < nt ; ii++ ) gval[ii] = sin(b*t[ii]+c) ;
      return ;

      case 1:
         for( ii=0 ; ii < nt ; ii++ ) gval[ii] = a*t[ii]*cos(b*t[ii]+c) ;
      return ;

      case 2:
         for( ii=0 ; ii < nt ; ii++ ) gval[ii] = a*cos(b*t[ii]+c) ;
      return ;

      case 3:
         for( ii=0 ; ii < nt ; ii++ ) gval[ii] = 1 ;
      return ;

      case 4:
         for( ii=0 ; ii < nt ; ii++ ) gval[ii] = t[ii] ;
      return ;
   }
}

/*--------------------------------------------------------------------*/

static float huber_aa( float c )
{
   float a ;
   a =   0.5*erf(c/1.4142136)*(1.0-c*c)
       + 0.5*c*c - c*exp(-0.5*c*c)/sqrt(2.0*PI) ;
   return a ;
}

/*--------------------------------------------------------------------*/

static HUBER_interface hhint = { 5, sin_func, sin_grad,
                                 {100.0,0.314,0.333,100.0,0.01} } ;

static float range[5] = {100.0 , 0.310 , 1.555 , 300.0 , 0.02} ;

#define XX(x)  (((x)<cc && (x)>-cc) ? 0.5*(xx)*(xx) : 0.5*(cc)*(cc))
#define PSI(x) ( ((x)>=cc) ? cc : ((x)<=-cc) ? -cc : (x) )

#define IGN 2

void huber_func( int num, double to,double dt, float * vec, char ** str )
{
   static float * t  = NULL ; static int ntold = 0 ;
   static float * v  = NULL ;
   static float * w  = NULL ;
   static float ** g = NULL ; static int ngold = 0 ;
   static float new_to,new_dt , old_to,old_dt ;
   static float cc = 1.0 , aa = -1.0 ;
   static char * strout = NULL ;

   float parm[MAX_PARAMETERS] , sig , xx , yy , *tau ;
   float pold[MAX_PARAMETERS] , rold ;
   int ii , nt , nparm , kk , nite ;

   nt    = num ;
   nparm = hhint.numpar ;
   if( nt <= 2*nparm ){ *str = NULL ; return ; }  /* do nothing */

   if( ntold < nt || ngold < nparm ){             /* reallocate memory */
      if( t != NULL ) free(t) ;
      if( v != NULL ) free(v) ;
      if( w != NULL ) free(w) ;
      t = (float *) malloc( sizeof(float)*nt ) ;
      v = (float *) malloc( sizeof(float)*nt ) ;
      w = (float *) malloc( sizeof(float)*nt ) ;

      if( g != NULL ){
         for( ii=0 ; ii < ngold ; ii++ ) free(g[ii]) ;
         free(g) ;
      }
      g = (float **) malloc( sizeof(float *)*nparm ) ;
      for( kk=0 ; kk < nparm ; kk++ )
         g[kk] = (float *) malloc( sizeof(float)*nt ) ;
      ngold = nparm ;

      ntold = nt ;
      old_to=-666.0 ; old_dt=-555.5 ;

      for( ii=0 ; ii < nt ; ii++ ) w[ii] = (ii<IGN) ? 0.0 : 1.0 ;
   }

   /* time points */

   new_to = to ; new_dt = dt ;
   if( new_to != old_to || new_dt != old_dt ){
      for( ii=0 ; ii < nt ; ii++ ) t[ii] = new_to + new_dt * ii ;
   }

   if( aa < 0.0 ) aa = huber_aa(cc) ;  /* Huber's 'a' constant */

   /* initialize parameters */

   for( kk=0 ; kk < nparm ; kk++ ) pold[kk] = hhint.pinit[kk] ;
   hhint.func_func( pold , nt , t , v ) ;
   xx = 0.0 ;
   for( ii=IGN ; ii < nt ; ii++ ) xx += fabs(vec[ii]-v[ii]) ;
   rold = xx ;

   for( nite=0 ; nite < 9999 ; nite++ ){
      for( kk=0 ; kk < nparm ; kk++ )
         parm[kk] = hhint.pinit[kk] + (drand48()-0.5)*range[kk] ;
      hhint.func_func( parm , nt , t , v ) ;
      xx = 0.0 ;
      for( ii=IGN ; ii < nt ; ii++ ) xx += fabs(vec[ii]-v[ii]) ;
      if( xx < rold ){
         for( kk=0 ; kk < nparm ; kk++ ) pold[kk] = parm[kk] ;
         rold = xx ;
      }
   }
   for( kk=0 ; kk < nparm ; kk++ ) parm[kk] = pold[kk] ;
   sig = rold / (nt-IGN) ;

   nite = 0 ;
   do{
      hhint.func_func( parm , nt , t , v ) ;         /* eval funcs */

      for( ii=0 ; ii < IGN ; ii++ ) v[ii] = 0.0 ;
      yy = 0 ;
      for( ii=IGN ; ii < nt ; ii++ ){
         v[ii] = vec[ii] - v[ii] ;                   /* residuals */
         xx = v[ii] / sig ; yy += XX(xx) ;
      }
      sig *= sqrt( yy / ((nt-hhint.numpar-IGN) * aa) ) ; /* Huber scale */

      for( ii=0 ; ii < nt ; ii++ ){                  /* Winsorize */
         xx = v[ii] / sig ; v[ii] = PSI(xx) * sig ;
      }

      for( kk=0 ; kk < nparm ; kk++ )                /* gradients */
         hhint.grad_func( kk , parm , nt , t , g[kk] ) ;

      tau = lsqfit( nt , v , w , nparm , g ) ;    /* fit residuals */
      if( tau == NULL ){
         fprintf(stderr,"*** lsqfit fails in huber!\n") ;
         *str = NULL ; return ;
      }

      for( kk=0 ; kk < nparm ; kk++ ) parm[kk] += tau[kk] ;  /* update */

      free(tau) ; nite++ ;
   } while(nite < 10) ;

   hhint.func_func( parm , nt , t , vec ) ;   /* eval funcs */

   if( strout != NULL ){ free(strout) ; strout = NULL ; }

   strout = THD_zzprintf( strout , "\n" ) ;
   for( kk=0 ; kk < nparm ; kk++ )
      strout = THD_zzprintf( strout , "param %d = %g\n" , kk,parm[kk] ) ;

   *str = strout ;
   return ;
}
