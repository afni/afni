#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#undef  NJ
#define NJ  9

#undef  NPT
#define NPT ((1<<NJ)*10)

void ibeta( double xcut , double a , double b ,
            double *f1 , double *flogx , double *flog1x )
{
   int nn , ii , jj , istep ;
   double dx , s1,sx,s1x , x, a1=a-1.0,b1=b-1.0 ;
   double as1[NJ+1] , asx[NJ+1] , as1x[NJ+1] ;
   double bs1[NJ-1] , bsx[NJ-1] , bs1x[NJ-1] ;
   double cs1[NJ-3] , csx[NJ-3] , cs1x[NJ-3] ;
   double xab[NPT] , xabx[NPT] , xab1x[NPT] ;

   if( xcut <= 0.0 || xcut >= 1.0 || a <= 0.0 || b <= 0.0 ) return ;

   if( f1 == NULL && flogx == NULL && flog1x == NULL ) return ;

   dx = xcut / NPT ;
   for( ii=1 ; ii <= NPT ; ii++ ){
      x = ii*dx ;
/* fprintf(stderr,"ii=%d dx=%g a1=%g b1=%g x=%g\n",ii,dx,a1,b1,x) ; */
      xab[ii]   = pow(x,a1) * pow(1.0-x,b1) ;
      xabx[ii]  = xab[ii] * log(x) ;
      xab1x[ii] = xab[ii] * log(1.0-x) ;

/* fprintf(stderr,"ii=%d x=%g xab=%g xabx=%g xab1x=%g a1=%g b1=%g dx=%g\n",
        ii,x,xab[ii],xabx[ii],xab1x[ii],a1,b1,dx) ; */
   }

   for( nn=NPT,istep=1,jj=NJ ; jj >= 0 ; jj--,istep*=2,nn/=2 ){
      s1 = sx = s1x = 0.0 ;
      for( ii=istep ; ii <= NPT ; ii+=istep ){
        s1 += xab[ii] ; sx += xabx[ii] ; s1x += xab1x[ii] ;
      }
      dx = xcut / nn ;
      as1[jj] = s1*dx ; asx[jj] = sx*dx ; as1x[jj] = s1x*dx ;
   }

#undef  AITKEN
#define AITKEN(x,y,z) ((x) - ((y)-(x))*((y)-(x)) / (((z)-(y))-((y)-(x))) )

   for( jj=0 ; jj <= NJ-2 ; jj++ ){
      bs1[jj]  = AITKEN( as1[jj]  , as1[jj+1]  , as1[jj+2]  ) ;
      bsx[jj]  = AITKEN( asx[jj]  , asx[jj+1]  , asx[jj+2]  ) ;
      bs1x[jj] = AITKEN( as1x[jj] , as1x[jj+1] , as1x[jj+2] ) ;
   }

   for( jj=0 ; jj <= NJ-4 ; jj++ ){
      cs1[jj]  = AITKEN( bs1[jj]  , bs1[jj+1]  , bs1[jj+2]  ) ;
      csx[jj]  = AITKEN( bsx[jj]  , bsx[jj+1]  , bsx[jj+2]  ) ;
      cs1x[jj] = AITKEN( bs1x[jj] , bs1x[jj+1] , bs1x[jj+2] ) ;
   }

#define DMP(tbl,jt)                                                    \
 do{ fprintf(stderr," table " #tbl ":") ;                              \
     for( jj=0 ; jj <= jt ; jj++ ) fprintf(stderr," %12.5g",tbl[jj]) ; \
     fprintf(stderr,"\n") ; } while(0)

   fprintf(stderr,"ibeta(xc=%g,a=%g,b=%g)\n",xcut,a,b) ;
   DMP(as1,NJ) ; DMP(bs1,NJ-2) ; DMP(cs1,NJ-4) ;
   DMP(asx,NJ) ; DMP(bsx,NJ-2) ; DMP(csx,NJ-4) ;
   DMP(as1x,NJ) ; DMP(bs1x,NJ-2) ; DMP(cs1x,NJ-4) ;

   if( f1     != NULL ) *f1     = cs1[NJ-4]  ;
   if( flogx  != NULL ) *flogx  = csx[NJ-4]  ;
   if( flog1x != NULL ) *flog1x = cs1x[NJ-4] ;

   return ;
}

int main( int argc , char * argv[] )
{
   double xcut , a , b , f1,f2,f3 ;

   if( argc < 4 ) exit(0) ;
   xcut = strtod(argv[1],NULL) ;
   a    = strtod(argv[2],NULL) ;
   b    = strtod(argv[3],NULL) ;

   ibeta( xcut,a,b , &f1,&f2,&f3 ) ; exit(0) ;
}
