
#undef  NJ
#define NJ  5

#undef  NPT
#define NPT ((1<<NJ)*50)

void ibeta( double xcut , double a , double b ,
            double *f1 , double *flogx , double *flog1x )
{
   int nn , ii , jj , istep ;
   double dx , s1,sx,s1x , x,xab , a1=a-1.0,b1=b-1.0 ;
   double as1[NJ] , asx[NJ] , as1x[NJ] ;
   double xab[NPT] , xabx[NPT] , xab1x[NPT] ;

   if( xcut <= 0.0 || a <= 0.0 || b <= 0.0 ) return ;

   if( f1 == NULL && flogx == NULL && flog1x == NULL ) return ;

   dx = xcut / NPT ;
   for( ii=1 ; ii <= NPT ; ii++ ){
      x = ii*dx ;
      xab[ii]   = pow(x,a1) * pow(1.0-x,b1) ;
      xabx[ii]  = xab[ii] * log(x) ;
      xab1x[ii] = xab[ii] * log(1.0-x) ;
   }

   for( nn=NPT,istep=1,jj=4 ; jj >=0 ; jj--,istep*=2,nn/=2 ){
      s1 = sx = s1x = 0.0 ;
      for( ii=istep ; ii <= NPT ; ii+=istep ){
        s1 += xab[ii] ; sx += xabx[ii] ; s1x += xab1x[ii] ;
      }
      dx = xcut / nn ;
      as1[jj] = s1*dx ; asx[jj] = sx*dx ; as1x[jj] = s1x*dx ;
   }
