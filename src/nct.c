
/****************************************************************************
  Noncentral t distribution function by
    Professor K. Krishnamoorthy
    Department of Mathematics
    University of Louisiana at Lafayette
*****************************************************************************/


#if 0
static double alng( double x )
{
   int indx ;
   double xx,fterm,sum,valg ;
   double b[9] = { 0.0 ,
                   8.33333333333333e-2, 3.33333333333333e-2,
                   2.52380952380952e-1, 5.25606469002695e-1,
                   1.01152306812684e0,  1.51747364915329e0,
                   2.26948897420496e0,  3.00991738325940e0   } ;

   if( x < 8.0 ){ xx = x + 8.0 ; indx = 1 ; }
   else         { xx = x       ; indx = 0 ; }

   fterm = (xx-0.5)*log(xx) - xx + 9.1893853320467e-1 ;
   sum = b[1]/(xx+b[2]/(xx+b[3]/(xx+b[4]/(xx+b[5]/(xx+b[6]/
                                         (xx+b[7]/(xx+b[8]))))))) ;
   valg = sum + fterm ;
   if(indx)
     valg = valg-log(x+7.0)-log(x+6.0)-log(x+5.0)
                -log(x+4.0)-log(x+3.0)-log(x+2.0)-log(x+1.0)-log(x) ;
   return valg ;
}
#else
# define alng(x) lgamma(x)   /* C math library log(Gamma(x)) function */
#endif

/*---------------------------------------------------------------------------*/

#if 1
static double gaudf( double x )
{
   static double p0=913.16744211475570 , p1=1024.60809538333800,
                 p2=580.109897562908800, p3=202.102090717023000,
                 p4=46.0649519338751400, p5=6.81311678753268400,
                 p6=6.047379926867041e-1,p7=2.493381293151434e-2 ;
   static double q0=1826.33488422951125, q1=3506.420597749092,
                 q2=3044.77121163622200, q3=1566.104625828454,
                 q4=523.596091947383490, q5=116.9795245776655,
                 q6=17.1406995062577800, q7=1.515843318555982,
                 q8=6.25e-2 ;
   static double sqr2pi=2.506628274631001 ;
   int check ;
   double reslt,z , first,phi ;

   if(x > 0.0){ z = x ; check = 1 ; }
   else       { z =-x ; check = 0 ; }

   if( z > 32.0 ) return (x > 0.0) ? 1.0 : 0.0 ;

   first = exp(-0.5*z*z) ;
   phi   = first/sqr2pi ;

   if (z < 7.0)
      reslt = first* (((((((p7*z+p6)*z+p5)*z+p4)*z+p3)*z+p2)*z+p1)*z+p0)
                   /((((((((q8*z+q7)*z+q6)*z+q5)*z+q4)*z+q3)*z+q2)*z+q1)*z+q0);
   else
      reslt = phi/(z+1.0/(z+2.0/(z+3.0/(z+4.0/(z+6.0/(z+7.0)))))) ;

   if(check) reslt = 1.0 - reslt ;
   return reslt ;
}
#else
static double gaudf( double x )
{
   pqpair pq = normal_s2pq(x) ; return pq.p ;
}
#endif

/*---------------------------------------------------------------------------*/

#if 1
static double betadf( double x , double p , double q )
{
   int check , ns ;
   double result,betf,psq,xx,cx,pp,qq ;
   double term,ai,rx,temp ;

   if( x >= 1.0 ) return 1.0 ;
   if( x <= 0.0 ) return 0.0 ;

   betf = alng(p)+alng(q)-alng(p+q) ;
   result=x ;
   psq=p+q ;
   cx=1.0-x ;
   if(p < psq*x){ xx=cx ; cx=x ; pp=q ; qq=p ; check=1 ; }
   else         { xx=x  ;        pp=p ; qq=q ; check=0 ; }

   term=1.0 ;
   ai=1.0 ;
   result=1.0 ;
   ns=(int)(qq+cx*psq) ;
   rx=xx/cx ;
L3:
   temp=qq-ai ;
   if(ns == 0) rx=xx ;
L4:
   term=term*temp*rx/(pp+ai) ;
   result=result+term ;
   temp=fabs(term) ;
   if(temp <= 1.e-14 && temp <= 1.e-14*result) goto L5 ;
   ai=ai+1.0 ;
   ns=ns-1 ;
   if(ns >= 0) goto L3 ;
   temp=psq ;
   psq=psq+1.0 ;
   goto L4 ;

L5:
   result=result*exp(pp*log(xx)+(qq-1.0)*log(cx)-betf)/pp ;
   if(check) result=1.0-result ;
   return result ;
}
#else
static double betadf( double x , double p , double q )
{
   pqpair pq = beta_s2pq(x,p,q) ; return pq.p ;
}
#endif

/*---------------------------------------------------------------------------*/

double tnonc_s2p( double t , double df , double delta )
{
   int indx , k , i ;
   double x,del,tnd,ans,y,dels,a,b,c ;
   double pkf,pkb,qkf,qkb , pgamf,pgamb,qgamf,qgamb ;
   double pbetaf,pbetab,qbetaf,qbetab ;
   double ptermf,qtermf,ptermb,qtermb,term ;
   double rempois,delosq2,sum,cons,error ;

   if( t < 0.0 ){ x = -t ; del = -delta ; indx = 1 ; }
   else         { x =  t ; del =  delta ; indx = 0 ; }

   ans = gaudf(-del) ;
   if( x == 0.0 ) return ans ;

   y = x*x/(df+x*x) ;
   dels = 0.5*del*del ;
   k = (int)dels ;
   a = k+0.5 ;
   c = k+1.0 ;
   b = 0.5*df ;

   pkf = exp(-dels+k*log(dels)-alng(k+1.0)) ;
   pkb = pkf ;
   qkf = exp(-dels+k*log(dels)-alng(k+1.0+0.5)) ;
   qkb = qkf ;

   pbetaf = betadf(y, a, b) ;
   pbetab = pbetaf ;
   qbetaf = betadf(y, c, b) ;
   qbetab = qbetaf ;

   pgamf = exp(alng(a+b-1.0)-alng(a)-alng(b)+(a-1.0)*log(y)+b*log(1.0-y)) ;
   pgamb = pgamf*y*(a+b-1.0)/a ;
   qgamf = exp(alng(c+b-1.0)-alng(c)-alng(b)+(c-1.0)*log(y) + b*log(1.0-y)) ;
   qgamb = qgamf*y*(c+b-1.0)/c ;

   rempois = 1.0 - pkf ;
   delosq2 = del/1.4142135623731 ;
   sum = pkf*pbetaf+delosq2*qkf*qbetaf ;
   cons = 0.5*(1.0 + 0.5*fabs(delta)) ;
   i = 0 ;
L1:
   i = i + 1 ;
   pgamf = pgamf*y*(a+b+i-2.0)/(a+i-1.0) ;
   pbetaf = pbetaf - pgamf ;
   pkf = pkf*dels/(k+i) ;
   ptermf = pkf*pbetaf ;
   qgamf = qgamf*y*(c+b+i-2.0)/(c+i-1.0) ;
   qbetaf = qbetaf - qgamf ;
   qkf = qkf*dels/(k+i-1.0+1.5) ;
   qtermf = qkf*qbetaf ;
   term = ptermf + delosq2*qtermf  ;
   sum = sum + term ;
   error = rempois*cons*pbetaf ;
   rempois = rempois - pkf ;

   if( i > k ){
     if( error <= 1.e-12 || i >= 1000 ) goto L2 ;
     goto L1 ;
   } else {
     pgamb = pgamb*(a-i+1.0)/(y*(a+b-i)) ;
     pbetab = pbetab + pgamb ;
     pkb = (k-i+1.0)*pkb/dels ;
     ptermb = pkb*pbetab  ;
     qgamb = qgamb*(c-i+1.0)/(y*(c+b-i)) ;
     qbetab = qbetab + qgamb ;
     qkb = (k-i+1.0+0.5)*qkb/dels ;
     qtermb = qkb*qbetab  ;
     term =  ptermb + delosq2*qtermb ;
     sum = sum + term  ;
     rempois = rempois - pkb ;
     if (rempois <= 1.e-12 || i >= 1000) goto L2 ;
     goto L1 ;
   }
L2:
   tnd = 0.5*sum + ans ;
   if(indx) tnd = 1.0 - tnd ;
   return tnd ;
}
