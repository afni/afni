#include "mrilib.h"

char * approximate_number_string( double val )
{
   static char sval[128] ;
   double aval=fabs(val) , tval ;
   int    lv , qv ;

   if( aval == 0.0 ){ strcpy(sval,"Zero"); return; }

   if( val < 0.0 ){ strcpy(sval,"-"); } else { sval[0] = '\0'; }

   lv   = (int) floor(log10(aval)/3.0) ;
   tval = pow(10.0,(double)(3*lv)) ;
   qv   = (int) rint(aval/tval) ;
   sprintf( sval+strlen(sval) , "%d" , qv ) ;

   switch( lv ){

     case 0: break ;

     case 1: strcat(sval+strlen(sval)," thousand")    ; break ;
     case 2: strcat(sval+strlen(sval)," million" )    ; break ;
     case 3: strcat(sval+strlen(sval)," billion" )    ; break ;
     case 4: strcat(sval+strlen(sval)," trillion")    ; break ;
     case 5: strcat(sval+strlen(sval)," quadrillion") ; break ;
     case 6: strcat(sval+strlen(sval)," quintillion") ; break ;

     case -1: strcat(sval+strlen(sval)," thousand-ths") ; break ;
     case -2: strcat(sval+strlen(sval)," million-ths")  ; break ;
     case -3: strcat(sval+strlen(sval)," billion-ths")  ; break ;
     case -4: strcat(sval+strlen(sval)," trillion-ths") ; break ;

     default:
       strcat(sval+strlen(sval)," jillion") ;
       if( lv < 0 ) strcat(sval+strlen(sval),"-ths") ;
     break ;
   }

   return (char *)sval ;
}
