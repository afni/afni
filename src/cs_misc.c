#include "mrilib.h"

/*-----------------------------------------------------------------------------*/

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
   if( qv > 9 ){
     sprintf( sval+strlen(sval) , "%d" , qv ) ;
   } else {
     qv = (int) rint(aval/(0.1*tval)) ;
     sprintf( sval+strlen(sval) , "%.1f" , 0.1*qv ) ;
   }

   switch( lv ){

     case 0: break ;

     case 1: strcat(sval+strlen(sval)," thousand")    ; break ;
     case 2: strcat(sval+strlen(sval)," million" )    ; break ;
     case 3: strcat(sval+strlen(sval)," billion" )    ; break ;
     case 4: strcat(sval+strlen(sval)," trillion")    ; break ;
     case 5: strcat(sval+strlen(sval)," quadrillion") ; break ;
     case 6: strcat(sval+strlen(sval)," quintillion") ; break ;
     case 7: strcat(sval+strlen(sval)," sextillion")  ; break ;
     case 8: strcat(sval+strlen(sval)," septillion")  ; break ;
     case 9: strcat(sval+strlen(sval)," octillion")   ; break ;

     case -1: strcat(sval+strlen(sval)," thousand-ths")   ; break ;
     case -2: strcat(sval+strlen(sval)," million-ths")    ; break ;
     case -3: strcat(sval+strlen(sval)," billion-ths")    ; break ;
     case -4: strcat(sval+strlen(sval)," trillion-ths")   ; break ;
     case -5: strcat(sval+strlen(sval)," quadrillion-ths"); break ;
     case -6: strcat(sval+strlen(sval)," quintillion-ths"); break ;
     case -7: strcat(sval+strlen(sval)," sextillion-ths") ; break ;
     case -8: strcat(sval+strlen(sval)," septillion-ths") ; break ;
     case -9: strcat(sval+strlen(sval)," octillion-ths")  ; break ;

     default:
       strcat(sval+strlen(sval)," gazillion") ;
       if( lv < 0 ) strcat(sval+strlen(sval),"-ths") ;
     break ;
   }

   return (char *)sval ;
}

/*-----------------------------------------------------------------------------*/

int strcmp_aboot( char *a , char *b )  /* 12 Mar 2007 */
{
   char *aa , *bb , *qq ; int ii ;
   if( a == b    ) return  0 ;
   if( a == NULL ) return -1 ;
   if( b == NULL ) return  1 ;
   aa = strchr(a,' '); if( aa == NULL ) aa = strchr(a,'_');   /* if no blanks */
   bb = strchr(b,' '); if( bb == NULL ) bb = strchr(b,'_'); /* or underscores */
   if( aa == NULL && bb == NULL ) return strcmp(a,b) ;    /* do normal strcmp */
   aa = strdup(a) ;
   for( qq=aa ; *qq != '\0' ; qq++ )
     if( *qq == ' ' || *qq == '_' ) *qq = '-' ;  /* get rid of blanks */
   bb = strdup(b) ;                              /* and underscores  */
   for( qq=bb ; *qq != '\0' ; qq++ )
     if( *qq == ' ' || *qq == '_' ) *qq = '-' ;
   ii = strcmp(aa,bb) ;                          /* and compare THESE strings */
   free(bb); free(aa); return ii;
}
