#include "mrilib.h"

/*-----------------------------------------------------------------------------*/

char * commaized_integer_string( long long val )
{
   static char sval[128] ;
   char qval[128] ;
   int qq,ss , qpos , qlen , pval = (val >= 0) ;

   sprintf(qval,"%lld",val) ; qlen = strlen(qval) ;

   if( AFNI_yesenv("AFNI_DONT_COMMAIZE") ||
       ( pval && qlen <= 3)              ||
       (!pval && qlen <= 4)                ){ strcpy(sval,qval); return sval; }

   if( pval ) qpos = (qlen-1) % 3 + 1 ;
   else       qpos = (qlen-2) % 3 + 2 ;

   for( qq=0 ; qq < qpos ; qq++ ) sval[qq] = qval[qq] ;
   ss = qpos ;
   for( ; qq < qlen ; ){
     sval[ss++] = ','        ;
     sval[ss++] = qval[qq++] ;
     sval[ss++] = qval[qq++] ;
     sval[ss++] = qval[qq++] ;
   }
   sval[ss] = '\0' ; return sval ;
}

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

/*! function to write a value in nice formatted ways Daniel Glen (put here by ZSS) */

char *format_value_4print(double value, int oform, char *formatstr)
{
   static int ii, len, isint;
   static char sans[256]={""}, *strptr=NULL, ch='\0';

   switch (oform) {
      case CCALC_DOUBLE: /* double */
         sprintf(sans,"%f",value) ;
         break;
      case CCALC_NICE:
         sprintf(sans,"%g",value) ;
         break;
      case CCALC_INT:
         if (value < 0.0) {
            value -= 0.5;
         } else {
            value += 0.5;
         }
         sprintf(sans,"%d",(int)value) ;
         break;
      case CCALC_FINT:
         sprintf(sans,"%d",(int)floor(value)) ;
         break;
      case CCALC_CINT:
         sprintf(sans,"%d",(int)ceil(value)) ;
         break;
  	   case CCALC_CUSTOM:           /* use user customized format */
	      sans[0]='\0';
         /* add check for integer output %d */
	      strptr = strchr(formatstr, '%');
         if(strptr==NULL) {
            sprintf(sans,"%f",value) ;
         } else {
            isint = 0;
            len = strlen(strptr);
            for(ii=1;ii<len;ii++) {
               ch = *(++strptr);
  	            switch(ch) {
		            case 'd':
                  case 'i':
                  case 'c':
                  case 'o':
                  case 'u':
                  case 'x': case 'X':
		               isint = 1; /* integer (no decimal) type output */
                     ii = len + 1;
                     break;
                  case 'e': case 'E':         /* floating point output types */
		            case 'f': case 'F':
                  case 'g': case 'G':
                  case 'a': case 'A':
                     ii = len+1;
		               break;
                  case '%':
                     strptr = strchr(strptr, '%'); /* find next % symbol */
                  default:
		               break;
               }
            }
		      if(ii==len) {
		         fprintf(stderr,
                     "unknown format specifier.\n"
                     "Try %%d, %%c, %%f or %%g instead.\n");
               sans[0] = '\0';
               return(sans);
		      }
            strptr = (char *) 1;
            while(strptr) {
	  	         strptr = strstr(formatstr, "\\n");
               if(strptr) {
                  *strptr = ' ';
                  *(strptr+1) = AFNI_EOL;
		         }
            }
            /* must type cast here*/
            if (isint) sprintf(sans,formatstr,(int)value) ;
            else sprintf(sans,formatstr,value) ;
         }
         break;
      default:
         sprintf(sans,"%f",value) ;
         break;
   }
   return(sans);
}
