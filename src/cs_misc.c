#include "mrilib.h"

/*-----------------------------------------------------------------------------*/
/* mm is in msec; output pointer is statically allocated -- do not free!
   Sample usage:
      int ct = NI_clock_time() ;
       ... stuff happens ...
      printf("stuff clock time =%s",nice_time_string(NI_clock_time()-ct)) ;
*//*---------------------------------------------------------------------------*/

char * nice_time_string( int mm )
{
   static char str[256] ; int ms,nd,nh,nm,ns ;

   if( mm == 0 ){ strcpy(str,"0 s") ; return str ; }
   ms = (mm < 0 ) ? -mm : mm ;

   nd = ms / 86400000 ; ms = ms % 86400000 ;
   nh = ms /  3600000 ; ms = ms %  3600000 ;
   nm = ms /    60000 ; ms = ms %    60000 ;
   ns = ms /     1000 ; ms = ms %     1000 ;

   if( mm < 0 ) strcpy(str," -(") ; else str[0] = '\0' ;
   if( nd > 0 ) sprintf( str+strlen(str) , " %dd",nd) ;
   if( nd > 0 || nh > 0 ) sprintf( str+strlen(str) , " %dh",nh) ;
   if( nd > 0 || nh > 0 || nm > 0 ) sprintf( str+strlen(str) , " %dm",nm) ;
   if( nd > 0 || nh > 0 || nm > 0 || ns > 0 ) sprintf( str+strlen(str) , " %ds",ns) ;
   if( nd > 0 || nh > 0 || nm > 0 || ns > 0 || ms > 0 ) sprintf( str+strlen(str) , " %dms",ms) ;
   if( mm < 0 ) strcat(str," )") ;
   return str ;
}

/*-----------------------------------------------------------------------------*/

int is_a_number( char *str )  /* 03 Apr 2012 */
{
   double val ; char *cpt ;

   if( str == NULL || *str == '\0' ) return 0 ;

   val = strtod(str,&cpt) ;
   return ( *cpt == '\0' ) ;
}

/*-----------------------------------------------------------------------------*/

int is_an_int( char *str )
{
   double val ; char *cpt ;

   if( str == NULL || *str == '\0' ) return 0 ;

   val = strtod(str,&cpt) ;
   return ( *cpt == '\0' && val == (int)val ) ;
}

/*-----------------------------------------------------------------------------*/

char * commaized_integer_string( long long val )
{
   static char svals[9][128] ;   /* ZSS Aug. 2010 */
   char *sval;
   char qval[128] ;
   int qq,ss , qpos , qlen , pval = (val >= 0) ;
   static int k=0;

   /* without these three lines, a call like this:
      sprintf(str,"Hello %s, %s\n",
               commaized_integer_string(100),
               commaized_integer_string(500));
      would fail to write out both numbers.  ZSS Aug. 2010*/
   k = k % 9;
   sval = svals[k];
   ++k;

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
   static char svals[9][128] ;
   char *sval;
   double aval=fabs(val) , tval ;
   int    lv , qv ;
   static int k=0;

   k = k % 9;     /* ZSS Aug. 2010 (same fix as commaized_integer_string)*/
   sval = svals[k];
   ++k;

   if( aval == 0.0 ){ strcpy(sval,"Zero"); return sval; }

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

     /** also see http://www.isthe.com/chongo/tech/math/number/tenpower.html **/

     case 1: strcat(sval+strlen(sval)," thousand [kilo]")     ; break ;
     case 2: strcat(sval+strlen(sval)," million [mega]" )     ; break ;
     case 3: strcat(sval+strlen(sval)," billion [giga]" )     ; break ;
     case 4: strcat(sval+strlen(sval)," trillion [tera]")     ; break ;
     case 5: strcat(sval+strlen(sval)," quadrillion [peta]")  ; break ;
     case 6: strcat(sval+strlen(sval)," quintillion [exa]")   ; break ;
     case 7: strcat(sval+strlen(sval)," sextillion [zetta]")  ; break ;
     case 8: strcat(sval+strlen(sval)," septillion [yotta]")  ; break ;
     case 9: strcat(sval+strlen(sval)," octillion")         ; break ;
     case 10: strcat(sval+strlen(sval)," nonillion")        ; break ;
     case 11: strcat(sval+strlen(sval)," decillion")        ; break ;
     case 12: strcat(sval+strlen(sval)," undecillion")      ; break ;
     case 13: strcat(sval+strlen(sval)," duodecillion")     ; break ;
     case 14: strcat(sval+strlen(sval)," tredecillion")     ; break ;
     case 15: strcat(sval+strlen(sval)," quattuordecillion"); break ;
     case 16: strcat(sval+strlen(sval)," quindecillion")    ; break ;
     case 17: strcat(sval+strlen(sval)," sexdecillion")     ; break ;
     case 18: strcat(sval+strlen(sval)," septendecillion")  ; break ;
     case 19: strcat(sval+strlen(sval)," octodecillion")    ; break ;
     case 20: strcat(sval+strlen(sval)," novemdecillion")   ; break ;
     case 21: strcat(sval+strlen(sval)," vigintillion")     ; break ;
     case 31: strcat(sval+strlen(sval)," trigintillion")    ; break ;
     case 33: strcat(sval+strlen(sval)," duotrigintillion") ; break ;
     case 101: strcat(sval+strlen(sval)," centillion")      ; break ;

     case -1: strcat(sval+strlen(sval)," thousand-ths [milli]")   ; break ;
     case -2: strcat(sval+strlen(sval)," million-ths [micro]")    ; break ;
     case -3: strcat(sval+strlen(sval)," billion-ths [nano]")     ; break ;
     case -4: strcat(sval+strlen(sval)," trillion-ths [pico]")    ; break ;
     case -5: strcat(sval+strlen(sval)," quadrillion-ths [femto]"); break ;
     case -6: strcat(sval+strlen(sval)," quintillion-ths [atto]") ; break ;
     case -7: strcat(sval+strlen(sval)," sextillion-ths [zepto]") ; break ;
     case -8: strcat(sval+strlen(sval)," septillion-ths [yocto]") ; break ;
     case -9: strcat(sval+strlen(sval)," octillion-ths")          ; break ;
     case -10: strcat(sval+strlen(sval)," nonillion-ths")         ; break ;
     case -11: strcat(sval+strlen(sval)," decillion-ths")         ; break ;
     case -12: strcat(sval+strlen(sval)," undecillion-ths")       ; break ;
     case -13: strcat(sval+strlen(sval)," duodecillion-ths")      ; break ;
     case -14: strcat(sval+strlen(sval)," tredecillion-ths")      ; break ;
     case -15: strcat(sval+strlen(sval)," quattuordecillion-ths") ; break ;
     case -16: strcat(sval+strlen(sval)," quindecillion-ths")     ; break ;
     case -17: strcat(sval+strlen(sval)," sexdecillion-ths")      ; break ;
     case -18: strcat(sval+strlen(sval)," septendecillion-ths")   ; break ;
     case -19: strcat(sval+strlen(sval)," octodecillion-ths")     ; break ;
     case -20: strcat(sval+strlen(sval)," novemdecillion-ths")    ; break ;
     case -21: strcat(sval+strlen(sval)," vigintillion-ths")      ; break ;
     case -31: strcat(sval+strlen(sval)," trigintillion-ths")     ; break ;
     case -33: strcat(sval+strlen(sval)," duotrigintillion-ths")  ; break ;
     case -101: strcat(sval+strlen(sval)," centillion-ths")       ; break ;

     default:
       strcat(sval+strlen(sval)," GAZILLION") ;
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

/*-----------------------------------------------------------------------------*/
/* function to write a value in nice formatted ways Daniel Glen (moved by ZSS) */

char *format_value_4print(double value, int oform, char *formatstr)
{
   static int ii, len, isint;
   static char sans[256]={""}, *strptr=NULL, ch='\0';

   if(abs(value-0.0)<TINY_NUMBER) value = 0.0;
   
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

/*----------------------------------------------------------------------------*/
/* Substitute repl for targ in src.
   * Return is NULL if nothing was done (e.g., no copies of targ inside src)
   * Return value is a new maLloc-ed string with the changes made
   * src and targ can't be NULL, but repl can be NULL if you just want to
     delete targ from the src string
  ** Not particularly efficient, but seems to work.  [RWCox - 01 Jul 2011]
*//*--------------------------------------------------------------------------*/

char * string_substitute( char *src , char *targ , char *repl )
{
   char *spt , *tpt , **ptarg=NULL , *snew ;
   int ntarg , ltarg , lrepl , ii ;

   if( src  == NULL || *src  == '\0' ) return NULL ;
   if( targ == NULL || *targ == '\0' ) return NULL ;
   if( repl == NULL ) repl = "\0" ;

   /* find and make a list of pointers to all targets inside src */

   spt = src ; ntarg = 0 ; ltarg = strlen(targ) ; lrepl = strlen(repl) ;
   while(1){
     tpt = strstr(spt,targ) ; if( tpt == NULL ) break ; /* none left */
     ntarg++ ;
     ptarg = (char **)realloc(ptarg,sizeof(char *)*ntarg) ;
     ptarg[ntarg-1] = tpt ; spt = tpt+ltarg ;
   }
   if( ntarg == 0 ) return NULL ;

   /* space for new string */

   snew = (char *)calloc( strlen(src)+ntarg*(lrepl-ltarg+4)+64 , sizeof(char) ) ;

   /* for each target:
        - copy the string from spt up to the target location
        - copy the replacement
        - move spt up to the end of the target
      when done, copy the string after the end of the last target */

   spt = src ;
   for( ii=0 ; ii < ntarg ; ii++ ){
     strncat( snew , spt , sizeof(char)*(ptarg[ii]-spt) ) ;
     if( lrepl > 0 ) strcat ( snew , repl ) ;
     spt = ptarg[ii] + ltarg ;
   }
   strcat( snew , spt ) ;

   free(ptarg) ; return snew ;
}
