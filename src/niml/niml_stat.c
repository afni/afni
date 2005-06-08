#include "niml_private.h"
#include <math.h>

/****************************************************************************/
/********* Statistics stuff for NIML ****************************************/
/****************************************************************************/

static int numparam[] = { 0,0,3,1,2,0,1,2,2,2,
                          1,2,3,2,2,2,2,2,3,1,
                          2,2,0,0,0,
                        -1 } ;

static char *distname[] = {
   "none"     , "none"    , "Correl"     , "Ttest"      , "Ftest"    ,
   "Zscore"   , "Chisq"   , "Beta"       , "Binom"      , "Gamma"    ,
   "Poisson"  , "Normal"  , "Ftest_nonc" , "Chisq_nonc" , "Logistic" ,
   "Laplace"  , "Uniform" , "Ttest_nonc" , "Weibull"    , "Chi"      ,
   "Invgauss" , "Extval"  , "Pval"       , "LogPval"    , "Log10Pval",
 NULL } ;

/*---------------------------------------------------------------------------*/

int NI_stat_numparam( int scode )
{
   return numparam[(scode >=0 && scode <= NI_STAT_LASTCODE) ? scode : 0] ;
}

/*---------------------------------------------------------------------------*/

char * NI_stat_distname( int scode )
{
   return distname[(scode >=0 && scode <= NI_STAT_LASTCODE) ? scode : 0] ;
}

/*---------------------------------------------------------------------------*/

static void NI_fval_to_char( float qval , char *buf )
{
   float aval = fabs(qval) ;
   int lv , il ;
   char lbuf[32] ;

   /* special case if the value is an integer */

   if( qval == 0.0 ){ strcpy(buf,"0"); return; }

   lv = (fabs(qval) < 99999999.0) ? (int)qval : 100000001 ;

   if( qval == lv && abs(lv) < 100000000 ){ sprintf(buf,"%d",lv); return; }

/* macro to strip trailing zeros from output */

#undef  BSTRIP
#define BSTRIP for( il=strlen(lbuf)-1 ;                        \
                    il>1 && (lbuf[il]=='0' || lbuf[il]==' ') ; \
                    il-- ) lbuf[il] = '\0'

   /* noninteger: choose floating format based on magnitude */

   lv = (int) (10.0001 + log10(aval)) ;

   switch( lv ){

      default:
         if( qval > 0.0 ) sprintf( lbuf , "%-12.6e" , qval ) ;
         else             sprintf( lbuf , "%-12.5e" , qval ) ;
      break ;

      case  6:  /* 0.0001-0.001 */
      case  7:  /* 0.001 -0.01  */
      case  8:  /* 0.01  -0.1   */
      case  9:  /* 0.1   -1     */
      case 10:  /* 1     -9.99  */
         sprintf( lbuf , "%-9.6f" , qval ) ; BSTRIP ; break ;

      case 11:  /* 10-99.9 */
         sprintf( lbuf , "%-9.5f" , qval ) ; BSTRIP ; break ;

      case 12:  /* 100-999.9 */
         sprintf( lbuf , "%-9.4f" , qval ) ; BSTRIP ; break ;

      case 13:  /* 1000-9999.9 */
         sprintf( lbuf , "%-9.3f" , qval ) ; BSTRIP ; break ;

      case 14:  /* 10000-99999.9 */
         sprintf( lbuf , "%-9.2f" , qval ) ; BSTRIP ; break ;

      case 15:  /* 100000-999999.9 */
         sprintf( lbuf , "%-9.1f" , qval ) ; BSTRIP ; break ;

      case 16:  /* 1000000-9999999.9 */
         sprintf( lbuf , "%-9.0f" , qval ) ; break ;
   }

   strcpy(buf,lbuf) ; return ;
}

/*---------------------------------------------------------------------------*/
/* free() the result when done, please. */

char * NI_stat_encode( int scode , float p1, float p2, float p3 )
{
   char *buf , *nam , b1[16] , b2[16] , b3[16] ;
   int np ;

   if( scode < NI_STAT_FIRSTCODE || scode > NI_STAT_LASTCODE ){
     buf = strdup("none") ; return buf ;
   }

   np  = NI_stat_numparam( scode ) ;  /* # of parameters */
   nam = distname[scode] ;            /* distribution name */
   buf = malloc( 20*(np+1) ) ;        /* output buffer */
   switch( np ){
     case 0:
       sprintf(buf,"%s()",nam) ; break ;

     case 1:
       NI_fval_to_char( p1 , b1 ) ;
       sprintf(buf,"%s(%s)",nam,b1) ; break ;

     case 2:
       NI_fval_to_char( p1 , b1 ) ;
       NI_fval_to_char( p2 , b2 ) ;
       sprintf(buf,"%s(%s,%s)",nam,b1,b2) ; break ;

     default:
     case 3:
       NI_fval_to_char( p1 , b1 ) ;
       NI_fval_to_char( p2 , b2 ) ;
       NI_fval_to_char( p3 , b3 ) ;
       sprintf(buf,"%s(%s,%s,%s)",nam,b1,b2,b3) ; break ;
   }

   return buf ;
}

/*---------------------------------------------------------------------------*/
/* Input is in format "distname(param1,param2,param3)"
-----------------------------------------------------------------------------*/

void NI_stat_decode( char *str, int *scode, float *p1, float *p2 , float *p3 )
{
   int jj , ll ;
   char *dnam , qnam[64] ;

   if( scode == NULL ) return ;                 /* bad input */
   *scode = 0 ;
   if( str == NULL || *str == '\0' ) return ;   /* bad input */

   /* scan for distribution name */

   for( jj=NI_STAT_FIRSTCODE ; jj <= NI_STAT_LASTCODE ; jj++ ){
     dnam = NI_stat_distname(jj) ;
     strcpy(qnam,dnam); strcat(qnam,"("); ll = strlen(qnam);
     if( strncasecmp(str,qnam,ll) == 0 ) break ;
   }
   if( jj <= NI_STAT_LASTCODE ){
     float parm[3]={1.0f,1.0f,1.0f} ; int np,kk,mm , sp ;
     np = NI_stat_numparam(jj) ; sp = ll ;
     for( kk=0 ; kk < np ; kk++ ){
       mm = 0 ; sscanf(str+sp,"%f%n",parm+kk,&mm) ; sp += mm+1 ;
     }
     *scode = jj ;                     /* Save results */
     if( p1 != NULL ) *p1 = parm[0] ;  /* into output */
     if( p2 != NULL ) *p2 = parm[1] ;  /* variables. */
     if( p3 != NULL ) *p3 = parm[2] ;
   }
   return ;
}
