#define  NEED_PARSER_INTERNALS
#include "parser.h"

/***** C routines to interface to the f2c generated parser code *****/

/*------------------------------------------------------------------
   Input  = expression string
   Output = structure containing information about how to
            evaluate the expression; should be free-d when unneeded;
            if NULL is returned, an error occurred.
--------------------------------------------------------------------*/

PARSER_code * PARSER_generate_code( char * expression )
{
   logical false = FALSE_ ;
   integer num_code ;
   int nexp ;
   PARSER_code * pc ;

   if( expression == NULL ) return NULL ;
   nexp = strlen( expression ) ;
   if( nexp == 0 ) return NULL ;

   pc = (PARSER_code *) malloc( sizeof(PARSER_code) ) ;

   parser_( expression , &false , &num_code , pc->c_code ,
            (ftnlen) nexp , (ftnlen) 8 ) ;

   if( num_code <= 0 ){ free(pc) ; return NULL ; }

   pc->num_code = (int) num_code ;
   return pc ;
}

/*---------------------------------------------------------------
   pc   = code to evaluate expression
   atoz = double [26] containing values for variables A,B,...,Z
-----------------------------------------------------------------*/

double PARSER_evaluate_one( PARSER_code * pc , double atoz[] )
{
   integer num_code ;
   double  value ;

   if( pc == NULL || pc->num_code <= 0 ) return 0.0 ;

   num_code = (integer) pc->num_code ;

   value = (double) pareval_( &num_code, pc->c_code,
                              (doublereal *) atoz , (ftnlen) 8 ) ;
   return value ;
}

/*----------------------------------------------------------------------
   Return 1 if the given code uses the symbol given by the
   first character of sym, otherwise return 0 - 15 Sep 1999 - RWCox.
------------------------------------------------------------------------*/

#include <ctype.h>

int PARSER_has_symbol( char * sym , PARSER_code * pc )
{
   int hh ;
   char sss[8] ;
   integer num_code ;

   if( !isalpha(sym[0]) ) return 0 ;          /* not alphabetic */

   sss[0] = toupper(sym[0]) ; sss[1] = '\0' ; /* uppercase it */

   num_code = (integer) pc->num_code ;

   hh = (int) hassym_( sss , &num_code , pc->c_code ,
                       (ftnlen) 8 , (ftnlen) 8       ) ;

   return hh ;
}

void PARSER_mark_symbols( PARSER_code * pc , int * sl )
{
   int ii ;
   static char abet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

   if( pc == NULL || sl == NULL ) return ;

   for( ii=0 ; ii < 26 ; ii++ )
      sl[ii] = PARSER_has_symbol( abet+ii , pc ) ;

   return ;
}

/*----------------------------------------------------------------------
   pc    = code to evaluate expression
   atoz  = double * [26] containing values for variables A..Z
   nv    = length of vectors
   vout  = double [nv]; will get the output of the expression
             evaluated using the values in atoz

   The only reason for calling this routine instead of
   PARSER_evaluate_one nv different times is efficiency.
------------------------------------------------------------------------*/

void PARSER_evaluate_vector( PARSER_code * pc , double* atoz[] ,
                             int nv , double vout[] )
{
   integer num_code , nvar , ivar , lvec , ldvec ;

   if( pc == NULL || pc->num_code <= 0 ) return ;

   num_code = (integer) pc->num_code ;
   lvec     = (integer) nv ;

   parevec_( &num_code , pc->c_code ,
             (doublereal *) atoz[0]  , (doublereal *) atoz[1] ,
             (doublereal *) atoz[2]  , (doublereal *) atoz[3] ,
             (doublereal *) atoz[4]  , (doublereal *) atoz[5] ,
             (doublereal *) atoz[6]  , (doublereal *) atoz[7] ,
             (doublereal *) atoz[8]  , (doublereal *) atoz[9] ,
             (doublereal *) atoz[10] , (doublereal *) atoz[11] ,
             (doublereal *) atoz[12] , (doublereal *) atoz[13] ,
             (doublereal *) atoz[14] , (doublereal *) atoz[15] ,
             (doublereal *) atoz[16] , (doublereal *) atoz[17] ,
             (doublereal *) atoz[18] , (doublereal *) atoz[19] ,
             (doublereal *) atoz[20] , (doublereal *) atoz[21] ,
             (doublereal *) atoz[22] , (doublereal *) atoz[23] ,
             (doublereal *) atoz[24] , (doublereal *) atoz[25] ,
         &lvec , (doublereal *) vout , (ftnlen) 8 ) ;

   return ;
}

/*----------------------------------------------------------------------
   Evaluate an expression at a set of evenly spaced points.
     expr = expression - first symbol found will be the variable
     nt   = number of points
     tz   = value of first point (the variable)
     dt   = spacing between points
     vec  = pointer to pre-allocated output location of length nt
   Return value is 1 for good results, 0 for errors.

   17 Nov 1999 - RW Cox - adapted from 1deval.c [hence the name]
------------------------------------------------------------------------*/

int PARSER_1deval( char * expr, int nt, float tz, float dt, float * vec )
{
   PARSER_code * pcode = NULL ;
   char sym[4] ;
   double atoz[26] ;
   int ii , kvar ;

   if( expr == NULL || nt <= 0 || vec == NULL ) return 0 ;  /* bad */

   pcode = PARSER_generate_code( expr ) ;        /* compile */
   if( pcode == NULL ) return 0 ;                /* bad news */

   kvar = -1 ;                                   /* find symbol */
   for( ii=0 ; ii < 26 ; ii++ ){
      sym[0] = 'A' + ii ; sym[1] = '\0' ;
      if( PARSER_has_symbol(sym,pcode) ){ kvar = ii ; break ; }
   }

   for( ii=0 ; ii < 26 ; ii++ ) atoz[ii] = 0.0 ; /* initialize */

   if( kvar >= 0 ){                              /* the normal case */
      for( ii=0 ; ii < nt ; ii++ ){
         atoz[kvar] = tz + ii*dt ;
         vec[ii]    = PARSER_evaluate_one( pcode , atoz ) ;
      }
   } else {                                      /* no variable found! */
      vec[0] = PARSER_evaluate_one( pcode , atoz ) ;
      for( ii=1 ; ii < nt ; ii++ ) vec[ii] = vec[0] ;
   }

   free(pcode) ; return 1 ;
}

/*** use the math library to provide Bessel and error functions ***/

doublereal dbesj0_( doublereal * x )
{ return (doublereal) j0( (double) *x ) ; }

doublereal dbesj1_( doublereal * x )
{ return (doublereal) j1( (double) *x ) ; }

doublereal dbesy0_( doublereal * x )
{ return (doublereal) (*x>0) ? y0( (double) *x ) : 0.0 ; }

doublereal dbesy1_( doublereal * x )
{ return (doublereal) (*x>0) ? y1( (double) *x ) : 0.0 ; }

doublereal derf_ ( doublereal * x )
{ return (doublereal) erf( (double) *x ) ; }

doublereal derfc_( doublereal * x )
{ return (doublereal) erfc( (double) *x ) ; }

/**** statistic conversion routines ****/

/*--- macros to create functions ---*/

#undef FUNC3
#undef FUNC2
#undef FUNC1
#undef FUNC0

#define FUNC4(fname,fcode,sfunc)                                      \
   doublereal fname( doublereal * x, doublereal * a, doublereal * b,  \
                                                     doublereal * c ) \
   {  float aux[3] , xx , val ;                                       \
      xx     = (float) (*x) ;                                         \
      aux[0] = (float) (*a) ;                                         \
      aux[1] = (float) (*b) ;                                         \
      aux[2] = (float) (*c) ;                                         \
      val = sfunc( xx , fcode , aux ) ;                               \
      return (doublereal) val ;                                       \
   }

#define FUNC3(fname,fcode,sfunc)                                      \
   doublereal fname( doublereal * x, doublereal * a, doublereal * b ) \
   {  float aux[2] , xx , val ;                                       \
      xx     = (float) (*x) ;                                         \
      aux[0] = (float) (*a) ;                                         \
      aux[1] = (float) (*b) ;                                         \
      val = sfunc( xx , fcode , aux ) ;                               \
      return (doublereal) val ;                                       \
   }

#define FUNC2(fname,fcode,sfunc)                       \
   doublereal fname( doublereal * x , doublereal * a ) \
   {  float aux[1] , xx , val ;                        \
      xx     = (float) (*x) ;                          \
      aux[0] = (float) (*a) ;                          \
      val = sfunc( xx , fcode , aux ) ;                \
      return (doublereal) val ;                        \
   }

#define FUNC1(fname,fcode,sfunc)         \
   doublereal fname( doublereal * x )    \
   {  float  xx , val ;                  \
      xx     = (float) (*x) ;            \
      val = sfunc( xx , fcode , NULL ) ; \
      return (doublereal) val ;          \
   }

#define FUNC_COR_TYPE   2
#define FUNC_TT_TYPE    3
#define FUNC_FT_TYPE    4
#define FUNC_ZT_TYPE    5
#define FUNC_CT_TYPE    6
#define FUNC_BT_TYPE    7
#define FUNC_BN_TYPE    8
#define FUNC_GT_TYPE    9
#define FUNC_PT_TYPE   10

extern float THD_stat_to_pval(float,int,float *) ;
extern float THD_pval_to_stat(float,int,float *) ;
extern float THD_stat_to_zscore(float,int,float *) ;

/*--- now create the functions ---*/

FUNC4(ficotp_,FUNC_COR_TYPE,THD_stat_to_pval)
FUNC4(ficopt_,FUNC_COR_TYPE,THD_pval_to_stat)
FUNC4(ficotz_,FUNC_COR_TYPE,THD_stat_to_zscore)

FUNC2(fitttp_,FUNC_TT_TYPE,THD_stat_to_pval)
FUNC2(fittpt_,FUNC_TT_TYPE,THD_pval_to_stat)
FUNC2(fitttz_,FUNC_TT_TYPE,THD_stat_to_zscore)

FUNC3(fifttp_,FUNC_FT_TYPE,THD_stat_to_pval)
FUNC3(fiftpt_,FUNC_FT_TYPE,THD_pval_to_stat)
FUNC3(fifttz_,FUNC_FT_TYPE,THD_stat_to_zscore)

FUNC1(fizttp_,FUNC_ZT_TYPE,THD_stat_to_pval)
FUNC1(fiztpt_,FUNC_ZT_TYPE,THD_pval_to_stat)
FUNC1(fizttz_,FUNC_ZT_TYPE,THD_stat_to_zscore)

FUNC2(ficttp_,FUNC_CT_TYPE,THD_stat_to_pval)
FUNC2(fictpt_,FUNC_CT_TYPE,THD_pval_to_stat)
FUNC2(ficttz_,FUNC_CT_TYPE,THD_stat_to_zscore)

FUNC3(fibttp_,FUNC_BT_TYPE,THD_stat_to_pval)
FUNC3(fibtpt_,FUNC_BT_TYPE,THD_pval_to_stat)
FUNC3(fibttz_,FUNC_BT_TYPE,THD_stat_to_zscore)

FUNC3(fibntp_,FUNC_BN_TYPE,THD_stat_to_pval)
FUNC3(fibnpt_,FUNC_BN_TYPE,THD_pval_to_stat)
FUNC3(fibntz_,FUNC_BN_TYPE,THD_stat_to_zscore)

FUNC3(figttp_,FUNC_GT_TYPE,THD_stat_to_pval)
FUNC3(figtpt_,FUNC_GT_TYPE,THD_pval_to_stat)
FUNC3(figttz_,FUNC_GT_TYPE,THD_stat_to_zscore)

FUNC2(fipttp_,FUNC_PT_TYPE,THD_stat_to_pval)
FUNC2(fiptpt_,FUNC_PT_TYPE,THD_pval_to_stat)
FUNC2(fipttz_,FUNC_PT_TYPE,THD_stat_to_zscore)
