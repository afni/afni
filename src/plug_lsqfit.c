/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to provide a least squares fitting 1D function for graphs
************************************************************************/

/*------------- string to 'help' the user -------------*/

static char helpstring[] =
   " Purpose: Control the 'LSqFit' and 'LSqDtr 1D functions.\n"
   "\n"
   " Parameters:  Baseline = 'Constant' or 'Linear'\n"
   "                           Is the baseline 'a' or 'a+b*t'?\n"
   "              Ignore   = Number of points to ignore at\n"
   "                           start of each timeseries.\n"
   " \n"
   " Sinusoids:   Period    = Fundamental period to use.\n"
   "              Harmonics = Number of overtones to use.\n"
   " \n"
   " Timeseries:  File      = Input timeseries file to use.\n"
;

static char plehstring[] =
   " Purpose: Generate a timeseries and store in AFNI list\n"
   "\n"
   " Parameters:  Delta  = time step between points\n"
   "              Length = number of points\n"
   "\n"
   " Output:      Label  = String to label timeseries by\n"
   "                        in AFNI choosers\n"
   "\n"
   " Polynomial:  Order  = Maximum power to include\n"
   "                       (Chebyshev polynomials are used)\n"
   "\n"
   " Sinusoid:    Period    = Fundamental period to use.\n"
   "              Harmonics = Number of overtones to use.\n"
;

/*------- Strings for baseline control ------*/

#define NBASE 3
static char * baseline_strings[NBASE] = { "Constant" , "Linear" , "Quadratic" } ;

/*--------------- prototypes for internal routines ---------------*/

char * LSQ_main( PLUGIN_interface * ) ;  /* the entry point */

void LSQ_fitter( int nt, double to, double dt, float * vec, char ** label ) ;
void LSQ_detrend( int nt, double to, double dt, float * vec, char ** label ) ;
void LSQ_worker( int nt, double dt, float * vec, int dofit, char ** label ) ;

PLUGIN_interface * TSGEN_init(void) ;
char * TSGEN_main( PLUGIN_interface * ) ;

PLUGIN_interface * EXP0D_init(void) ;
char * EXP0D_main( PLUGIN_interface * ) ;
void EXP0D_worker( int num , float * vec ) ;

#ifdef ALLOW_LOMO
PLUGIN_interface * LOMOR_init(void) ;
char * LOMOR_main( PLUGIN_interface * ) ;
void LOMOR_fitter() ;
#endif

/*---------------- global data -------------------*/

static PLUGIN_interface * global_plint = NULL ;

#define NRMAX_SIN 2
#define NRMAX_TS  2
#define HARM_MAX  22

static int polort=1 , ignore=3 , nrsin=0 , nrts=0 , initialize=1 ;
static float sinper[NRMAX_SIN] ;
static int   sinharm[NRMAX_SIN] ;
static MRI_IMAGE * tsim[NRMAX_TS] ;

/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset"    for a dataset chooser,
        "PLUTO_add_string"     for a string chooser,
        "PLUTO_add_number"     for a number chooser,
        "PLUTO_add_timeseries" for a timeseries chooser.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   int ii ;
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 3 ) return NULL ;  /* generate interfaces for ncall 0-3 */

   if( ncall == 1 ) return TSGEN_init() ;  /* interface # 1 */
   if( ncall == 2 ) return EXP0D_init() ;  /* interface # 2 */
#ifdef ALLOW_LOMO
   if( ncall == 3 ) return LOMOR_init() ;  /* interface # 3 */
#else
   if( ncall == 3 ) return NULL ;
#endif

   /***** otherwise, do interface # 0 *****/

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "LSqFit & Dtr" ,
                                "Control LSqFit and LSqDtr Functions" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , LSQ_main ) ;

   global_plint = plint ;  /* make global copy */

   PLUTO_set_sequence( plint , "A:funcs:fitting" ) ;

   PLUTO_add_hint( plint , "Control LSqFit and LSqDtr Functions" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;  /* 04 Nov 2003 */

   /*----- Parameters -----*/

   PLUTO_add_option( plint , "Parameters" , "Parameters" , TRUE ) ;

   PLUTO_add_string( plint , "Baseline" , NBASE , baseline_strings , 1 ) ;

   PLUTO_add_number( plint , "Ignore" , 0,20,0,3 , FALSE ) ;

   /*----- Sinusoid -----*/

   for( ii=0 ; ii < NRMAX_SIN ; ii++ ){
      PLUTO_add_option( plint , "Sinusoid" , "Sinusoid" , FALSE ) ;
      PLUTO_add_number( plint , "Period" , 0,99999,0,20, TRUE ) ;
      PLUTO_add_number( plint , "Harmonics" , 1,HARM_MAX,0,1 , FALSE ) ;
   }

   /*----- Timeseries -----*/

   for( ii=0 ; ii < NRMAX_TS ; ii++ ){
      PLUTO_add_option( plint , "Timeseries" , "Timeseries" , FALSE ) ;
      PLUTO_add_timeseries( plint , "File" ) ;
   }

   /*--------- done with interface setup ---------*/

   PLUTO_register_1D_funcstr( "LSqFit" , LSQ_fitter ) ;
   PLUTO_register_1D_funcstr( "LSqDtr" , LSQ_detrend ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

char * LSQ_main( PLUGIN_interface * plint )
{
   char * str ;
   int  ii ;
   float * tsar ;

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   str    = PLUTO_get_string(plint) ;
   polort = PLUTO_string_index( str , NBASE , baseline_strings ) ;

   ignore = PLUTO_get_number(plint) ;

   /*------ loop over remaining options, check their tags, process them -----*/

   nrsin = nrts = 0 ;
   do {
      str = PLUTO_get_optiontag(plint) ; if( str == NULL ) break ;

      if( strcmp(str,"Sinusoid") == 0 ){

         sinper[nrsin]  = PLUTO_get_number(plint) ;
         sinharm[nrsin] = PLUTO_get_number(plint) - 1.0 ;
         if( sinper[nrsin] <= 0.0 )
            return "************************\n"
                   "Illegal Sinusoid Period!\n"
                   "************************"  ;

         nrsin++ ;

      } else if( strcmp(str,"Timeseries") == 0 ){

         tsim[nrts] = PLUTO_get_timeseries(plint) ;

         if( tsim[nrts] == NULL || tsim[nrts]->nx < 3 || tsim[nrts]->kind != MRI_float )
            return "*************************\n"
                   "Illegal Timeseries Input!\n"
                   "*************************"  ;

         tsar = MRI_FLOAT_PTR(tsim[nrts]) ;
         for( ii=ignore ; ii < tsim[nrts]->nx && tsar[ii] >= WAY_BIG ; ii++ ) ; /* nada */
         ignore = ii ;
         nrts++ ;

      } else {
         return "************************\n"
                "Illegal optiontag found!\n"
                "************************"  ;
      }
   } while(1) ;

   /*--- nothing left to do until data arrives ---*/

   initialize = 1 ;  /* force re-initialization */

   /*** compute how many ref functions are ordered ***/

   { int nref , ks ;
     char str[64] ;

     nref = (polort+1) + nrts ;
     for( ks=0 ; ks < nrsin ; ks++ ) nref += 2*(sinharm[ks]+1) ;
     sprintf(str," \nNumber of fit parameters = %d\n",nref) ;
     PLUTO_popup_transient( plint , str ) ;
   }

   return NULL ;
}

/*---------------------------------------------------------------*/

/** 22 Apr 1997: added label that will go to graphs **/

void LSQ_fitter( int nt , double to , double dt , float * vec , char ** label )
{
   LSQ_worker( nt , dt , vec , TRUE , label ) ;
   return ;
}

void LSQ_detrend( int nt , double to , double dt , float * vec , char ** label )
{
   LSQ_worker( nt , dt , vec , FALSE , label ) ;
   return ;
}

static char lbuf[4096] ;  /* 22 Apr 1997: will hold label for graphs */
static char sbuf[256] ;

void LSQ_worker( int nt , double dt , float * vec , int dofit , char ** label )
{
   int nlen , nref ;

   static int nlen_old = -666 , nref_old = -666 ;
   static double dt_old = -666.666 ;
   static float ** ref = NULL ;
   static double * dch = NULL ;

   int ir , ii , ks,jh , j;
   float fac , tm , val ;
   float * fit , * tsar ;

   /*** compute how many ref functions are ordered ***/

    nref = (polort+1) + nrts ;
    for( ks=0 ; ks < nrsin ; ks++ ) nref += 2*(sinharm[ks]+1) ;

   /*** do nothing if not enough data to fit ***/

   nlen = nt - ignore ;

   if( nlen <= nref ) return ;  /* do nothing if not enough data to fit */

   /** if data vectors are new length,
       or have a new number of reference vectors,
       or have a new time step and need sinusoids,
       or the initialize flag is set,
       then reinitialize reference vectors and Choleski factor **/

   if( nlen != nlen_old || nref != nref_old ||
       initialize       || (dt != dt_old && nrsin > 0) ){

      /* free old storage */

      if( ref != NULL ){
         for( ir=0 ; ir < nref_old ; ir++ ) if( ref[ir] != NULL ) free(ref[ir]) ;
         free(ref) ;
      }
      if( dch != NULL ) free(dch) ;

      /* make space for ref vectors */

      ref = (float **) malloc( sizeof(float *) * nref ) ;
      if( ref == NULL ){fprintf(stderr,"\nmalloc error in plug_lsqfit\n\a");
         return;
         /* EXIT(1); */
      }
      for( ir=0 ; ir < nref ; ir++ ){
         ref[ir] = (float *) malloc( sizeof(float) * nlen ) ;
         if( ref[ir] == NULL ){
            fprintf(stderr,"\nmalloc error in plug_lsqfit\n\a");
            for(j=0;j<=ir;j++)
	      free(ref[j]);
	    free(ref);
            ref = NULL;
            return;
            /* EXIT(1); */
         }
      }
      nlen_old = nlen ;
      nref_old = nref ;
      dt_old   = dt ;

      /**** fill ref vectors ****/

      /* r(t) = 1 */

      for( ii=0 ; ii < nlen ; ii++ ) ref[0][ii] = 1.0 ;

      ir = 1 ;
      if( polort > 0 ){

         /* r(t) = t - tmid */

         tm = 0.5 * (nlen-1.0) ; fac = 2.0 / nlen ;
         for( ii=0 ; ii < nlen ; ii++ ) ref[1][ii] = (ii-tm)*fac ;
         ir = 2 ;

         /* r(t) = (t-tmid)**ir */

         for( ; ir <= polort ; ir++ )
            for( ii=0 ; ii < nlen ; ii++ )
               ref[ir][ii] = pow( (ii-tm)*fac , (double)ir ) ;
      }

      if( dt == 0.0 ) dt = 1.0 ;

      /* r(t) = sinusoids */

      for( ks=0 ; ks < nrsin ; ks++ ){
         for( jh=0 ; jh <= sinharm[ks] ; jh++ ){
            fac = (2.0*PI) * dt * (jh+1) / sinper[ks] ;
            for( ii=0 ; ii < nlen ; ii++ ){
               ref[ir]  [ii] = cos( fac * ii ) ;
               ref[ir+1][ii] = sin( fac * ii ) ;
            }
            ir += 2 ;
         }
      }

      /* r(t) = timeseries files */

      for( ks=0 ; ks < nrts ; ks++ ){
         if( tsim[ks] == NULL || tsim[ks]->nx - ignore < nlen ){
            initialize = 1 ;
            fprintf(stderr,"Inadequate time series #%d in LSQ plugin\n\a",ks+1) ;
            return ;
         }
         tsar = MRI_FLOAT_PTR(tsim[ks]) ;
         for( ii=0 ; ii < nlen ; ii++ ) ref[ir][ii] = tsar[ii+ignore] ;
         ir++ ;
      }

      /* Cholesky-ize */

      dch = startup_lsqfit( nlen , NULL , nref , ref ) ;
      if( dch == NULL ){
         initialize = 1 ;
         fprintf(stderr,"Choleski error in LSQ plugin\n\a") ;
         return ;
      }

      initialize = 0 ;
   }

   /** find least squares fit coefficients **/

   fit = delayed_lsqfit( nlen , vec+ignore , nref , ref , dch ) ;

   for( ii=0 ; ii < nlen ; ii++ ){
      val = 0.0 ;
      for( ir=0 ; ir < nref ; ir++ ) val += fit[ir] * ref[ir][ii] ;

      vec[ii+ignore] = (dofit) ? val : vec[ii+ignore] - val ;
   }

   /** 22 Apr 1997: create label if desired by AFNI         **/
   /** [This is in static storage, since AFNI will copy it] **/

   if( label != NULL ){  /* assemble this 1 line at a time from sbuf */

      lbuf[0] = '\0' ;   /* make this a 0 length string to start */

      /** for each reference, make a string into sbuf **/

      ir = 0 ;
      sprintf(sbuf,"Coef of 1 = %g\n" , fit[ir++] ) ;
      strcat(lbuf,sbuf) ;

      for( ; ir <= polort ; ir++  ){
         sprintf(sbuf,"Coef of t**%d = %g\n" , ir,fit[ir] ) ;
         strcat(lbuf,sbuf) ;
      }

      for( ks=0 ; ks < nrsin ; ks++ ){
         for( jh=0 ; jh <= sinharm[ks] ; jh++ ){
            fac = sinper[ks] / (jh+1) ;
            sprintf(sbuf,"Coef of cos(2*Pi*t/%g) = %g\n" , fac , fit[ir++] ) ;
            strcat(lbuf,sbuf) ;
            sprintf(sbuf,"Coef of sin(2*Pi*t/%g) = %g\n" , fac , fit[ir++] ) ;
            strcat(lbuf,sbuf) ;
         }
      }

      for( ks=0 ; ks < nrts ; ks++ ){
         sprintf(sbuf,"Coef of %s = %g\n" , tsim[ks]->name , fit[ir++] ) ;
         strcat(lbuf,sbuf) ;
      }

      *label = lbuf ;  /* send address of lbuf back in what label points to */
   }

   free(fit) ;
   return ;
}

/*********************************************************************************/

PLUGIN_interface * TSGEN_init(void)
{
   PLUGIN_interface * plint ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "TS Generate" ,
                                "Generate a Timeseries" ,
                                plehstring ,
                                PLUGIN_CALL_VIA_MENU , TSGEN_main ) ;

   PLUTO_add_hint( plint , "Generate a 1D Timeseries" ) ;

   /*----- Parameters -----*/

   PLUTO_add_option( plint , "Parameters" , "Parameters" , TRUE ) ;
   PLUTO_add_number( plint , "Delta"  , 0,99999,1, 0 , TRUE ) ;
   PLUTO_add_number( plint , "Length" , 3,9999,0,3   , TRUE ) ;

   /*----- Output -----*/

   PLUTO_add_option( plint , "Output" , "Output" , TRUE ) ;
   PLUTO_add_string( plint , "Label" , 0,NULL , 19 ) ;

   /*----- Polynomial -----*/

   PLUTO_add_option( plint , "Polynomial" , "Polynomial" , FALSE ) ;
   PLUTO_add_number( plint , "Order" , 2,20,0,2 , FALSE ) ;

   /*----- Sinusoid -----*/

   PLUTO_add_option( plint , "Sinusoid" , "Sinusoid" , FALSE ) ;
   PLUTO_add_number( plint , "Period" , 0,99999,0,20, TRUE ) ;
   PLUTO_add_number( plint , "Harmonics" , 1,HARM_MAX,0,1 , FALSE ) ;

   return plint ;
}

char * TSGEN_main( PLUGIN_interface * plint )
{
   char * label , * str ;
   int  ii , jj ;
   float * tsar ;
   MRI_IMAGE * tsim ;
   float delta , period=0.0 ;
   int   nx , ny=0 , npol=0 , nharm=-1 ;
   int pp ;
   double fac , val ;

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   delta = PLUTO_get_number(plint) ;
   if( delta <= 0.0 ) return "**********************\n"
                             "Illegal value of Delta\n"
                             "**********************"  ;

   nx = PLUTO_get_number(plint) ;

   /*----- next input line -----*/

   PLUTO_next_option(plint) ;
   label = PLUTO_get_string(plint) ;
   if( label == NULL || strlen(label) == 0 ) return "**********************\n"
                                                    "Illegal value of Label\n"
                                                    "**********************"  ;

   /*----- rest of input lines -----*/

   do {
      str = PLUTO_get_optiontag(plint) ; if( str == NULL ) break ;

      if( strcmp(str,"Sinusoid") == 0 ){

         period = PLUTO_get_number(plint) ;
         nharm  = PLUTO_get_number(plint) - 1.0 ;

         if( period <= 0.0 ) return "***********************\n"
                                    "Illegal Sinusoid Period\n"
                                    "***********************"  ;


      } else if( strcmp(str,"Polynomial") == 0 ){

         npol = PLUTO_get_number(plint) ;

      } else {
         return "***********************\n"
                "Illegal optiontag found\n"
                "***********************"  ;
      }
   } while(1) ;

   /********** Make the timeseries ***********/

   ny = 0 ;
   if( npol > 0 )     ny  = npol-1 ;
   if( period > 0.0 ) ny += 2*(nharm+1) ;

   if( ny < 1 ) return "***********************\n"
                       "No timeseries specified\n"
                       "***********************"  ;

   tsim = mri_new( nx , ny , MRI_float ) ;
   jj   = 0 ;

   fac  = 1.99999 / (nx-1) ;
   for( pp=2 ; pp <= npol ; pp++,jj++ ){

      tsar = MRI_FLOAT_PTR(tsim) + (jj*nx) ;

      for( ii=0 ; ii < nx ; ii++ ){
         val = fac * ii - 0.999995 ;
         tsar[ii] = cos( pp * acos(val) ) ;
      }
   }

   for( pp=0 ; pp <= nharm ; pp++ , jj+=2 ){
      fac  = (2.0*PI) * delta * (pp+1) / period ;
      tsar = MRI_FLOAT_PTR(tsim) + (jj*nx) ;

      for( ii=0 ; ii < nx ; ii++ ){
         tsar[ii]    = cos( fac * ii ) ;
         tsar[ii+nx] = sin( fac * ii ) ;
      }
   }

   PLUTO_register_timeseries( label , tsim ) ;
   mri_free(tsim) ;
   return NULL ;
}

/***************************************************************************/

#include "parser.h"

static char fredstring[] =
  " Purpose: Control the Expr 0D Transformation function\n"
  "\n"
  " Variable   = letter used as input to expression\n"
  " Expression = arithmetic expression to evaluate\n"
;

#define NALPHA 26
static char * vstring[NALPHA] = {
  " A ",  " B ",  " C ",  " D ",  " E ",
  " F ",  " G ",  " H ",  " I ",  " J ",
  " K ",  " L ",  " M ",  " N ",  " O ",
  " P ",  " Q ",  " R ",  " S ",  " T ",
  " U ",  " V ",  " W ",  " X ",  " Y ",  " Z " } ;

static int exp0d_var = 23 ;

static PARSER_code * exp0d_pc = NULL ;

static PLUGIN_interface *plint_EXP0D=NULL ;

static void EXP0D_func_init(void)   /* 21 Jul 2003 */
{
   PLUG_startup_plugin_CB( NULL , (XtPointer)plint_EXP0D , NULL ) ;
}


PLUGIN_interface * EXP0D_init(void)
{
   PLUGIN_interface * plint ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Expr 0D" ,
                                "Control the Expr 0D transformation" ,
                                fredstring ,
                                PLUGIN_CALL_VIA_MENU , EXP0D_main ) ;

   PLUTO_add_option( plint , "Variable" , "Variable" , TRUE ) ;
   PLUTO_add_string( plint , NULL , NALPHA,vstring , exp0d_var ) ;

   PLUTO_add_option( plint , "Expression" , "Expression" , TRUE ) ;
   PLUTO_add_string( plint , NULL , 0,NULL , 50 ) ;

   PLUTO_register_0D_function( "Expr 0D" , EXP0D_worker ) ;

   plint_EXP0D = plint ;
   AFNI_register_nD_func_init( 0 , (generic_func *)EXP0D_func_init ) ;  /* 21 Jul 2003 */

   return plint ;
}

char * EXP0D_main( PLUGIN_interface * plint )
{
   char * str ;

   PLUTO_next_option(plint) ;
   str       = PLUTO_get_string(plint) ;
   exp0d_var = PLUTO_string_index( str , NALPHA , vstring ) ;

   if( exp0d_pc != NULL ){ free(exp0d_pc) ; exp0d_pc = NULL ; }
   PLUTO_next_option(plint) ;
   str       = PLUTO_get_string(plint) ;
   exp0d_pc  = PARSER_generate_code( str ) ;

   if( exp0d_pc == NULL ) return "*******************************\n"
                                 "Error when compiling expression\n"
                                 "*******************************"  ;

   return NULL ;
}

#define VSIZE 64

void EXP0D_worker( int num , float * vec )
{
   int ii , jj , jbot,jtop ;

   static int first = 1 ;
   static double * atoz[26] ;
   static double   tvec[VSIZE] ;

   if( num <= 0 || vec == NULL || exp0d_pc == NULL ) return ;

#if 0
fprintf(stderr,"Enter EXP0D_worker\n") ;
#endif

   if( first ){
      for( ii=0 ; ii < 26 ; ii++)
        atoz[ii] = (double *) malloc(sizeof(double) * VSIZE ) ;
      first = 0 ;
#if 0
fprintf(stderr,"Allocated atoz\n") ;
#endif
   }

   for( ii=0 ; ii < 26 ; ii++ )
      for (jj=0; jj<VSIZE; jj++) atoz[ii][jj] = 0.0 ;

#if 0
fprintf(stderr,"Zeroed atoz\n") ;
#endif

   for( ii=0 ; ii < num ; ii+=VSIZE ){
      jbot = ii ;
      jtop = MIN( ii + VSIZE , num ) ;

      for( jj=jbot ; jj < jtop ; jj ++ ) atoz[exp0d_var][jj-ii] = vec[jj] ;

      PARSER_evaluate_vector( exp0d_pc , atoz , jtop-jbot , tvec ) ;

      for( jj=jbot ; jj < jtop ; jj ++ ) vec[jj] = tvec[jj-ii] ;
   }

#if 0
fprintf(stderr,"Exit EXP0D_worker\n") ;
#endif

   return ;
}

/*------------------------------------------------------------------*/

#ifdef ALLOW_LOMO
static int lomo_order  = 3 ;
static int lomo_levels = 32 ;
int lomo_regress( int , int , int * , int * ) ;

PLUGIN_interface * LOMOR_init(void)
{
   PLUGIN_interface * plint ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Lomo Regression" ,
                                "Control the Local Monotone Regression" ,
                                NULL ,
                                PLUGIN_CALL_VIA_MENU , LOMOR_main ) ;

   PLUTO_add_option( plint , "Parameters" , "Parameters" , TRUE ) ;
   PLUTO_add_number( plint , "Order" , 3, 20,0,lomo_order  , FALSE ) ;
   PLUTO_add_number( plint , "Levels", 4,128,0,lomo_levels , FALSE ) ;

   PLUTO_register_1D_function( "Lomo 1D" , LOMOR_fitter ) ;

   return plint ;
}

char * LOMOR_main( PLUGIN_interface * plint )
{
   PLUTO_next_option(plint) ;
   lomo_order  = PLUTO_get_number(plint) ;
   lomo_levels = PLUTO_get_number(plint) ;
   return NULL ;
}

static int lnum   = 0 ;
static int * xin  = NULL ;
static int * yout = NULL ;

void LOMOR_fitter( int num , double to , double dt , float * vec )
{
   int ii ;
   float top , bot , scl ;

   if( num <= 3 || vec == NULL ) return ;

   if( lnum < num ){
      if( xin != NULL ){ free(xin) ; free(yout) ; }
      xin  = (int *) malloc( sizeof(int) * num ) ;
      yout = (int *) malloc( sizeof(int) * num ) ;
      if( xin == NULL || yout == NULL ) return ;
      lnum = num ;
   }

   bot = top = vec[0] ;
   for( ii=1 ; ii < num ; ii++ ){
           if( vec[ii] < bot ) bot = vec[ii] ;
      else if( vec[ii] > top ) top = vec[ii] ;
   }

   if( bot >= top ) return ;
   scl = (lomo_levels - 0.01) / (top-bot) ;

fprintf(stderr,"LOMO: range %f .. %f; scl=%f\n",bot,top,scl) ;

   for( ii=0 ; ii < num ; ii++ ) xin[ii] = (int)((vec[ii]-bot)*scl) ;

   ii = lomo_regress( num , lomo_order , xin , yout ) ;
   if( ii == -1 ) return ;

   scl = 1.0 / scl ;
   for( ii=0 ; ii < num ; ii++ ) vec[ii] = bot + yout[ii]*scl ;
   return ;
}

/*******************************************************/
/* Fast Digital Locally Monotonic Regression           */
/*******************************************************/
/*               Copyright (c) 1995                    */
/*    University of Maryland at College Park           */
/*               All Rights Reserved                   */
/*******************************************************/
/*  by Nicholas Sidiropoulos, Aug.  1995               */
/*******************************************************/
/* compile using -lm option                            */
/*******************************************************/
/* input: from stdinput.dat: standard matlab vector    */
/*        (i.e., ASCII file containing                 */
/*               a long line of N vector elements      */
/*               separated by spaces)                  */
/* output: in stdoutput.dat.*: same format as input    */
/* control: in stdcontrol_switch.dat: the              */
/*          ``effective'' M is  */
/*          the first entry here, followed by `\n`     */
/*          Rest: fixed to 1 (future option)           */
/*          So stdcontrol_switch .dat should be ASCII  */
/*          file starting with the effective M \n      */
/*          and followed by N `1''s \n, e.g., for M=10 */
/*  10<newline>1<newline>...1<newline>                 */
/*                 a total of N ones                   */
/*******************************************************/
/* This is just an ``Academic'' piece of software -    */
/* it has been checked for correctness to the best     */
/* of my ability, however, no guarantees whatsoever    */
/* are given - all disclaimers here!                   */
/* It has been optimized for minimum development effort*/
/* and NOT for optimum speed and/or minimum comput.    */
/* resources. Note, in particular, that I do not take  */
/* advantage of trellis path merging to reduce storage */
/* requirements. As a result, depending on your choice */
/* of parameters M,N,R, the program may require        */
/* considerable amounts of static memory. Therefore,   */
/* if your computer lacks it you need to rlogin to     */
/* a machine (like oxygen or apollo at SIL lab) which  */
/* has sufficient memory                               */
/*******************************************************/

/*=====================================================*/
/*== Modified Feb 1997 by RWCox, to be a C function. ==*/
/*=====================================================*/

#include <stdio.h>
#include <math.h>
#include <string.h>

typedef struct _state {
  int value, length, cumcost;
  struct _state *prevstate;
} state;

#ifdef ABS
#undef ABS
#endif
#define ABS(x) (((x)<0) ? (-x) : (x))

#define USE_STATIC_TRELLIS
#ifdef USE_STATIC_TRELLIS
#  define MMAX 35    /* (strictly) > max lomo degree   */
#  define NMAX 512   /* input data length              */
#  define RMAX 100   /* range of input: 0 -> R-1 inc.  */
   static state TRELLIS[RMAX][2*MMAX][NMAX] ;
#  define trellis(i,j,k) TRELLIS[i][j][k]
#else
   static state * TRELLIS = NULL ;
   static int     TDIM1 , TDIM2 , TDIM12 , TDIM3 ;
#  define trellis(i,j,k) TRELLIS[i+j*TDIM1+k*TDIM12]
#endif

/*=====================================================*/
/*==   N   = number of input points                  ==*/
/*==   m   = local monotonicity order to impose      ==*/
/*==   yin = pointer to inputs  (array of length N)  ==*/
/*==   x   = pointer to outputs (array of length N)  ==*/
/*==                                                 ==*/
/*==   return value is 0 if all is OK, -1 if not     ==*/
/*=====================================================*/

int lomo_regress( int N , int m , int * yin , int * x )
{
  int base , R , * y ;
  int n,v,l,pv,pl,i,j;
  int cost, maxcost;
  int peakval;
  state dummy_state;      /* dummy initial state */

  /*== check inputs for OK-ness ==*/

  if( N < 3 || m < 3 || m >= N || yin == NULL || x == NULL ) return -1 ;

  /*== Compute range of input into R ==*/

  y = (int *) malloc(sizeof(int)*N) ; if( y == NULL ) return -1 ;
  base = yin[0] ;
  for( i=1 ; i < N ; i++ ) if( yin[i] < base ) base = yin[i] ;
  R = y[0] = yin[0] - base ;
  for( i=1 ; i < N ; i++ ){
     y[i] = yin[i] - base ; if( y[i] > R ) R = y[i] ;
  }
  R++ ;
  if( R == 1 ){
      for( i=0 ; i < N ; i++ ) x[i] = yin[i] ;
      free(y) ; return 0 ;
  }

fprintf(stderr,"LOMO: %d points; %d levels; %d order\n",N,R,m) ;

  /* compute maxcost */

  maxcost = 0;
  for (n = 0; n < N; n++) maxcost += ABS(y[n]);

  /* now init FLOMOR : */

  dummy_state.value   = -1; dummy_state.length    = m   ;
  dummy_state.cumcost = 0 ; dummy_state.prevstate = NULL;

#ifndef USE_STATIC_TRELLIS
  /*== malloc trellis space ==*/

  TDIM1 = R ; TDIM2 = 2*m+2 ; TDIM12 = TDIM1*TDIM2 ; TDIM3 = N ;
  TRELLIS = (state *) calloc( TDIM1*TDIM2*TDIM3 , sizeof(state) ) ;
  if( TRELLIS == NULL ){ free(y) ; return -1 ; }
#endif

fprintf(stderr,"LOMO: init trellis\n") ;

  for (n = 0; n < N; n++)
  {
    for (l = 1; l <= 2*m; l++)
    {
      for (v = 0; v < R; v++) { trellis(v,l,n).value = v;
                                trellis(v,l,n).length = l;
                                if ((n == 0) && ((l == 1) || (l == m+1)))
                                { trellis(v,l,n).cumcost   = ABS((v-y[0]));
                                  trellis(v,l,n).prevstate = &dummy_state;
                                }
                                else
                                { trellis(v,l,n).cumcost   = maxcost;
                                  trellis(v,l,n).prevstate = NULL;
                                }
                              }
    }
  }

/************************ main FLOMOR loop ************************************/
/* note that l = -1, ..., -m is mapped to l = m+1, ..., 2m resp.              */
/******************************************************************************/

fprintf(stderr,"LOMO: compute trellis") ; fflush(stderr) ;

  for (n = 1; n < N; n++)
  {
    /* states of the first type: (v,1) */

      fprintf(stderr,".") ; fflush(stderr) ;

      for (v = 0; v < R; v++)
      {
        for (pv = 0; pv < v; pv++)
        {
          for (pl = 1; pl <= m; pl++)
          {
            if (trellis(pv,pl,n-1).cumcost != maxcost)
            {
              cost = trellis(pv,pl,n-1).cumcost + ABS((v-y[n]));
              if (cost < trellis(v,1,n).cumcost)
              {
                trellis(v,1,n).cumcost = cost;
                trellis(v,1,n).prevstate = &trellis(pv,pl,n-1);
              }
            }
          }
          if (trellis(pv,2*m,n-1).cumcost != maxcost)
          {
            cost = trellis(pv,2*m,n-1).cumcost + ABS((v-y[n]));
            if (cost < trellis(v,1,n).cumcost)
            {
              trellis(v,1,n).cumcost = cost;
              trellis(v,1,n).prevstate = &trellis(pv,2*m,n-1);
            }
          }
        }
      }

    /* states of the second type: (v,-1) */

      for (v = 0; v < R; v++)
      {
        for (pv = R - 1; pv > v; pv--)
        {
          for (pl = m + 1; pl <= 2*m; pl++)
          {
            if (trellis(pv,pl,n-1).cumcost != maxcost)
            {
              cost = trellis(pv,pl,n-1).cumcost + ABS((v-y[n]));
              if (cost < trellis(v,m+1,n).cumcost)
              {
                trellis(v,m+1,n).cumcost = cost;
                trellis(v,m+1,n).prevstate = &trellis(pv,pl,n-1);
              }
            }
          }
          if (trellis(pv,m,n-1).cumcost != maxcost)
          {
            cost = trellis(pv,m,n-1).cumcost + ABS((v-y[n]));
            if (cost < trellis(v,m+1,n).cumcost)
            {
              trellis(v,m+1,n).cumcost = cost;
              trellis(v,m+1,n).prevstate = &trellis(pv,m,n-1);
            }
          }
        }
      }

    /* states of the third type: (v,l), 1 < l < m */

    for (l = 2; l < m; l++)
    {
      for (v = 0; v < R; v++)
      {
        if (trellis(v,l-1,n-1).cumcost != maxcost)
        {
          trellis(v,l,n).cumcost = trellis(v,l-1,n-1).cumcost + ABS((v-y[n]));
          trellis(v,l,n).prevstate = &trellis(v,l-1,n-1);
        }
      }
    }

    /* states of the fourth type: (v,l), -m < l < -1 */

    for (l = m+2; l < 2*m; l++)
    {
      for (v = 0; v < R; v++)
      {
        if (trellis(v,l-1,n-1).cumcost != maxcost)
        {
          trellis(v,l,n).cumcost = trellis(v,l-1,n-1).cumcost + ABS((v-y[n]));
          trellis(v,l,n).prevstate = &trellis(v,l-1,n-1);
        }
      }
    }

    /* states of the fifth type: (v,l), l = m */

    for (v = 0; v < R; v++)
    {
      if (trellis(v,m,n-1).cumcost != maxcost)
      {
        cost = trellis(v,m,n-1).cumcost + ABS((v-y[n]));
        if (cost < trellis(v,m,n).cumcost)
        {
          trellis(v,m,n).cumcost = cost;
          trellis(v,m,n).prevstate = &trellis(v,m,n-1);
        }
      }

      if (trellis(v,m-1,n-1).cumcost != maxcost)
      {
        cost = trellis(v,m-1,n-1).cumcost + ABS((v-y[n]));
        if (cost < trellis(v,m,n).cumcost)
        {
          trellis(v,m,n).cumcost = cost;
          trellis(v,m,n).prevstate = &trellis(v,m-1,n-1);
        }
      }
    }

    /* states of the sixth type: (v,l), l = -m */

    for (v = 0; v < R; v++)
    {
      if (trellis(v,2*m,n-1).cumcost != maxcost)
      {
        cost = trellis(v,2*m,n-1).cumcost + ABS((v-y[n]));
        if (cost < trellis(v,2*m,n).cumcost)
        {
          trellis(v,2*m,n).cumcost = cost;
          trellis(v,2*m,n).prevstate = &trellis(v,2*m,n-1);
        }
      }

      if (trellis(v,2*m-1,n-1).cumcost != maxcost)
      {
        cost = trellis(v,2*m-1,n-1).cumcost + ABS((v-y[n]));
        if (cost < trellis(v,2*m,n).cumcost)
        {
          trellis(v,2*m,n).cumcost = cost;
          trellis(v,2*m,n).prevstate = &trellis(v,2*m-1,n-1);
        }
      }
    }


  }

/************************ eof main FLOMOR loop *******************************/

  /* now pick best path, and write it to x[n] */

fprintf(stderr,"\nLOMO: pick best path\n") ; fflush(stderr) ;

  cost = maxcost;
  for (l = 1; l <= m; l++)
  {
    for (v = 0; v < R; v++)
    {
      if (trellis(v,l,N-1).cumcost < cost)
      {
        cost = trellis(v,l,N-1).cumcost;
        pl = l; pv = v;
      }
    }
  }

  /* now traverse backwards */

  if (cost < maxcost)
  {
    x[N-1] = pv;
fprintf(stderr,"  [%d] = %d\n",N-1,pv) ;

    for (n = N-2; n >=0; n--)
    {
fprintf(stderr,"  [%d] = %d",n,trellis(pv,pl,n+1).prevstate->value) ; fflush(stderr) ;
      x[n] = trellis(pv,pl,n+1).prevstate->value;
fprintf(stderr,"  pv = %d",trellis(pv,pl,n+1).prevstate->value) ; fflush(stderr) ;
      pv = trellis(pv,pl,n+1).prevstate->value;
fprintf(stderr,"  pl = %d",trellis(pv,pl,n+1).prevstate->length) ; fflush(stderr) ;
      pl = trellis(pv,pl,n+1).prevstate->length;
fprintf(stderr,"\n") ;
    }
  }
  else { for (n=0;n<N;n++) x[n] = 0; /* as good as any... */ }

  /*== add base back to output ==*/

fprintf(stderr,"\nLOMO: add base back on\n") ;

  for( i=0 ; i < N ; i++ ) x[n] += base ;

fprintf(stderr,"LOMO: exit regression\n") ;

  free(y) ;
#ifndef USE_STATIC_TRELLIS
  free(TRELLIS) ;
#endif

  return 0 ;
}
#endif /* ALLOW_LOMO */
