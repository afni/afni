#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to provide a L1 fitting 1D function for graphs
************************************************************************/

/*------------- string to 'help' the user -------------*/

static char helpstring[] =
   " Purpose: Control the 'L1_Fit' and 'L1_Dtr 1D functions.\n"
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

/*------- Strings for baseline control ------*/

#define NBASE 4
static char * baseline_strings[NBASE] = { "Constant", "Linear", "Quadratic", "Cubic" } ;

/*--------------- prototypes for internal routines ---------------*/

char * L1F_main( PLUGIN_interface * ) ;  /* the entry point */

void L1F_fitter() ;
void L1F_detrend() ;
void L1F_worker() ;

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

PLUGIN_interface * PLUGIN_init( int ncall )
{
   int ii ;
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "L1_Fit & Dtr" ,
                                "Control L1_Fit and L1_Dtr Functions" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , L1F_main ) ;

   global_plint = plint ;  /* make global copy */

   PLUTO_set_sequence( plint , "A:funcs:fitting" ) ;

   PLUTO_add_hint( plint , "Control L1_Fit and L1_Dtr Functions" ) ;

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

   PLUTO_register_1D_funcstr( "L1_Fit" , L1F_fitter ) ;
   PLUTO_register_1D_funcstr( "L1_Dtr" , L1F_detrend ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

char * L1F_main( PLUGIN_interface * plint )
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

void L1F_fitter( int nt , double to , double dt , float * vec , char ** label )
{
   L1F_worker( nt , dt , vec , TRUE , label ) ;
   return ;
}

void L1F_detrend( int nt , double to , double dt , float * vec , char ** label )
{
   L1F_worker( nt , dt , vec , FALSE , label ) ;
   return ;
}

static char lbuf[4096] ;  /* 22 Apr 1997: will hold label for graphs */
static char sbuf[256] ;

void L1F_worker( int nt , double dt , float * vec , int dofit , char ** label )
{
   int nlen , nref ;

   static int nlen_old = -666 , nref_old = -666 ;
   static double dt_old = -666.666 ;
   static float ** ref = NULL ;
   static float *  fit = NULL ;

   int ir , ii , ks,jh ;
   float fac , tm , val , cls ;
   float * tsar ;

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
      if( fit != NULL ) free(fit) ;

      /* make space for ref vectors */

      ref = (float **) malloc( sizeof(float *) * nref ) ;
      if( ref == NULL ){fprintf(stderr,"\nmalloc error in plug_lsqfit\n\a");EXIT(1);}
      for( ir=0 ; ir < nref ; ir++ ){
         ref[ir] = (float *) malloc( sizeof(float) * nlen ) ;
         if( ref[ir] == NULL )
            {fprintf(stderr,"\nmalloc error in plug_lsqfit\n\a");EXIT(1);}
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
            fprintf(stderr,"Inadequate time series #%d in L1F plugin\n\a",ks+1) ;
            return ;
         }
         tsar = MRI_FLOAT_PTR(tsim[ks]) ;
         for( ii=0 ; ii < nlen ; ii++ ) ref[ir][ii] = tsar[ii+ignore] ;
         ir++ ;
      }

      /* make space for fit vector */

      fit = (float *) malloc(sizeof(float)*nref) ;

      initialize = 0 ;
   }

   /** find L1 fit coefficients **/

   cls = cl1_solve( nlen , nref , vec+ignore , ref , fit,0 ) ;

   if( cls < 0.0 ) return ;  /* bad fit */

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

      for( ; ir <= polort ; ){
         sprintf(sbuf,"Coef of t**%d = %g\n" , ir,fit[ir++] ) ;
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

   return ;
}
