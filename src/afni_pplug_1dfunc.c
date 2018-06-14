
#include "afni.h"
#include "parser.h"

#ifndef ALLOW_PLUGINS
void F1D_init(void){}
#else

/***********************************************************************
  Pseudo-plugin to set generic 1D func
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: control the 1DChain function\n"
  "\n"
  "The 1DChain transformation allows you to chain together a sequence\n"
  "of up to 8 other 0D or 1D transformation functions.  Each row of\n"
  "the input form controls one transformation.  If it is toggled ON,\n"
  "then you can choose from a menu of functions.  Each ON function is\n"
  "applied in turn.\n"
  "\n"
  "The special 'Expr 9' function lets you compute any expression\n"
  "that depends on the local 9-wide neighborhood of each voxel, where\n"
  "the expression is entered (a la 3dcalc) in the string field at\n"
  "the right of the form.  The variable names that may be used in\n"
  "the expresssion are:\n"
  "                        r s t u v w x y z\n"
  "\n"
  "where 'v' is the center voxel (remember 'v for voxel').  You may\n"
  "also use as a variable any name from previous active chain links\n"
  "(e.g., A,B,C, ...).  For example, on line C, if you select 'Expr 9'\n"
  "as the function and enter\n"
  "\n"
  "             (r+s+t+u+v+w+x+y+z)/9-A\n"
  "\n"
  "for the expression, this will evaluate the average of the 9\n"
  "local voxels and subtract the output of chain link A.\n"
  "On line C, you cannot use variables C or higher, since they\n"
  "won't have been calculated at the time C is being computed.\n"
  "\n"
  "Author -- RW Cox -- Aug 2001"
;

/*----------------- prototypes for internal routines -----------------*/

static char * F1D_main( PLUGIN_interface * ) ;
static void F1D_chainfunc( int , double , double , float * ar ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

#define NUM_CHAIN 8
static char alpha[27] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

#undef  RR
#undef  SS
#undef  TT
#undef  UU
#undef  VV
#undef  WW
#undef  XX
#undef  YY
#undef  ZZ
#define RR 17
#define SS 18
#define TT 19
#define UU 20
#define VV 21
#define WW 22
#define XX 23
#define YY 24
#define ZZ 25

static int              num0D ,     num1D ;
static generic_func ** func0D , ** func1D ;

static int     numfunc  ;
static char ** funcname ;

static int            chain_do[NUM_CHAIN] ;
static PARSER_code *  chain_pc[NUM_CHAIN] ;
static int            chain_dd[NUM_CHAIN] ;
static generic_func * chain_ff[NUM_CHAIN] ;

PLUGIN_interface * F1D_init(void)
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */
   int ii , num , ll ;
   char str[16] ;
   MCW_function_list * rlist ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "1D Chain Func" ,
                                "Control 1DChain function" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())F1D_main  ) ;

   PLUTO_add_hint( plint , "Control 1DChain function" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;  /* 04 Nov 2003 */

   /*-------- Initialize list of all functions that can be chained -----*/

   numfunc     = 1 ;
   funcname    = (char **) malloc(sizeof(char **)) ;
   funcname[0] = "Expr 9 ---->" ;

   /*-------- Get list of pre-registered functions --------*/

   rlist = &(GLOBAL_library.registered_0D) ;
   num0D = rlist->num ;
   if( num0D > 0 ){
     int n0 = 0 ;
     funcname = (char **) realloc( (void *)funcname, sizeof(char **)*(numfunc+num0D) );
     func0D   = (generic_func **) malloc( sizeof(generic_func *)*num0D ) ;
     for( ii=0 ; ii < num0D ; ii++ ){
       if( rlist->flags[ii] == 0 ){            /* 18 Dec 2003: only allow "normal" funcs */
         ll = strlen(rlist->labels[ii]) ;
         funcname[numfunc] = AFMALL(char, ll+8) ;
         strcpy(funcname[numfunc],"0D: ") ;
         strcat(funcname[numfunc],rlist->labels[ii]) ;
         func0D[n0++] = rlist->funcs[ii] ;
         numfunc++ ;
       }
     }
     num0D = n0 ;
   }

   rlist = &(GLOBAL_library.registered_1D) ;
   num1D = rlist->num ;
   if( num1D > 0 ){
     int n1 = 0 ;
     funcname = (char **) realloc( (void *)funcname, sizeof(char **)*(numfunc+num1D) );
     func1D   = (generic_func **) malloc( sizeof(generic_func *)*num1D ) ;
     for( ii=0 ; ii < num1D ; ii++ ){
       if( rlist->flags[ii] == 0 ){            /* 18 Dec 2003: only allow "normal" funcs */
         ll = strlen(rlist->labels[ii]) ;
         funcname[numfunc] = AFMALL(char, ll+8) ;
         strcpy(funcname[numfunc],"1D: ") ;
         strcat(funcname[numfunc],rlist->labels[ii]) ;
         func1D[n1++] = rlist->funcs[ii] ;
         numfunc++ ;
       }
     }
     num1D = n1 ;
   }

   AFNI_register_1D_function( "1DChain" , F1D_chainfunc ) ;  /* add this only now */

   /*--------- make interface lines -----------*/

   for( ii=0 ; ii < NUM_CHAIN ; ii++ ){

      chain_do[ii] = 0 ;     /* off */
      chain_pc[ii] = NULL ;
      chain_ff[ii] = NULL ;

      str[0] = alpha[ii] ; str[1] = '\0' ;

      PLUTO_add_option( plint ,
                        str ,  /* label at left of input line */
                        str ,  /* tag to return to plugin */
                        FALSE  /* is this mandatory? */
                      ) ;

      PLUTO_add_string( plint , "Function"  ,
                        numfunc , funcname , 0 ) ;

      PLUTO_add_string( plint , "Expr 9" , 0,NULL,32 ) ;
   }

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * F1D_main( PLUGIN_interface * plint )
{
   char *tag , *str ;
   int ii,kk,jj , ndone=0 ;

   /*-- turn off all function rows --*/

   for( ii=0 ; ii < NUM_CHAIN ; ii++ ){
      chain_do[ii] = 0 ;
      chain_ff[ii] = NULL ;
      if( chain_pc[ii] != NULL ){ free(chain_pc[ii]); chain_pc[ii]=NULL; }
   }

   /*--------- loop over input lines, re-enable functions rows ---------*/

   while(1){
      tag = PLUTO_get_optiontag(plint) ;  /* "A", "B", etc */
      if( tag == NULL ) break ;

      /* find which variable */

      for( kk=0 ; kk < NUM_CHAIN ; kk++ )
        if( tag[0] == alpha[kk] ) break ;

      if( kk >= NUM_CHAIN ) break ;       /* should not happen */

      chain_do[kk] = 1 ; ndone++ ;        /* mark to do this chain link */

      str = PLUTO_get_string(plint) ;                       /* function name */
      jj  = PLUTO_string_index( str , numfunc,funcname ) ;  /* index of function */

      if( jj < 0 || jj >= numfunc ){ /* should not happen */

         for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
         return "** Internal Error **" ;

      } else if( jj == 0 ){          /* Expr 9 */
         int hasym[26] , ns ;

         str = PLUTO_get_string(plint) ;             /* get expression */
         chain_pc[kk] = PARSER_generate_code(str) ;  /* parse it */
         chain_dd[kk] = -1 ;                         /* code for this case */

         if( chain_pc[kk] == NULL ){
           for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
           return "** Expr 9 parser error **" ;
         }

         /* check symbol usage */

         PARSER_mark_symbols( chain_pc[kk] , hasym ) ;

         for( ii=0 ; ii < kk ; ii++ ){                 /* previous */
           if( hasym[ii] && chain_do[ii] == 0 ){
             for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
             return "** Expr 9 uses inactive symbol **" ;
           }
         }

         if( hasym[kk] ){                              /* current */
           for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
           return "** Expr 9 uses current symbol **" ;
         }

         for( ii=kk+1 ; ii < NUM_CHAIN  ; ii++ ){      /* subsequent */
           if( hasym[ii] ){
             for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
             return "** Expr 9 uses subsequent symbol **" ;
           }
         }

         for( ii=NUM_CHAIN ; ii < RR ; ii++ ){         /* undefined */
           if( hasym[ii] ){
             for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
             return "** Expr 9 uses undefined symbol **" ;
           }
         }

         for( ns=ii=0 ; ii < kk ; ii++ ) if( hasym[ii] ) ns++ ;
         for( ii=RR   ; ii <=ZZ ; ii++ ) if( hasym[ii] ) ns++ ;

         if( ns == 0 ){
           for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
           return "** Expr 9 doesn't use any symbols **" ;
         }

      } else if( jj >= 1 && jj <= num0D ){   /* 0D function */

         chain_dd[kk] = 0 ;            /* code for 0D */
         chain_ff[kk] = func0D[jj-1] ;

      } else {                               /* 1D function */

         chain_dd[kk] = 1 ;            /* code for 1D */
         chain_ff[kk] = func1D[jj-num0D-1] ;
      }

   } /* end of while(1) loop over option lines */

   /*--------- finished -------*/

   if( ndone == 0 ) return " \n** Don't you want to do anything? **\n " ;

   return NULL ;
}

/*-----------------------------------------------------------------------*/

static void F1D_chainfunc( int nx , double to , double dt , float * ar )
{
   int kk,ii,pp , ndo,nexp ;
   float * abc[NUM_CHAIN] , *aprev=ar ;
   double atoz[26] ;

   /* allocate vectors workspace */

   for( nexp=ndo=kk=0 ; kk < NUM_CHAIN ; kk++ ){
     if( chain_do[kk] ){
       abc[kk] = (float *) malloc(sizeof(float)*nx) ;
       ndo++ ;
       if( chain_pc[kk] != NULL ) nexp++ ;
     } else {
       abc[kk] = NULL ;
     }
   }

   if( ndo == 0 ) return ;  /* nothing to do */

   /* loop over chain links */

   for( kk=0 ; kk < NUM_CHAIN ; kk++ ){
     if( !chain_do[kk] ) continue ;     /* skip this link */

     switch( chain_dd[kk] ){

       case 0:                                      /* 0D func */
         memcpy( abc[kk] , aprev , sizeof(float)*nx ) ;
#if 0
         chain_ff[kk]( nx , abc[kk] ) ;
#else
         AFNI_CALL_0D_function( chain_ff[kk] , nx,abc[kk] ) ;
#endif
       break ;

       case 1:                                      /* 1D func */
         memcpy( abc[kk] , aprev , sizeof(float)*nx ) ;
#if 0
         chain_ff[kk]( nx , to,dt , abc[kk] ) ;
#else
         AFNI_CALL_1D_function( chain_ff[kk] , nx,to,dt,abc[kk] ) ;
#endif
       break ;

       case -1:{                                    /* Expr 9 */
         int hasym[26] , jj ;

         PARSER_mark_symbols( chain_pc[kk] , hasym ) ;  /* which symbols to load? */

         for( ii=0 ; ii < nx ; ii++ ){  /* loop over voxels */

           for( pp=0 ; pp < 26 ; pp++ ) atoz[pp] = 0.0 ; /* all variables=0 */

           /* load previous chain vectors at this voxel */

           for( pp=0 ; pp < kk ; pp++ )
             if( hasym[pp] ) atoz[pp] = (double) abc[pp][ii] ;

           /* load local voxels from the immediately previous chain vector */

           if( hasym[RR] ){                         /* load R */
             jj = ii-4 ; if( jj < 0 ) jj = 0 ;
             atoz[RR] = (double) aprev[jj] ;
           }

           if( hasym[SS] ){                         /* load S */
             jj = ii-3 ; if( jj < 0 ) jj = 0 ;
             atoz[SS] = (double) aprev[jj] ;
           }

           if( hasym[TT] ){                         /* load T */
             jj = ii-2 ; if( jj < 0 ) jj = 0 ;
             atoz[TT] = (double) aprev[jj] ;
           }

           if( hasym[UU] ){                         /* load U */
             jj = ii-1 ; if( jj < 0 ) jj = 0 ;
             atoz[UU] = (double) aprev[jj] ;
           }

           if( hasym[VV] ){                         /* load V */
             atoz[VV] = (double) aprev[ii] ;
           }

           if( hasym[WW] ){                         /* load W */
             jj = ii+1 ; if( jj >= nx ) jj = nx-1 ;
             atoz[WW] = (double) aprev[jj] ;
           }

           if( hasym[XX] ){                         /* load X */
             jj = ii+2 ; if( jj >= nx ) jj = nx-1 ;
             atoz[XX] = (double) aprev[jj] ;
           }

           if( hasym[YY] ){                         /* load Y */
             jj = ii+3 ; if( jj >= nx ) jj = nx-1 ;
             atoz[YY] = (double) aprev[jj] ;
           }

           if( hasym[ZZ] ){                         /* load Z */
             jj = ii+4 ; if( jj >= nx ) jj = nx-1 ;
             atoz[ZZ] = (double) aprev[jj] ;
           }

           /* compute this row! */

           abc[kk][ii] = PARSER_evaluate_one( chain_pc[kk] , atoz ) ;

         } /* end of loop over voxels */
       }
       break ;
     }

     aprev = abc[kk] ;  /* for next time, this is previous image */

   } /* end of loop over chain links */

   /* copy last result into input array: this is the result */

   memcpy( ar , aprev , sizeof(float)*nx ) ;

   /* take out the trash */

   for( kk=0 ; kk < NUM_CHAIN ; kk++ )       /* images */
     if( abc[kk] != NULL ) free(abc[kk]) ;

   return ;
}
#endif
