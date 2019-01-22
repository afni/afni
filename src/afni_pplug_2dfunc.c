/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"
#include "parser.h"

#ifndef ALLOW_PLUGINS
void F2D_init(void){}
#else

/***********************************************************************
  Pseudo-plugin to set generic 2D func
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: control the 2DChain function\n"
  "\n"
  "The 2DChain transformation allows you to chain together a sequence\n"
  "of up to 8 other 0D or 2D transformation functions.  Each row of\n"
  "the input form controls one transformation.  If it is toggled ON,\n"
  "then you can choose from a menu of functions.  Each ON function is\n"
  "applied in turn.\n"
  "\n"
  "The special 'Expr 3x3' function lets you compute any expression\n"
  "that depends on the local 3x3 neighborhood of each pixel, where\n"
  "the expression is entered (a la 3dcalc) in the string field at\n"
  "the right of the form.  The variable names that may be used in\n"
  "the expresssion are:\n"
  "                        r s t\n"
  "                        u v w\n"
  "                        x y z\n"
  "\n"
  "where 'v' is the center pixel.  You may also use as a variable\n"
  "any name from previous active chain links (e.g., A,B,C, ...).\n"
  "For example, on line C, if you select 'Expr 3x3' as the function\n"
  "and enter\n"
  "             (r+s+t+u+v+w+x+y+z)/9-A\n"
  "\n"
  "for the expression, this will evaluate the average of the 9\n"
  "local pixels and subtract the output of chain link A.\n"
  "On line C, you cannot use variables C or higher, since they\n"
  "won't have been calculated at the time C is being computed.\n"
  "\n"
  "Author -- RW Cox -- July 2000"
;

/*----------------- prototypes for internal routines -----------------*/

static char * F2D_main( PLUGIN_interface * ) ;
static void F2D_chainfunc( int , int , double , double , float * ) ;

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

static int              num0D ,     num2D ;
static generic_func ** func0D , ** func2D ;

static int     numfunc  ;
static char ** funcname ;

static int            chain_do[NUM_CHAIN] ;
static PARSER_code *  chain_pc[NUM_CHAIN] ;
static int            chain_dd[NUM_CHAIN] ;
static generic_func * chain_ff[NUM_CHAIN] ;

PLUGIN_interface * F2D_init(void)
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */
   int ii , num , ll ;
   char str[16] ;
   MCW_function_list * rlist ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "2D Chain Func" ,
                                "Control 2DChain function" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())F2D_main  ) ;

   PLUTO_add_hint( plint , "Control 2DChain function" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;  /* 04 Nov 2003 */

   /*-------- Initialize list of all functions that can be chained -----*/

   numfunc     = 1 ;
   funcname    = (char **) malloc(sizeof(char **)) ;
   funcname[0] = "Expr 3x3 -->" ;

   /*-------- Get list of pre-registered functions --------*/

   rlist = &(GLOBAL_library.registered_0D) ;
   num0D = rlist->num ;
   if( num0D > 0 ){
     int n0 = 0 ;
     funcname = (char **) realloc( funcname, sizeof(char **)*(numfunc+num0D) );
     func0D   = (generic_func **) malloc( sizeof(generic_func *)*num0D ) ;
     for( ii=0 ; ii < num0D ; ii++ ){
       if( rlist->flags[ii] == 0 ){            /* 18 Dec 2003: only allow "normal" funcs */
         ll = strlen(rlist->labels[ii]) ;
         funcname[numfunc] = AFMALL(char,ll+8) ;
         strcpy(funcname[numfunc],"0D: ") ;
         strcat(funcname[numfunc],rlist->labels[ii]) ;
         func0D[n0++] = rlist->funcs[ii] ;
         numfunc++ ;
       }
     }
     num0D = n0 ;
   }

   rlist = &(GLOBAL_library.registered_2D) ;
   num2D = rlist->num ;
   if( num2D > 0 ){
     int n2 = 0 ;
     funcname = (char **) realloc( funcname, sizeof(char **)*(numfunc+num2D) );
     func2D   = (generic_func **) malloc( sizeof(generic_func *)*num2D ) ;
     for( ii=0 ; ii < num2D ; ii++ ){
       if( rlist->flags[ii] == 0 ){            /* 18 Dec 2003: only allow "normal" funcs */
         ll = strlen(rlist->labels[ii]) ;
         funcname[numfunc] = AFMALL(char, ll+8) ;
         strcpy(funcname[numfunc],"2D: ") ;
         strcat(funcname[numfunc],rlist->labels[ii]) ;
         func2D[n2++] = rlist->funcs[ii] ;
         numfunc++ ;
       }
     }
     num2D = n2 ;
   }

   AFNI_register_2D_function( "2DChain" , F2D_chainfunc ) ;  /* add this only now */

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

      PLUTO_add_string( plint , "Expr 3x3" , 0,NULL,32 ) ;
   }

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * F2D_main( PLUGIN_interface * plint )
{
   char *tag , *str ;
   int ii,kk,jj , ndone=0 ;

   /*-- turn off all rows --*/

   for( ii=0 ; ii < NUM_CHAIN ; ii++ ){
      chain_do[ii] = 0 ;
      chain_ff[ii] = NULL ;
      if( chain_pc[ii] != NULL ){ free(chain_pc[ii]); chain_pc[ii]=NULL; }
   }

   /*--------- loop over input lines ---------*/

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

      } else if( jj == 0 ){          /* Expr 3x3 */
         int hasym[26] , ns ;

         str = PLUTO_get_string(plint) ;             /* get expression */
         chain_pc[kk] = PARSER_generate_code(str) ;  /* parse it */
         chain_dd[kk] = -1 ;                         /* code for this case */

         if( chain_pc[kk] == NULL ){
            for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
            return "** Expr 3x3 parser error **" ;
         }

         /* check symbol usage */

         PARSER_mark_symbols( chain_pc[kk] , hasym ) ;

         for( ii=0 ; ii < kk ; ii++ ){                 /* previous */
           if( hasym[ii] && chain_do[ii] == 0 ){
               for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
               return "** Expr 3x3 uses inactive symbol **" ;
            }
         }

         if( hasym[kk] ){                              /* current */
            for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
            return "** Expr 3x3 uses current symbol **" ;
         }

         for( ii=kk+1 ; ii < NUM_CHAIN  ; ii++ ){      /* subsequent */
            if( hasym[ii] ){
               for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
               return "** Expr 3x3 uses subsequent symbol **" ;
            }
         }

         for( ii=NUM_CHAIN ; ii < RR ; ii++ ){         /* undefined */
            if( hasym[ii] ){
               for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
               return "** Expr 3x3 uses undefined symbol **" ;
            }
         }

         for( ns=ii=0 ; ii < kk ; ii++ ) if( hasym[ii] ) ns++ ;
         for( ii=RR   ; ii <=ZZ ; ii++ ) if( hasym[ii] ) ns++ ;

         if( ns == 0 ){
            for( jj=0 ; jj < NUM_CHAIN ; jj++ ) chain_do[jj] = 0 ;
            return "** Expr 3x3 doesn't use any symbols **" ;
         }

      } else if( jj >= 1 && jj <= num0D ){   /* 0D function */

         chain_dd[kk] = 0 ;            /* code for 0D */
         chain_ff[kk] = func0D[jj-1] ;

      } else {                               /* 2D function */

         chain_dd[kk] = 2 ;            /* code for 2D */
         chain_ff[kk] = func2D[jj-num0D-1] ;
      }

   } /* end of while(1) loop over option lines */

   /*--------- finished -------*/

   if( ndone == 0 ) return " \n** Don't you want to do anything? **\n " ;

   return NULL ;
}

/*-----------------------------------------------------------------------*/

static void F2D_chainfunc( int nx , int ny , double dx, double dy, float * ar )
{
   int kk,ii,pp , ndo,nexp , nxy=nx*ny ;
   float * abc[NUM_CHAIN] , *aprev=ar , *rst,*uvw,*xyz ;
   double * atoz[26] , *tmp=NULL ;

   /* allocate image workspace */

   for( nexp=ndo=kk=0 ; kk < NUM_CHAIN ; kk++ ){
      if( chain_do[kk] ){
         abc[kk] = (float *) malloc(sizeof(float)*nxy) ;
         ndo++ ;
         if( chain_pc[kk] != NULL ) nexp++ ;
      } else {
         abc[kk] = NULL ;
      }
   }

   if( ndo == 0 ) return ;  /* nothing to do */

   /* expression workspace */

   if( nexp > 0 ){
      tmp = (double *) malloc(sizeof(double)*nx) ;
      for( ii=0 ; ii < 26 ; ii++)
         atoz[ii] = (double *) malloc(sizeof(double)*nx) ;
   }

   /* loop over chain links */

   for( kk=0 ; kk < NUM_CHAIN ; kk++ ){
     if( !chain_do[kk] ) continue ;     /* skip this link */

     switch( chain_dd[kk] ){

       case 0:                                      /* 0D func */
         memcpy( abc[kk] , aprev , sizeof(float)*nxy ) ;
#if 0
         chain_ff[kk]( nxy , abc[kk] ) ;
#else
         AFNI_CALL_0D_function( chain_ff[kk] , nxy,abc[kk] ) ;
#endif
       break ;

       case 2:                                      /* 2D func */
         memcpy( abc[kk] , aprev , sizeof(float)*nxy ) ;
#if 0
         chain_ff[kk]( nx,ny , dx,dy , abc[kk] ) ;
#else
         AFNI_CALL_2D_function( chain_ff[kk] , nx,ny,dx,dy,abc[kk] ) ;
#endif
       break ;

      case -1:{                                    /* Expr 3x3 */
        int hasym[26] , jj ;

        PARSER_mark_symbols( chain_pc[kk] , hasym ) ;  /* which symbols to load? */

        for( jj=0 ; jj < ny ; jj++ ){  /* loop over rows */

           uvw = aprev + jj*nx ;                     /* row containing u,v,w */

           rst = (jj == 0) ? uvw                     /* row containing r,s,t */
                           : aprev + (jj-1)*nx ;

           xyz = (jj == ny-1 ) ? uvw                 /* row containing x,y,z */
                               : aprev + (jj+1)*nx ;

           /* initialize all variables to 0 */

           for( pp=0 ; pp < 26 ; pp++){
             for( ii=0 ; ii < nx ; ii++ ) atoz[pp][ii] = 0.0 ;
           }

           /* load previous images */

           for( pp=0 ; pp < kk ; pp++ ){
             if( hasym[pp] ){
               for( ii=0 ; ii < nx ; ii++ )
                 atoz[pp][ii] = (double) abc[pp][ii+jj*nx] ;
             }
           }

           if( hasym[RR] ){                         /* load R */
             atoz[RR][0] = (double) rst[0] ;
             for( ii=1 ; ii < nx ; ii++ )
               atoz[RR][ii] = (double) rst[ii-1] ;
           }

           if( hasym[SS] ){                         /* load S */
             for( ii=0 ; ii < nx ; ii++ )
               atoz[SS][ii] = (double) rst[ii] ;
           }

           if( hasym[TT] ){                         /* load T */
             for( ii=0 ; ii < nx-1 ; ii++ )
               atoz[TT][ii] = (double) rst[ii+1] ;
             atoz[TT][nx-1] = (double) rst[nx-1] ;
           }

           if( hasym[UU] ){                         /* load U */
             atoz[UU][0] = (double) uvw[0] ;
             for( ii=1 ; ii < nx ; ii++ )
               atoz[UU][ii] = (double) uvw[ii-1] ;
           }

           if( hasym[VV] ){                         /* load V */
             for( ii=0 ; ii < nx ; ii++ )
               atoz[VV][ii] = (double) uvw[ii] ;
           }

           if( hasym[WW] ){                         /* load W */
             for( ii=0 ; ii < nx-1 ; ii++ )
               atoz[WW][ii] = (double) uvw[ii+1] ;
             atoz[WW][nx-1] = (double) uvw[nx-1] ;
           }

           if( hasym[XX] ){                         /* load X */
             atoz[XX][0] = (double) xyz[0] ;
             for( ii=1 ; ii < nx ; ii++ )
               atoz[XX][ii] = (double) xyz[ii-1] ;
           }

           if( hasym[YY] ){                         /* load Y */
             for( ii=0 ; ii < nx ; ii++ )
               atoz[YY][ii] = (double) xyz[ii] ;
           }

           if( hasym[ZZ] ){                         /* load Z */
             for( ii=0 ; ii < nx-1 ; ii++ )
               atoz[ZZ][ii] = (double) xyz[ii+1] ;
             atoz[ZZ][nx-1] = (double) xyz[nx-1] ;
           }

           /* compute this row! */

           PARSER_evaluate_vector( chain_pc[kk] , atoz , nx , tmp ) ;

           /* store back in output row */

           uvw = abc[kk] + jj*nx ;
           for( ii=0 ; ii < nx ; ii++ ) uvw[ii] = (float) tmp[ii] ;

        } /* end of loop over rows */
       }
       break ;
     }

     aprev = abc[kk] ;  /* for next time, this is previous image */
   }

   /* copy last result into input array: this is the result */

   memcpy( ar , aprev , sizeof(float)*nxy ) ;

   /* take out the trash */

   for( kk=0 ; kk < NUM_CHAIN ; kk++ )       /* images */
     if( abc[kk] != NULL ) free(abc[kk]) ;

   if( nexp > 0 ){
     for( ii=0 ; ii < 26 ; ii++ ) free(atoz[ii]) ;  /* expression variables */
     free(tmp) ;
   }

   return ;
}
#endif
