/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------
This program is revised for 3D+time data calculation,
  [Raoqiong Tong, August 1997]

Added ability to use a 1D time series file as a "dataset" -- see TS variables.
  [RW Cox, April 1998]

Added ability to operate on 3D bucket datasets.
  [RW Cox, April 1998]

Added ability to use sub-brick selectors on input datasets.
  [RW Cox, Jan 1999]

Modified output to scale each sub-brick to shorts/bytes separately
  [RW Cox, Mar 1999]

Modifed sub-brick selection of type "-b3 name+view" to mangle dataset
into form "name+view[3]", since that code works better on 3D+time.
Modified TS_reader to use new mri_read_1D() function, instead of
mri_read_ascii().
Added -histpar option.
Added the _dshift stuff.
  [RW Cox, Nov 1999]

Modified help menu
  [P Christidis, July 2005]
----------------------------------------------------------------------------*/

#include "mrilib.h"
#include "parser.h"

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

static int                CALC_datum = ILLEGAL_TYPE ;
static int                CALC_nvox  = -1 ;
static PARSER_code *      CALC_code  = NULL ;
static int                ntime[26] ;
static int                ntime_max = 0 ;
static int                CALC_fscale = 0 ;  /* 16 Mar 1998 */
static int                CALC_gscale = 0 ;  /* 01 Apr 1999 */
static int                CALC_nscale = 0 ;  /* 15 Jun 2000 */

static int                CALC_histpar = -1; /* 22 Nov 1999 */

static int                CALC_usetemp = 0 ; /* 18 Oct 2005 */

#undef  ALLOW_FDR  /* this was purely experimental */
#ifdef  ALLOW_FDR
static int                CALC_fdrize  = 0 ; /* 17 Jan 2008 */
#endif

#define ALLOW_SORT /* this is not experimental, but is pretty useless */
#ifdef  ALLOW_SORT
static int                CALC_sort    = 0 ; /* 22 Jan 2008 */
#endif

/*---------- dshift stuff [22 Nov 1999] ----------*/

#define DSHIFT_MODE_STOP  0
#define DSHIFT_MODE_WRAP  1
#define DSHIFT_MODE_ZERO  2

static int                CALC_dshift     [26] ; /* 22 Nov 1999 */
static int                CALC_dshift_i   [26] ;
static int                CALC_dshift_j   [26] ;
static int                CALC_dshift_k   [26] ;
static int                CALC_dshift_l   [26] ;
static int                CALC_dshift_mode[26] ;

static int                CALC_dshift_mode_current = DSHIFT_MODE_STOP ;
static int                CALC_has_timeshift       = 0 ;

/*------------------------------------------------*/

static int   CALC_has_sym[26] ;                      /* 15 Sep 1999 */
static char  abet[] = "abcdefghijklmnopqrstuvwxyz" ;

#define HAS_I  CALC_has_sym[ 8]
#define HAS_J  CALC_has_sym[ 9]
#define HAS_K  CALC_has_sym[10]
#define HAS_X  CALC_has_sym[23]
#define HAS_Y  CALC_has_sym[24]
#define HAS_Z  CALC_has_sym[25]
#define HAS_T  CALC_has_sym[19]  /* 19 Nov 1999 */
#define HAS_L  CALC_has_sym[11]  /* 19 Nov 1999 */

#define PREDEFINED_MASK ((1<< 8)|(1<< 9)|(1<<10)|(1<<11)| \
                         (1<<19)|(1<<23)|(1<<24)|(1<<25) )

static int     CALC_has_predefined = 0 ;  /* 19 Nov 1999 */
static int     CALC_has_xyz        = 0 ;  /* 17 May 2005 */
static int     CALC_mangle_xyz     = 0 ;  /* 17 May 2005 */

#define MANGLE_NONE 0
#define MANGLE_RAI  1
#define MANGLE_LPI  2

static THD_3dim_dataset *  CALC_dset[26] ;
static int                 CALC_type[26] ;
static byte **             CALC_byte[26] ;
static short **            CALC_short[26] ;
static float **            CALC_float[26] ;
static complex **          CALC_complex[26] ; /* 10 Mar 2006 */
static int                 CALC_cxcode[26] ;
static float *             CALC_ffac[26] ;
static int                 CALC_noffac[26] ;  /* 14 Nov 2003 */

static int                 CALC_verbose = 0 ; /* 30 April 1998 */

static char CALC_output_prefix[THD_MAX_PREFIX] = "calc" ;

static char CALC_session[THD_MAX_NAME]         = "./"   ;

static MRI_IMAGE * TS_flim[26] ;  /* 17 Apr 1998 */
static float *     TS_flar[26] ;
static int         TS_nmax = 0 ;
static int         TS_make = 0 ;
static float       TS_dt   = 1.0 ; /* 13 Aug 2001 */

static MRI_IMAGE * IJKAR_flim[26] ;  /* 22 Feb 2005 */
static float *     IJKAR_flar[26] ;
static int         IJKAR_dcod[26] ;

/* this macro tells if a variable (index 0..25) is defined,
   either by a time series file or an input dataset - 16 Nov 1999 */

#define VAR_DEFINED(kv) \
   (TS_flim[kv]   != NULL || IJKAR_flim[kv]  != NULL || \
    CALC_dset[kv] != NULL || CALC_dshift[kv] >= 0      )

static float Rfac = 0.299 ;  /* 10 Feb 2002: for RGB inputs */
static float Gfac = 0.587 ;
static float Bfac = 0.114 ;

static int   CALC_taxis_num = 0 ;    /* 28 Apr 2003 */

#define CX_REALPART  0               /* 10 Mar 2006: complex to real methods */
#define CX_IMAGPART  1
#define CX_MAGNITUDE 2
#define CX_PHASE     3
static int CUR_cxcode = CX_MAGNITUDE ;  /* default conversion method */

/*----------------------------------------------------------------------------*/
static char *tempfnam = NULL ;  /* 18 Oct 2005: -usetemp stuff */
static FILE *tempfile = NULL ;

static void calc_atexit(void) /*-- called by exit(): delete tempfile --*/
{
   if( tempfile != NULL ){ fclose(tempfile) ; tempfile = NULL ; }
   if( tempfnam != NULL ){
     INFO_message("Deleting -usetemp file %s",tempfnam) ;
     remove(tempfnam) ; tempfnam = NULL ;
   }
   return ;
}

/*--------------------------- prototypes ---------------------------*/
void CALC_read_opts( int , char ** ) ;
void CALC_Syntax(void) ;
int  TS_reader( int , char * ) ;
int  IJKAR_reader( int , char * ) ;

/*--------------------------------------------------------------------
  Read a time series file into TS variable number ival.
  Returns -1 if an error occured, 0 otherwise.
----------------------------------------------------------------------*/

int TS_reader( int ival , char *fname )
{
   MRI_IMAGE *tsim ;

   if( ival < 0 || ival >= 26 ) return -1 ;

   tsim = mri_read_1D( fname ) ;  /* 16 Nov 1999: replaces mri_read_ascii */
   if( tsim == NULL ) return -1 ;
   if( tsim->nx < 2 ){ mri_free(tsim) ; return -1 ; }

   TS_flim[ival] = tsim ;
   TS_nmax       = MAX( TS_nmax , TS_flim[ival]->nx ) ;
   TS_flar[ival] = MRI_FLOAT_PTR( TS_flim[ival] ) ;
   return 0 ;
}

/*--------------------------------------------------------------------
  Read a time series file into IJK variable number ival.
  Returns -1 if an error occured, 0 otherwise.
----------------------------------------------------------------------*/

int IJKAR_reader( int ival , char *fname )  /* 22 Feb 2005 */
{
   MRI_IMAGE *tsim ;

   if( ival < 0 || ival >= 26 ) return -1 ;

   tsim = mri_read_1D( fname ) ;  /* 16 Nov 1999: replaces mri_read_ascii */
   if( tsim == NULL ) return -1 ;
   if( tsim->nx < 2 ){ mri_free(tsim) ; return -1 ; }

   IJKAR_flim[ival] = tsim ;
   IJKAR_flar[ival]  = MRI_FLOAT_PTR( IJKAR_flim[ival] ) ;
   return 0 ;
}

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void CALC_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   int ids ;
   int ii, kk;

   for( ids=0 ; ids < 26 ; ids++ ){
      CALC_dset[ids]   = NULL ;
      CALC_type[ids]   = -1 ;
      TS_flim[ids]     = NULL ;
      IJKAR_flim[ids]  = NULL ;  /* 22 Feb 2005 */

      CALC_dshift[ids]      = -1 ;                        /* 22 Nov 1999 */
      CALC_dshift_mode[ids] = CALC_dshift_mode_current ;

      CALC_noffac[ids] = 1 ;   /* 14 Nov 2003 */
   }

   while( nopt < argc && argv[nopt][0] == '-' ){
#ifdef USE_TRACING
      if( strncmp(argv[nopt],"-trace",5) == 0 ){ DBG_trace=1; nopt++; continue; }
      if( strncmp(argv[nopt],"-TRACE",5) == 0 ){ DBG_trace=2; nopt++; continue; }
#endif
      /**** -dicom, -RAI, -LPI, -SPM [18 May 2005] ****/

      if( strcasecmp(argv[nopt],"-dicom") == 0 || strcasecmp(argv[nopt],"-rai") == 0 ){
        CALC_mangle_xyz = MANGLE_RAI ;
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-spm") == 0 || strcasecmp(argv[nopt],"-lpi") == 0 ){
        CALC_mangle_xyz = MANGLE_LPI ;
        nopt++ ; continue ;
      }

      /**** -rgbfac r g b [10 Feb 2003] ****/

      if( strncasecmp(argv[nopt],"-rgbfac",7) == 0 ){
        if( ++nopt >= argc )
          ERROR_exit("need an argument after -rgbfac!\n") ;
        Rfac = strtod( argv[nopt++] , NULL ) ;
        Gfac = strtod( argv[nopt++] , NULL ) ;
        Bfac = strtod( argv[nopt++] , NULL ) ;
        if( Rfac == 0.0 && Gfac == 0.0 && Bfac == 0.0 )
          ERROR_exit("All 3 factors after -rgbfac are zero!?\n");
        continue ;
      }

#ifdef ALLOW_FDR
      /**** -fdrize ****/  /* [[ not in -help at this time !! ]] */

      if( strcasecmp(argv[nopt],"-fdrize") == 0 ){  /* 17 Jan 2008 */
        CALC_fdrize++ ; CALC_datum = MRI_float ; nopt++ ; continue ;
      }
#endif

#ifdef ALLOW_SORT
      /**** -sort ****/

      if( strcmp(argv[nopt],"-sort") == 0 ){  /* 22 Jan 2008 */
        CALC_sort = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-SORT") == 0 ){
        CALC_sort = -1 ; nopt++ ; continue ;
      }
#endif

      /**** -cx2r code [10 Mar 2006] ***/

      if( strncasecmp(argv[nopt],"-cx2r",5) == 0 ){
        if( ++nopt >= argc )
          ERROR_exit("need an argument after -cx2r!\n") ;
             if( strncasecmp(argv[nopt],"real",4) == 0 )  CUR_cxcode=CX_REALPART ;
        else if( strncasecmp(argv[nopt],"imag",4) == 0 )  CUR_cxcode=CX_IMAGPART ;
        else if( strncasecmp(argv[nopt],"mag",3) == 0 ||
                 strncasecmp(argv[nopt],"abs",3) == 0   ) CUR_cxcode=CX_MAGNITUDE;
        else if( strncasecmp(argv[nopt],"pha",3) == 0 ||
                 strncasecmp(argv[nopt],"arc",3) == 0   ) CUR_cxcode=CX_PHASE    ;
        else {
                                                          CUR_cxcode=CX_MAGNITUDE;
          WARNING_message("Don't understand '-cx2r %s' - using ABS",argv[nopt]);
        }
        nopt++ ; continue ;
      }

      /**** -taxis N:dt [28 Apr 2003] ****/

      if( strncasecmp(argv[nopt],"-taxis",6) == 0 ){
        char *cpt ;
        if( ++nopt >= argc )
          ERROR_exit("need an argument after -taxis!\n") ;
        CALC_taxis_num = strtod( argv[nopt] , &cpt ) ;
        if( CALC_taxis_num < 2 )
          ERROR_exit("N value after -taxis must be bigger than 1!\n");
        if( *cpt == ':' ){
          float dt = strtod( cpt+1 , &cpt ) ;
          if( dt > 0.0 ){
            TS_dt = dt ;
            if( *cpt == 'm' && *(cpt+1) == 's' ) TS_dt *= 0.001 ;  /* 09 Mar 2004 */
          } else {
            WARNING_message("time step value in '-taxis %s' not legal!\n",argv[nopt]);
          }
        }
        nopt++ ; continue ;  /* go to next arg */
      }

      /**** -usetemp [18 Oct 2005] ****/

      if( strncmp(argv[nopt],"-usetemp",6) == 0 ){
        CALC_usetemp = 1 ;
        nopt++ ; continue ;  /* go to next arg */
      }

      /**** -dt val [13 Aug 2001] ****/

      if( strncasecmp(argv[nopt],"-dt",3) == 0 || strncmp(argv[nopt],"-TR",3) == 0 ){
        char *cpt ;
        if( ++nopt >= argc )
          ERROR_exit("need an argument after -dt!\n") ;
        TS_dt = strtod( argv[nopt] , &cpt ) ;
        if( TS_dt <= 0.0 )
          ERROR_exit("Illegal time step value after -dt!\n");
        if( *cpt == 'm' && *(cpt+1) == 's' ) TS_dt *= 0.001 ;  /* 09 Mar 2004 */
        nopt++ ; continue ;  /* go to next arg */
      }

      /**** -histpar letter [22 Nov 1999] ****/

      if( strncasecmp(argv[nopt],"-histpar",5) == 0 ){
         if( ++nopt >= argc )
           ERROR_exit("need an argument after -histpar!\n") ;
         if( argv[nopt][0] < 'a' || argv[nopt][0] > 'z')
            ERROR_exit("argument after -histpar is illegal!\n");
         CALC_histpar = (int) (argv[nopt][0] - 'a') ;

         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -float and -short and -byte [22 Jan 2008] ****/

      if( strcasecmp(argv[nopt],"-float") == 0 ){
        CALC_datum = MRI_float ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-short") == 0 ){
        CALC_datum = MRI_short ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-byte") == 0 ){
        CALC_datum = MRI_byte ; nopt++ ; continue ;
      }

      /**** -datum type ****/

      if( strncasecmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc )
           ERROR_exit("need an argument after -datum!\n") ;
         if( strcasecmp(argv[nopt],"short") == 0 ){
            CALC_datum = MRI_short ;
         } else if( strcasecmp(argv[nopt],"float") == 0 ){
            CALC_datum = MRI_float ;
         } else if( strcasecmp(argv[nopt],"byte") == 0 ){
            CALC_datum = MRI_byte ;
#if 0
         } else if( strcasecmp(argv[nopt],"complex") == 0 ){  /* not listed in help */
            CALC_datum = MRI_complex ;
#endif
         } else {
            ERROR_exit("-datum of type '%s' not supported in 3dcalc!\n",argv[nopt]) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -verbose [30 April 1998] ****/

      if( strncasecmp(argv[nopt],"-verbose",5) == 0 ){
         CALC_verbose = 1 ;
         nopt++ ; continue ;
      }

      /**** -nscale [15 Jun 2000] ****/

      if( strncasecmp(argv[nopt],"-nscale",6) == 0 ){
         CALC_gscale = CALC_fscale = 0 ;
         CALC_nscale = 1 ;
         nopt++ ; continue ;
      }

      /**** -fscale [16 Mar 1998] ****/

      if( strncasecmp(argv[nopt],"-fscale",6) == 0 ){
         CALC_fscale = 1 ;
         CALC_nscale = 0 ;
         nopt++ ; continue ;
      }

      /**** -gscale [01 Apr 1999] ****/

      if( strncasecmp(argv[nopt],"-gscale",6) == 0 ){
         CALC_gscale = CALC_fscale = 1 ;
         CALC_nscale = 0 ;
         nopt++ ; continue ;
      }

      /**** -prefix prefix ****/

      if( strncasecmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc )
           ERROR_exit("need argument after -prefix!\n") ;
         MCW_strncpy( CALC_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }
      /**** -session directory ****/

      if( strncasecmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc )
           ERROR_exit("need argument after -session!\n") ;
         MCW_strncpy( CALC_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -expr expression ****/

      if( strncasecmp(argv[nopt],"-expr",4) == 0 ){
         if( CALC_code != NULL )
           ERROR_exit("cannot have 2 -expr options!\n") ;
         nopt++ ;
         if( nopt >= argc )
            ERROR_exit("need argument after -expr!\n") ;
         PARSER_set_printout(1) ;  /* 21 Jul 2003 */
         CALC_code = PARSER_generate_code( argv[nopt++] ) ;
         if( CALC_code == NULL )
            ERROR_exit("illegal expression!\n") ;
         PARSER_mark_symbols( CALC_code , CALC_has_sym ) ; /* 15 Sep 1999 */
         continue ;
      }

      /**** -dsSTOP [22 Nov 1999] ****/

      if( strncasecmp(argv[nopt],"-dsSTOP",6) == 0 ){
         CALC_dshift_mode_current = DSHIFT_MODE_STOP ;
         nopt++ ; continue ;
      }

      /**** -dsWRAP [22 Nov 1999] ****/

      if( strncasecmp(argv[nopt],"-dsWRAP",6) == 0 ){
         CALC_dshift_mode_current = DSHIFT_MODE_WRAP ;
         nopt++ ; continue ;
      }

      /**** -dsZERO [22 Nov 1999] ****/

      if( strncasecmp(argv[nopt],"-dsZERO",6) == 0 ){
         CALC_dshift_mode_current = DSHIFT_MODE_ZERO ;
         nopt++ ; continue ;
      }

      /**** -<letter>[number] dataset ****/

      ids = strlen( argv[nopt] ) ;

      if( (argv[nopt][1] >= 'a' && argv[nopt][1] <= 'z') &&
          (ids == 2 ||
           (ids > 2 && argv[nopt][2] >= '0' && argv[nopt][2] <= '9')) ){

         int ival , nxyz , isub , ll ;
         THD_3dim_dataset * dset ;

         ival = argv[nopt][1] - 'a' ;
         if( VAR_DEFINED(ival) )
            ERROR_exit("Can't define %c symbol twice\n",argv[nopt][1]);

         isub = (ids == 2) ? 0 : strtol(argv[nopt]+2,NULL,10) ;
         if( isub < 0 )
            ERROR_exit("Illegal sub-brick value: %s\n",argv[nopt]) ;

         nopt++ ;
         if( nopt >= argc )
            ERROR_exit("need argument after %s\n",argv[nopt-1]);

         /*-- 22 Feb 2005: allow for I:, J:, K: prefix --*/

         ll = strlen(argv[nopt]) ;
         if( ll >= 4                         &&
             strstr(argv[nopt],"1D") != NULL &&
             argv[nopt][1] == ':'            &&
             (argv[nopt][0] == 'I' || argv[nopt][0] == 'i' ||
              argv[nopt][0] == 'J' || argv[nopt][0] == 'j' ||
              argv[nopt][0] == 'K' || argv[nopt][0] == 'k'   ) ){

           ll = IJKAR_reader( ival , argv[nopt]+2 ) ;
           if( ll == 0 ){
             switch( argv[nopt][0] ){
               case 'I': case 'i': IJKAR_dcod[ival] =  8 ; break ;
               case 'J': case 'j': IJKAR_dcod[ival] =  9 ; break ;
               case 'K': case 'k': IJKAR_dcod[ival] = 10 ; break ;
             }
             nopt++ ; goto DSET_DONE ;
           }
         }

         /*-- 17 Apr 1998: allow for a *.1D filename --*/

         ll = strlen(argv[nopt]) ;
         if( ll >= 4 && ( strstr(argv[nopt],".1D") != NULL ||
                          strstr(argv[nopt],"1D:") != NULL   )
                     && strstr(argv[nopt],"'") == NULL       ){

            ll = TS_reader( ival , argv[nopt] ) ;
            if( ll == 0 ){ nopt++ ;  goto DSET_DONE ; }

            /* get to here => something bad happened, so try it as a dataset */
         }

         /*-- 22 Nov 1999: allow for a differentially
                           subscripted name, as in "-b a[1,0,0,0]" --*/

         if( (argv[nopt][0] >= 'a' && argv[nopt][0] <= 'z') &&  /* legal name */
             ( (ll >= 3 && argv[nopt][1] == '[') ||             /* subscript */
               (ll == 3 &&                                      /*    OR    */
                (argv[nopt][1] == '+' || argv[nopt][1] == '-')) /* +- ijkl */
             ) ){

            int jds = argv[nopt][0] - 'a' ;  /* actual dataset index */
            int * ijkl ;                     /* array of subscripts */

            /*- sanity checks -*/

            if( ids > 2 )
              ERROR_exit("Can't combine %s with differential subscripting %s\n",
                         argv[nopt-1],argv[nopt]) ;
            if( CALC_dset[jds] == NULL )
              ERROR_exit("Must define dataset %c before using it in %s\n",
                         argv[nopt][0] , argv[nopt] ) ;

            /*- get subscripts -*/

            if( argv[nopt][1] == '[' ){            /* format is [i,j,k,l] */
               MCW_intlist_allow_negative(1) ;
               ijkl = MCW_get_intlist( 9999 , argv[nopt]+1 ) ;
               MCW_intlist_allow_negative(0) ;
               if( ijkl == NULL || ijkl[0] != 4 )
                 ERROR_exit("Illegal differential subscripting %s\n",
                            argv[nopt] ) ;
            } else {                               /* format is +i, -j, etc */
                ijkl = (int *) malloc( sizeof(int) * 5 ) ;
                ijkl[1] = ijkl[2] = ijkl[3] = ijkl[4] = 0 ;  /* initialize */
                switch( argv[nopt][2] ){
                   default:
                     ERROR_exit("Bad differential subscripting %s\n",argv[nopt]);

                   case 'i': ijkl[1] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                   case 'j': ijkl[2] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                   case 'k': ijkl[3] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                   case 'l': ijkl[4] = (argv[nopt][1]=='+') ? 1 : -1 ; break ;
                }
            }

            /*- more sanity checks -*/

            if( ijkl[1]==0 && ijkl[2]==0 && ijkl[3]==0 && ijkl[4]==0 )
              WARNING_message("differential subscript %s is all zero\n",argv[nopt]);

            if( ntime[jds] == 1 && ijkl[4] != 0 ){
               WARNING_message(
                       "differential subscript %s has nonzero time\n"
                       " +        shift on base dataset with 1 sub-brick!\n"
                       " +        Setting time shift to 0.\n" ,
                       argv[nopt] ) ;
               ijkl[4] = 0 ;
            }

            /*- set values for later use -*/

            CALC_dshift  [ival] = jds ;
            CALC_dshift_i[ival] = ijkl[1] ;
            CALC_dshift_j[ival] = ijkl[2] ;
            CALC_dshift_k[ival] = ijkl[3] ;
            CALC_dshift_l[ival] = ijkl[4] ;

            CALC_dshift_mode[ival] = CALC_dshift_mode_current ;
            CALC_cxcode[ival]      = CUR_cxcode ;  /* 10 Mar 2006 */

            CALC_has_timeshift = CALC_has_timeshift || (ijkl[4] != 0) ;

            /*- time to trot, Bwana -*/

            free(ijkl) ; nopt++ ; goto DSET_DONE ;

         } /* end of _dshift */

         /*-- meanwhile, back at the "normal" dataset opening ranch --*/

         { char dname[512] ;                               /* 02 Nov 1999 */
           char *fname = argv[nopt];          /* 8 May 2007 [rickr,dglen] */

           if( ids > 2 ){                                  /* mangle name */
              if( strstr(argv[nopt],"[") != NULL ){
                ERROR_exit(
                         "Illegal combination of sub-brick specifiers: "
                         "%s %s\n" ,
                         argv[nopt-1] , argv[nopt] ) ;
              }
              sprintf(dname,"%s[%d]",argv[nopt++],isub) ;  /* use sub-brick */
              fname = dname ;
              isub = 0 ;                                   /* 0 of dname    */
           } else {
              nopt++ ;                                     /* don't mangle */
           }
           dset = THD_open_dataset( fname ) ;              /* open it */
           if( dset == NULL )
              ERROR_exit("can't open dataset %s\n",fname) ;
         }

         /* set some parameters based on the dataset */

         ntime[ival] = DSET_NVALS(dset) ;
         if ( ids > 2 ) ntime[ival] = 1 ;
         ntime_max = MAX( ntime_max, ntime[ival] );

         nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
         if( CALC_nvox < 0 ){
            CALC_nvox = nxyz ;
         } else if( nxyz != CALC_nvox ){
            ERROR_exit("dataset %s differs in size from others\n",argv[nopt-1]);
         }

         if( !DSET_datum_constant(dset) ){   /* 29 May 2003 */
           float *far ;

           WARNING_message("dataset %s has sub-bricks with different types\n"
                           " +     ==> converting all sub-bricks to floats\n",
                          argv[nopt-1]);

           DSET_mallocize(dset) ; DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

           for( ii=0 ; ii < ntime[ival] ; ii++ ){
             if( DSET_BRICK_TYPE(dset,ii) != MRI_float ){
               far = calloc( sizeof(float) , nxyz ) ;
               if( far == NULL )
                 ERROR_exit("can't malloc space for conversion\n");
               EDIT_coerce_scale_type( nxyz , DSET_BRICK_FACTOR(dset,ii) ,
                                       DSET_BRICK_TYPE(dset,ii), DSET_ARRAY(dset,ii),
                                       MRI_float , far ) ;
               EDIT_substitute_brick( dset , ii , MRI_float , far ) ;
               DSET_BRICK_FACTOR(dset,ii) = 0.0f ;
             }
           }
         }

         CALC_type[ival] = DSET_BRICK_TYPE(dset,isub) ;
         CALC_dset[ival] = dset ;

         /* load floating scale factors */
         /* 14 Nov 2003: CALC_noffac[ival] signals there is no scale factor
                         (so can avoid the multiplication when loading values) */

         CALC_ffac[ival] = (float *) malloc( sizeof(float) * ntime[ival] ) ;
         if ( ntime[ival] == 1 ) {
           CALC_ffac[ival][0] = DSET_BRICK_FACTOR( dset , isub) ;
           if (CALC_ffac[ival][0] == 0.0 ) CALC_ffac[ival][0] = 1.0 ;
           if( CALC_ffac[ival][0] != 1.0 ) CALC_noffac[ival] = 0 ;  /* 14 Nov 2003 */
         } else {
            for (ii = 0 ; ii < ntime[ival] ; ii ++ ) {
              CALC_ffac[ival][ii] = DSET_BRICK_FACTOR(dset, ii) ;
              if (CALC_ffac[ival][ii] == 0.0 ) CALC_ffac[ival][ii] = 1.0;
              if( CALC_ffac[ival][ii] != 1.0 ) CALC_noffac[ival] = 0 ;  /* 14 Nov 2003 */
            }
         }

         /* read data from disk */

         if( CALC_verbose ){
           int iv , nb ;
           for( iv=nb=0 ; iv < DSET_NVALS(dset) ; iv++ )
             nb += DSET_BRICK_BYTES(dset,iv) ;
           INFO_message("Reading dataset %s (%d bytes)\n",argv[nopt-1],nb);
         }

         if( ! DSET_LOADED(dset) ){
           DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
         }

         /* set pointers for actual dataset arrays */

         CALC_cxcode[ival] = CUR_cxcode ; /* 10 Mar 2006 */

         switch (CALC_type[ival]) {
           case MRI_short:
             CALC_short[ival] = (short **) malloc( sizeof(short *) * ntime[ival] ) ;
             if (ntime[ival] == 1 )
               CALC_short[ival][0] = (short *) DSET_ARRAY(dset, isub) ;
             else
               for (ii=0; ii < ntime[ival]; ii++)
                 CALC_short[ival][ii] = (short *) DSET_ARRAY(dset, ii);
           break;

           case MRI_float:
             CALC_float[ival] = (float **) malloc( sizeof(float *) * ntime[ival] ) ;
             if (ntime[ival] == 1 )
               CALC_float[ival][0] = (float *) DSET_ARRAY(dset, isub) ;
             else
               for (ii=0; ii < ntime[ival]; ii++)
                 CALC_float[ival][ii] = (float *) DSET_ARRAY(dset, ii);
           break;

           case MRI_byte:
             CALC_byte[ival] = (byte **) malloc( sizeof(byte *) * ntime[ival] ) ;
             if (ntime[ival] == 1 )
               CALC_byte[ival][0] = (byte *) DSET_ARRAY(dset, isub) ;
             else
               for (ii=0; ii < ntime[ival]; ii++)
                 CALC_byte[ival][ii] = (byte *) DSET_ARRAY(dset, ii);
            break;

           case MRI_rgb:   /* 10 Feb 2003 */
             CALC_byte[ival] = (byte **) malloc( sizeof(byte *) * ntime[ival] ) ;
             if (ntime[ival] == 1 )
               CALC_byte[ival][0] = (byte *) DSET_ARRAY(dset, isub) ;
             else
               for (ii=0; ii < ntime[ival]; ii++)
                 CALC_byte[ival][ii] = (byte *) DSET_ARRAY(dset, ii);
           break ;

           case MRI_complex: /* 10 Mar 2006 */
             CALC_complex[ival] = (complex **)malloc( sizeof(complex *) * ntime[ival] );
             if( ntime[ival] == 1 )
               CALC_complex[ival][0] = (complex *) DSET_ARRAY(dset, isub) ;
             else
               for (ii=0; ii < ntime[ival]; ii++)
                 CALC_complex[ival][ii] = (complex *) DSET_ARRAY(dset, ii);
           break ;

           default:
             ERROR_exit("Dataset %s has illegal data type: %s\n" ,
                        argv[nopt-1] , MRI_type_name[CALC_type[ival]] ) ;

         } /* end of switch over type switch */

         /* if -datum not given or implied yet, set the output datum now */

         if( CALC_datum < 0 && CALC_type[ival] != MRI_rgb ){
           if( CALC_type[ival] == MRI_complex ) CALC_datum = MRI_float ;
           else                                 CALC_datum = CALC_type[ival] ;
         }

DSET_DONE: continue;  /*** target for various goto statements above ***/

      } /* end of dataset input */

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;

   }  /* end of loop over options */

   /*---------------------------------------*/
   /*** cleanup: check for various errors ***/

   if( nopt < argc )
     ERROR_exit(
      "Extra command line arguments puzzle me! argv[%d]=%s ...\n",nopt,argv[nopt]) ;

   if( CALC_gscale && CALC_usetemp )  /* 18 Oct 2005 */
     ERROR_exit("-gscale and -usetemp are incompatible!") ;

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   if( ids == 26 ){
     for( ids=0 ; ids < 26 ; ids++ ) if( TS_flim[ids] != NULL ) break ;
     if( ids < 26 )
       ERROR_exit("No actual input datasets given! "
                  "Use '1deval' for .1D file calculations.");
     else
       ERROR_exit("No actual input datasets given!") ;
   }

   /* 22 Feb 2005: check IJKAR inputs against 1st dataset found */

   for( ii=0 ; ii < 26 ; ii++ ){
     if( IJKAR_flim[ii] != NULL ){
       int siz=0 ;
       switch( IJKAR_dcod[ii] ){
         case  8: siz = DSET_NX(CALC_dset[ids]) ; break ;
         case  9: siz = DSET_NY(CALC_dset[ids]) ; break ;
         case 10: siz = DSET_NZ(CALC_dset[ids]) ; break ;
       }
       if( IJKAR_flim[ii]->nx != siz )
         WARNING_message("dimension mismatch between '-%c' and '%-c'\n",
                         'a'+ii , 'a'+ids ) ;
     }
   }

   if( CALC_code == NULL ) ERROR_exit("No expression given!\n") ;

   if( CALC_histpar >= 0 && CALC_dset[CALC_histpar] == NULL ){
     WARNING_message("-histpar dataset not defined!\n") ;
     CALC_histpar = -1 ;
   }

   for (ids=0; ids < 26; ids ++)
      if (ntime[ids] > 1 && ntime[ids] != ntime_max ) {
          ERROR_exit("Multi-brick datasets don't match!\n") ;
      }

   /* 17 Apr 1998: if all input datasets are 3D only (no time),
                   and if there are any input time series,
                   then the output must become 3D+time itself  */

   if( ntime_max == 1 && TS_nmax > 0 ){
      ntime_max = TS_nmax ;
      TS_make   = 1 ;        /* flag to force manufacture of a 3D+time dataset */
      INFO_message(
              "Calculating 3D+time[%d]"
              " dataset from 3D datasets and time series, with dt=%g s\n" ,
              ntime_max , TS_dt ) ;
   }

   if( CALC_taxis_num > 0 ){  /* 28 Apr 2003 */
     if( ntime_max > 1 ){
       WARNING_message("-taxis %d overriden by dataset input(s)\n",
                       CALC_taxis_num) ;
     } else {
       ntime_max = CALC_taxis_num ;
       TS_make   = 1 ;
       INFO_message("Calculating 3D+time[%d]"
                    " dataset from 3D datasets and -taxis with dt=%g s\n" ,
                    ntime_max , TS_dt ) ;
     }
   }

   /* 15 Apr 1999: check if each input dataset is used,
                   or if an undefined symbol is used.   */

   for (ids=0; ids < 26; ids ++){
      if( VAR_DEFINED(ids) && !CALC_has_sym[ids] )
         WARNING_message("input '%c' is not used in the expression\n" ,
                 abet[ids] ) ;

      else if( !VAR_DEFINED(ids) && CALC_has_sym[ids] ){

         if( ((1<<ids) & PREDEFINED_MASK) == 0 ){
            WARNING_message( "symbol %c is used but not defined\n" , abet[ids] ) ;
         } else {
            CALC_has_predefined++ ;
            INFO_message("Symbol %c using predefined value\n",abet[ids] ) ;
            if( ids >= 23 ) CALC_has_xyz = 1 ;
         }
      }
   }

   return ;
}

/*------------------------------------------------------------------*/

void CALC_Syntax(void)
{
   printf(
    "Program: 3dcalc                                                         \n"
    "Author:  RW Cox et al                                                   \n"
    "                                                                        \n"
    "3dcalc - AFNI's calculator program                                      \n"
    "                                                                        \n"
    "     This program does voxel-by-voxel arithmetic on 3D datasets         \n"
    "     (only limited inter-voxel computations are possible).              \n"
    "                                                                        \n"
    "     The program assumes that the voxel-by-voxel computations are being \n"
    "     performed on datasets that occupy the same space and have the same \n"
    "     orientations.                                                      \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "Usage:                                                                  \n"
    "-----                                                                   \n"
    "       3dcalc -a dsetA [-b dsetB...] \\                                 \n"
    "              -expr EXPRESSION       \\                                 \n"
    "              [options]                                                 \n"
    "                                                                        \n"
    "Examples:                                                               \n"
    "--------                                                                \n"
    "1. Average datasets together, on a voxel-by-voxel basis:                \n"
    "                                                                        \n"
    "     3dcalc -a fred+tlrc -b ethel+tlrc -c lucy+tlrc \\                  \n"
    "            -expr '(a+b+c)/3' -prefix subjects_mean                     \n"
    "                                                                        \n"
    "   Averaging datasets can also be done by programs 3dMean and 3dmerge.  \n"
    "   Use 3dTstat to averaging across sub-bricks in a single dataset.      \n"
    "                                                                        \n"
    "2. Perform arithmetic calculations between the sub-bricks of a single   \n"
    "   dataset by noting the sub-brick number on the command line:          \n"
    "                                                                        \n"
    "     3dcalc -a 'func+orig[2]' -b 'func+orig[4]' -expr 'sqrt(a*b)'       \n"
    "                                                                        \n"
    "3. Create a simple mask that consists only of values in sub-brick #0    \n"
    "   that are greater than 3.14159:                                       \n"
    "                                                                        \n"
    "     3dcalc -a 'func+orig[0]' -expr 'ispositive(a-3.14159)' \\          \n"
    "            -prefix mask                                                \n"
    "                                                                        \n"
    "4. Normalize subjects' time series datasets to percent change values in \n"
    "   preparation for group analysis:                                      \n"
    "                                                                        \n"
    "   Voxel-by-voxel, the example below divides each intensity value in    \n"
    "   the time series (epi_r1+orig) with the voxel's mean value (mean+orig)\n"
    "   to get a percent change value. The 'ispositive' command will ignore  \n"
    "   voxels with mean values less than 167 (i.e., they are labeled as     \n"
    "  'zero' in the output file 'percent_change+orig') and are most likely  \n"
    "   background/noncortical voxels.                                       \n"
    "                                                                        \n"
    "     3dcalc -a epi_run1+orig -b mean+orig     \\                        \n"
    "            -expr '100 * a/b * ispositive(b-167)' -prefix percent_chng  \n"
    "                                                                        \n"
    "5. Create a compound mask from a statistical dataset, where 3 stimuli   \n"
    "   show activation.                                                     \n"
    "      NOTE: 'step' and 'ispositive' are identical expressions that can  \n"
    "            be used interchangeably:                                    \n"
    "                                                                        \n"
    "     3dcalc -a 'func+orig[12]' -b 'func+orig[15]' -c 'func+orig[18]' \\ \n"
    "            -expr 'step(a-4.2)*step(b-2.9)*step(c-3.1)'              \\ \n"
    "            -prefix compound_mask                                       \n"
    "                                                                        \n"
    "   In this example, all 3 statistical criteria must be met at once for  \n"
    "   a voxel to be selected (value of 1) in this mask.                    \n"
    "                                                                        \n"
    "6. Same as example #5, but this time create a mask of 8 different values\n"
    "   showing all combinations of activations (i.e., not only where        \n"
    "   everything is active, but also each stimulus individually, and all   \n"
    "   combinations).  The output mask dataset labels voxel values as such: \n"
    "                                                                        \n"
    "        0 = none active    1 = A only active    2 = B only active       \n"
    "        3 = A and B only   4 = C only active    5 = A and C only        \n"
    "        6 = B and C only   7 = all A, B, and C active                   \n"
    "                                                                        \n"
    "     3dcalc -a 'func+orig[12]' -b 'func+orig[15]' -c 'func+orig[18]' \\ \n"
    "            -expr 'step(a-4.2)+2*step(b-2.9)+4*step(c-3.1)'          \\ \n"
    "            -prefix mask_8                                              \n"
    "                                                                        \n"
    "   In displaying such a binary-encoded mask in AFNI, you would probably \n"
    "   set the color display to have 8 discrete levels (the '#' menu).      \n"
    "                                                                        \n"
    "7. Create a region-of-interest mask comprised of a 3-dimensional sphere.\n"
    "   Values within the ROI sphere will be labeled as '1' while values     \n"
    "   outside the mask will be labeled as '0'. Statistical analyses can    \n"
    "   then be done on the voxels within the ROI sphere.                    \n"
    "                                                                        \n"
    "   The example below puts a solid ball (sphere) of radius 3=sqrt(9)     \n"
    "   about the point with coordinates (x,y,z)=(20,30,70):                 \n"
    "                                                                        \n"
    "     3dcalc -a anat+tlrc                                              \\\n"
    "            -expr 'step(9-(x-20)*(x-20)-(y-30)*(y-30)-(z-70)*(z-70))' \\\n"
    "            -prefix ball                                                \n"
    "                                                                        \n"
    "   The spatial meaning of (x,y,z) is discussed in the 'COORDINATES'     \n"
    "   section of this help listing (far below).                            \n"
    "                                                                        \n"
    "8. Some datsets are 'short' (16 bit) integers with a scalar attached,   \n"
    "   which allow them to be smaller than float datasets and to contain    \n"
    "   fractional values.                                                   \n"
    "                                                                        \n"
    "   Dataset 'a' is always used as a template for the output dataset. For \n"
    "   the examples below, assume that datasets d1+orig and d2+orig consist \n"
    "   of small integers.                                                   \n"
    "                                                                        \n"
    "   a) When dividing 'a' by 'b', the result should be scaled, so that a  \n"
    "      value of 2.4 is not truncated to '2'. To avoid this truncation,   \n"
    "      force scaling with the -fscale option:                            \n"
    "                                                                        \n"
    "        3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot -fscale   \n"
    "                                                                        \n"
    "   b) If it is preferable that the result is of type 'float', then set  \n"
    "      the output data type (datum) to float:                            \n"
    "                                                                        \n"
    "        3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot \\        \n"
    "                -datum float                                            \n"
    "                                                                        \n"
    "   c) Perhaps an integral division is desired, so that 9/4=2, not 2.24. \n"
    "      Force the results not to be scaled (opposite of example 8a) using \n"
    "      the -nscale option:                                               \n"
    "                                                                        \n"
    "        3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot -nscale   \n"
    "                                                                        \n"
    "9. Compare the left and right amygdala between the Talairach atlas,     \n"
    "   and the CA_N27_ML atlas.  The result will be 1 if TT only, 2 if CA   \n"
    "   only, and 3 where they overlap.                                      \n"
    "                                                                        \n"
    "     3dcalc -a 'TT_Daemon::amygdala' -b 'CA_N27_ML::amygdala' \\        \n"
    "            -expr 'step(a)+2*step(b)'  -prefix compare.maps             \n"
    "                                                                        \n"
    "   (see 'whereami -help' for more information on atlases)               \n"
    "                                                                        \n"
    "10. Convert a dataset from AFNI short format storage to NIfTI-1 floating\n"
    "    point (perhaps for input to an non-AFNI program that requires this):\n"
    "                                                                        \n"
    "      3dcalc -a zork+orig -prefix zfloat.nii -datum float -expr 'a'     \n"
    "                                                                        \n"
    "    This operation could also be performed with program 3dAFNItoNIFTI.  \n"
    "                                                                        \n"
    "11. Compute the edge voxels of a mask dataset.  An edge voxel is one    \n"
    "    that shares some face with a non-masked voxel.  This computation    \n"
    "    assumes 'a' is a binary mask (particularly for 'amongst').          \n"
    "                                                                        \n"
    "      3dcalc -a mask+orig -prefix edge                     \\           \n"
    "             -b a+i -c a-i -d a+j -e a-j -f a+k -g a-k     \\           \n"
    "             -expr 'a*amongst(0,b,c,d,e,f,g)'                           \n"
    "                                                                        \n"
    "    consider similar erosion or dilation operations:                    \n"
    "        erosion:  -expr 'a*(1-amongst(0,b,c,d,e,f,g))'                  \n"
    "        dilation: -expr 'amongst(1,a,b,c,d,e,f,g)'                      \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "ARGUMENTS for 3dcalc (must be included on command line):                \n"
    "---------                                                               \n"
    "                                                                        \n"
    " -a dname    = Read dataset 'dname' and call the voxel values 'a' in the\n"
    "               expression (-expr) that is input below. Up to 26 dnames  \n"
    "               (-a, -b, -c, ... -z) can be included in a single 3dcalc  \n"
    "               calculation/expression.                                  \n"
    "               ** If some letter name is used in the expression, but    \n"
    "                  not present in one of the dataset options here, then  \n"
    "                  that variable is set to 0.                            \n"
    "               ** If the letter is followed by a number, then that      \n"
    "                  number is used to select the sub-brick of the dataset \n"
    "                  which will be used in the calculations.               \n"
    "                     E.g., '-b3 dname' specifies that the variable 'b'  \n"
    "                     refers to sub-brick '3' of that dataset            \n"
    "                     (indexes in AFNI start at 0).                      \n"
    "               ** However, it is better to use the subscript '[]' method\n"
    "                  to select sub-bricks of datasets, as in               \n"
    "                     -b dname+orig'[3]'                                 \n"
    "                  rather than the older notation                        \n"
    "                     -b3 dname+orig                                     \n"
    "                  The subscript notation is more flexible, as it can    \n"
    "                  be used to select a collection of sub-bricks.         \n"
    "                                                                        \n"
    " -expr       = Apply the expression - within quotes - to the input      \n"
    "               datasets (dnames), one voxel at time, to produce the     \n"
    "               output dataset.                                          \n"
    "                                                                        \n"
    " NOTE: If you want to average or sum up a lot of datasets, programs     \n"
    "       3dTstat and/or 3dMean and/or 3dmerge are better suited for these \n"
    "       purposes.  A common request is to increase the number of input   \n"
    "       datasets beyond 26, but in almost all cases such users simply    \n"
    "       want to do simple addition!                                      \n"
    "                                                                        \n"
    " NOTE: If you want to include shell variables in the expression (or in  \n"
    "       the dataset sub-brick selection), then you should use double     \n"
    "       \"quotes\" and the '$' notation for the shell variables; this    \n"
    "       example uses csh notation to set the shell variable 'z':         \n"
    "                                                                        \n"
    "         set z = 3.5                                                    \n"
    "         3dcalc -a moose.nii -prefix goose.nii -expr \"a*$z\"           \n"
    "                                                                        \n"
    "       The shell will not expand variables inside single 'quotes',      \n"
    "       and 3dcalc's parser will not understand the '$' character.       \n"
    "                                                                        \n"
    " NOTE: You can use the ccalc program to play with the expression        \n"
    "       evaluator, in order to get a feel for how it works and           \n"
    "       what it accepts.                                                 \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
   ) ;
   printf(
    " OPTIONS for 3dcalc:                                                    \n"
    " -------                                                                \n"
    "                                                                        \n"
    "  -verbose   = Makes the program print out various information as it    \n"
    "               progresses.                                              \n"
    "                                                                        \n"
    "  -datum type= Coerce the output data to be stored as the given type,   \n"
    "               which may be byte, short, or float.                      \n"
    "               [default = datum of first input dataset]                 \n"
    "  -float }                                                              \n"
    "  -short }   = Alternative options to specify output data format.       \n"
    "  -byte  }                                                              \n"
    "                                                                        \n"
    "  -fscale    = Force scaling of the output to the maximum integer       \n"
    "               range. This only has effect if the output datum is byte  \n"
    "               or short (either forced or defaulted). This option is    \n"
    "               often necessary to eliminate unpleasant truncation       \n"
    "               artifacts.                                               \n"
    "                 [The default is to scale only if the computed values   \n"
    "                  seem to need it -- are all <= 1.0 or there is at      \n"
    "                  least one value beyond the integer upper limit.]      \n"
    "                                                                        \n"
    "                ** In earlier versions of 3dcalc, scaling (if used) was \n"
    "                   applied to all sub-bricks equally -- a common scale  \n"
    "                   factor was used.  This would cause trouble if the    \n"
    "                   values in different sub-bricks were in vastly        \n"
    "                   different scales. In this version, each sub-brick    \n"
    "                   gets its own scale factor. To override this behavior,\n"
    "                   use the '-gscale' option.                            \n"
    "                                                                        \n"
    "  -gscale    = Same as '-fscale', but also forces each output sub-brick \n"
    "               to get the same scaling factor.  This may be desirable   \n"
    "               for 3D+time datasets, for example.                       \n"
    "            ** N.B.: -usetemp and -gscale are incompatible!!            \n"
    "                                                                        \n"
    "  -nscale    = Don't do any scaling on output to byte or short datasets.\n"
    "               This may be especially useful when operating on mask     \n"
    "               datasets whose output values are only 0's and 1's.       \n"
    "               ** Another way to achieve the effect of '-b3' is described\n"
    "                  below in the dataset 'INPUT' specification section.   \n"
    "                                                                        \n"
    "  -prefix pname = Use 'pname' for the output dataset prefix name.       \n"
    "                  [default='calc']                                      \n"
    "                                                                        \n"
    "  -session dir  = Use 'dir' for the output dataset session directory.   \n"
    "                  [default='./'=current working directory]              \n"
    "                  You can also include the output directory in the      \n"
    "                  'pname' parameter to the -prefix option.              \n"
    "                                                                        \n"
    "  -usetemp      = With this option, a temporary file will be created to \n"
    "                  hold intermediate results.  This will make the program\n"
    "                  run slower, but can be useful when creating huge      \n"
    "                  datasets that won't all fit in memory at once.        \n"
    "                * The program prints out the name of the temporary      \n"
    "                  file; if 3dcalc crashes, you might have to delete     \n"
    "                  this file manually.                                   \n"
    "               ** N.B.: -usetemp and -gscale are incompatible!!         \n"
    "                                                                        \n"
    "  -dt tstep     = Use 'tstep' as the TR for \"manufactured\" 3D+time    \n"
    "    *OR*          datasets.                                             \n"
    "  -TR tstep     = If not given, defaults to 1 second.                   \n"
    "                                                                        \n"
    "  -taxis N      = If only 3D datasets are input (no 3D+time or .1D files),\n"
    "    *OR*          then normally only a 3D dataset is calculated.  With  \n"
    "  -taxis N:tstep: this option, you can force the creation of a time axis\n"
    "                  of length 'N', optionally using time step 'tstep'.  In\n"
    "                  such a case, you will probably want to use the pre-   \n"
    "                  defined time variables 't' and/or 'k' in your         \n"
    "                  expression, or each resulting sub-brick will be       \n"
    "                  identical. For example:                               \n"
    "                  '-taxis 121:0.1' will produce 121 points in time,     \n"
    "                  spaced with TR 0.1.                                   \n"
    "                                                                        \n"
    "            N.B.: You can also specify the TR using the -dt option.     \n"
    "            N.B.: You can specify 1D input datasets using the           \n"
    "                  '1D:n@val,n@val' notation to get a similar effect.    \n"
    "                  For example:                                          \n"
    "                     -dt 0.1 -w '1D:121@0'                              \n"
    "                  will have pretty much the same effect as              \n"
    "                     -taxis 121:0.1\n"
    "            N.B.: For both '-dt' and '-taxis', the 'tstep' value is in \n"
    "                  seconds.  You can suffix it with 'ms' to specify that\n"
    "                  the value is in milliseconds instead; e.g., '-dt 2000ms'.\n"
    "                                                                        \n"
    "  -rgbfac A B C = For RGB input datasets, the 3 channels (r,g,b) are    \n"
    "                  collapsed to one for the purposes of 3dcalc, using the\n"
    "                  formula value = A*r + B*g + C*b                       \n"
    "                                                                        \n"
    "                  The default values are A=0.299 B=0.587 C=0.114, which \n"
    "                  gives the grayscale intensity.  To pick out the Green \n"
    "                  channel only, use '-rgbfac 0 1 0', for example.  Note \n"
    "                  that each channel in an RGB dataset is a byte in the  \n"
    "                  range 0..255.  Thus, '-rgbfac 0.001173 0.002302 0.000447'\n"
    "                  will compute the intensity rescaled to the range 0..1.0\n"
    "                  (i.e., 0.001173=0.299/255, etc.)                      \n"
    "                                                                        \n"
    "  -cx2r METHOD  = For complex input datasets, the 2 channels must be    \n"
    "                  converted to 1 real number for calculation.  The      \n"
    "                  methods available are:  REAL  IMAG  ABS  PHASE        \n"
    "                * The default method is ABS = sqrt(REAL^2+IMAG^2)       \n"
    "                * PHASE = atan2(IMAG,REAL)                              \n"
    "                * Multiple '-cx2r' options can be given:                \n"
    "                    when a complex dataset is given on the command line,\n"
    "                    the most recent previous method will govern.        \n"
    "                * If a complex dataset is used in a differential        \n"
    "                    subscript, then the most recent previous -cx2r      \n"
    "                    method applies to the extraction; for example       \n"
    "                      -cx2r REAL -a cx+orig -cx2r IMAG -b 'a[0,0,0,0]'  \n"
    "                    means that variable 'a' refers to the real part     \n"
    "                    of the input dataset and variable 'b' to the        \n"
    "                    imaginary part of the input dataset.                \n"
    "                * 3dcalc cannot be used to CREATE a complex dataset!    \n"
    "                    [See program 3dTwotoComplex for that purpose.]      \n"
#ifdef ALLOW_SORT
    "                                                                        \n"
    "  -sort         = Sort each output brick separately, before output:     \n"
    "  -SORT           'sort' ==> increasing order, 'SORT' ==> decreasing.   \n"
    "                  [This is useful only under unusual circumstances!]    \n"
    "                  [Sorting is done in spatial indexes, not in time.]    \n"
    "                  [Program 3dTsort will sort voxels along time axis]    \n"
#endif
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "DATASET TYPES:                                                          \n"
    "-------------                                                           \n"
    "                                                                        \n"
    " The most common AFNI dataset types are 'byte', 'short', and 'float'.   \n"
    "                                                                        \n"
    " A byte value is an 8-bit signed integer (0..255), a short value ia a   \n"
    " 16-bit signed integer (-32768..32767), and a float value is a 32-bit   \n"
    " real number.  A byte value has almost 3 decimals of accuracy, a short  \n"
    " has almost 5, and a float has approximately 7 (from a 23+1 bit         \n"
    " mantissa).                                                             \n"
    "                                                                        \n"
    " Datasets can also have a scalar attached to each sub-brick. The main   \n"
    " use of this is allowing a short type dataset to take on non-integral   \n"
    " values, while being half the size of a float dataset.                  \n"
    "                                                                        \n"
    " As an example, consider a short dataset with a scalar of 0.0001. This  \n"
    " could represent values between -32.768 and +32.767, at a resolution of \n"
    " 0.001.  One could represnt the difference between 4.916 and 4.917, for \n"
    " instance, but not 4.9165. Each number has 15 bits of accuracy, plus a  \n"
    " sign bit, which gives 4-5 decimal places of accuracy. If this is not   \n"
    " enough, then it makes sense to use the larger type, float.             \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "3D+TIME DATASETS:                                                       \n"
    "----------------                                                        \n"
    "                                                                        \n"
    " This version of 3dcalc can operate on 3D+time datasets.  Each input    \n"
    " dataset will be in one of these conditions:                            \n"
    "                                                                        \n"
    "    (A) Is a regular 3D (no time) dataset; or                           \n"
    "    (B) Is a 3D+time dataset with a sub-brick index specified ('-b3'); or\n"
    "    (C) Is a 3D+time dataset with no sub-brick index specified ('-b').  \n"
    "                                                                        \n"
    " If there is at least one case (C) dataset, then the output dataset will\n"
    " also be 3D+time; otherwise it will be a 3D dataset with one sub-brick. \n"
    " When producing a 3D+time dataset, datasets in case (A) or (B) will be  \n"
    " treated as if the particular brick being used has the same value at each\n"
    " point in time.                                                         \n"
    "                                                                        \n"
    " Multi-brick 'bucket' datasets may also be used.  Note that if multi-brick\n"
    " (bucket or 3D+time) datasets are used, the lowest letter dataset will  \n"
    " serve as the template for the output; that is, '-b fred+tlrc' takes    \n"
    " precedence over '-c wilma+tlrc'.  (The program 3drefit can be used to  \n"
    " alter the .HEAD parameters of the output dataset, if desired.)         \n"

    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    MASTER_HELP_STRING
    "                                                                        \n"
    "** WARNING: you cannot combine sub-brick selection of the form          \n"
    "               -b3 bambam+orig       (the old method)                   \n"
    "            with sub-brick selection of the form                        \n"
    "               -b  'bambam+orig[3]'  (the new method)                   \n"
    "            If you try, the Doom of Mandos will fall upon you!          \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "1D TIME SERIES:                                                         \n"
    "--------------                                                          \n"
    "                                                                        \n"
    " You can also input a '*.1D' time series file in place of a dataset.    \n"
    " In this case, the value at each spatial voxel at time index n will be  \n"
    " the same, and will be the n-th value from the time series file.        \n"
    " At least one true dataset must be input.  If all the input datasets    \n"
    " are 3D (single sub-brick) or are single sub-bricks from multi-brick    \n"
    " datasets, then the output will be a 'manufactured' 3D+time dataset.    \n"
    "                                                                        \n"
    " For example, suppose that 'a3D+orig' is a 3D dataset:                  \n"
    "                                                                        \n"
    "   3dcalc -a a3D+orig -b b.1D -expr \"a*b\"                             \n"
    "                                                                        \n"
    " The output dataset will 3D+time with the value at (x,y,z,t) being      \n"
    " computed by a3D(x,y,z)*b(t).  The TR for this dataset will be set      \n"
    " to 'tstep' seconds -- this could be altered later with program 3drefit.\n"
    " Another method to set up the correct timing would be to input an       \n"
    " unused 3D+time dataset -- 3dcalc will then copy that dataset's time    \n"
    " information, but simply do not use that dataset's letter in -expr.     \n"
    "                                                                        \n"
    " If the *.1D file has multiple columns, only the first read will be     \n"
    " used in this program.  You can select a column to be the first by      \n"
    " using a sub-vector selection of the form 'b.1D[3]', which will         \n"
    " choose the 4th column (since counting starts at 0).                    \n"
    "                                                                        \n"
    " '{...}' row selectors can also be used - see the output of '1dcat -help'\n"
    " for more details on these.  Note that if multiple timeseries or 3D+time\n"
    " or 3D bucket datasets are input, they must all have the same number of \n"
    " points along the 'time' dimension.                                     \n"
    "                                                                        \n"
    " N.B.: To perform calculations ONLY on .1D files, use program 1deval.   \n"
    "       3dcalc takes .1D files for use in combination with 3D datasets!  \n"
    "                                                                        \n"
    " N.B.: If you auto-transpose a .1D file on the command line, (by ending \n"
    "       the filename with \\'), then 3dcalc will NOT treat it as the     \n"
    "       special case described above, but instead will treat it as       \n"
    "       a normal dataset, where each row in the transposed input is a    \n"
    "       'voxel' time series.  This would allow you to do differential    \n"
    "       subscripts on 1D time series, which program 1deval does not      \n"
    "       implement.  For example:                                         \n"
    "                                                                        \n"
    "        3dcalc -a '1D: 3 4 5 6'\\' -b a+l -expr 'sqrt(a+b)' -prefix -   \n"
    "                                                                        \n"
    "       This technique allows expression evaluation on multi-column      \n"
    "       .1D files, which 1deval also does not implement.  For example:   \n"
    "                                                                        \n"
    "        3dcalc -a '1D: 3 4 5 | 1 2 3'\\' -expr 'cbrt(a)' -prefix -      \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "'1D:' INPUT:                                                            \n"
    "-----------                                                             \n"
    "                                                                        \n"
    " You can input a 1D time series 'dataset' directly on the command line, \n"
    " without an external file.  The 'filename for such input takes the      \n"
    " general format                                                         \n"
    "                                                                        \n"
    "   '1D:n_1@val_1,n_2@val_2,n_3@val_3,...'                               \n"
    "                                                                        \n"
    " where each 'n_i' is an integer and each 'val_i' is a float.  For       \n"
    " example                                                                \n"
    "                                                                        \n"
    "    -a '1D:5@0,10@1,5@0,10@1,5@0'                                       \n"
    "                                                                        \n"
    " specifies that variable 'a' be assigned to a 1D time series of 35,     \n"
    " alternating in blocks between values 0 and value 1.                    \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "'I:*.1D' and 'J:*.1D' and 'K:*.1D' INPUT:                               \n"
    "----------------------------------------                                \n"
    "                                                                        \n"
    " You can input a 1D time series 'dataset' to be defined as spatially    \n"
    " dependent instead of time dependent using a syntax like:               \n"
    "                                                                        \n"
    "   -c I:fred.1D                                                         \n"
    "                                                                        \n"
    " This indicates that the n-th value from file fred.1D is to be associated\n"
    " with the spatial voxel index i=n (respectively j=n and k=n for 'J: and \n"
    " K: input dataset names).  This technique can be useful if you want to  \n"
    " scale each slice by a fixed constant; for example:                     \n"
    "                                                                        \n"
    "   -a dset+orig -b K:slicefactor.1D -expr 'a*b'                         \n"
    "                                                                        \n"
    " In this example, the '-b' value only varies in the k-index spatial     \n"
    " direction.                                                             \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "COORDINATES and PREDEFINED VALUES:                                      \n"
    "---------------------------------                                       \n"
    "                                                                        \n"
    " If you don't use '-x', '-y', or '-z' for a dataset, then the voxel     \n"
    " spatial coordinates will be loaded into those variables.  For example, \n"
    " the expression 'a*step(x*x+y*y+z*z-100)' will zero out all the voxels  \n"
    " inside a 10 mm radius of the origin x=y=z=0.                           \n"
    "                                                                        \n"
    " Similarly, the '-t' value, if not otherwise used by a dataset or *.1D  \n"
    " input, will be loaded with the voxel time coordinate, as determined    \n"
    " from the header file created for the OUTPUT.  Please note that the units\n"
    " of this are variable; they might be in milliseconds, seconds, or Hertz.\n"
    " In addition, slices of the dataset might be offset in time from one    \n"
    " another, and this is allowed for in the computation of 't'.  Use program\n"
    " 3dinfo to find out the structure of your datasets, if you are not sure.\n"
    " If no input datasets are 3D+time, then the effective value of TR is    \n"
    " tstep in the output dataset, with t=0 at the first sub-brick.          \n"
    "                                                                        \n"
    " Similarly, the '-i', '-j', and '-k' values, if not otherwise used,     \n"
    " will be loaded with the voxel spatial index coordinates.  The '-l'     \n"
    " (letter 'ell') value will be loaded with the temporal index coordinate.\n"
    "                                                                        \n"
    " Otherwise undefined letters will be set to zero.  In the future,       \n"
    " new default values for other letters may be added.                     \n"
    "                                                                        \n"
    " NOTE WELL: By default, the coordinate order of (x,y,z) is the order in \n"
    " *********  which the data array is stored on disk; this order is output\n"
    "            by 3dinfo.  The options below control can change this order:\n"
    "                                                                        \n"
    " -dicom }= Sets the coordinates to appear in DICOM standard (RAI) order,\n"
    " -RAI   }= (the AFNI standard), so that -x=Right, -y=Anterior , -z=Inferior,\n"
    "                                        +x=Left , +y=Posterior, +z=Superior.\n"
    "                                                                        \n"
    " -SPM   }= Sets the coordinates to appear in SPM (LPI) order,           \n"
    " -LPI   }=                      so that -x=Left , -y=Posterior, -z=Inferior,\n"
    "                                        +x=Right, +y=Anterior , +z=Superior.\n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "DIFFERENTIAL SUBSCRIPTS [22 Nov 1999]:                                  \n"
    "-----------------------                                                 \n"
    "                                                                        \n"
    " Normal calculations with 3dcalc are strictly on a per-voxel basis:\n"
    " there is no 'cross-talk' between spatial or temporal locations.\n"
    " The differential subscript feature allows you to specify variables\n"
    " that refer to different locations, relative to the base voxel.\n"
    " For example,\n"
    "   -a fred+orig -b 'a[1,0,0,0]' -c 'a[0,-1,0,0]' -d 'a[0,0,2,0]'\n"
    " means: symbol 'a' refers to a voxel in dataset fred+orig,\n"
    "        symbol 'b' refers to the following voxel in the x-direction,\n"
    "        symbol 'c' refers to the previous voxel in the y-direction\n"
    "        symbol 'd' refers to the 2nd following voxel in the z-direction\n"
    "\n"
    " To use this feature, you must define the base dataset (e.g., 'a')\n"
    " first.  Then the differentially subscripted symbols are defined\n"
    " using the base dataset symbol followed by 4 integer subscripts,\n"
    " which are the shifts in the x-, y-, z-, and t- (or sub-brick index)\n"
    " directions. For example,\n"
    "\n"
    "   -a fred+orig -b 'a[0,0,0,1]' -c 'a[0,0,0,-1]' -expr 'median(a,b,c)'\n"
    "\n"
    " will produce a temporal median smoothing of a 3D+time dataset (this\n"
    " can be done more efficiently with program 3dTsmooth).\n"
    "\n"
    " Note that the physical directions of the x-, y-, and z-axes depend\n"
    " on how the dataset was acquired or constructed.  See the output of\n"
    " program 3dinfo to determine what direction corresponds to what axis.\n"
    "\n"
    " For convenience, the following abbreviations may be used in place of\n"
    " some common subscript combinations:\n"
    "\n"
    "   [1,0,0,0] == +i    [-1, 0, 0, 0] == -i\n"
    "   [0,1,0,0] == +j    [ 0,-1, 0, 0] == -j\n"
    "   [0,0,1,0] == +k    [ 0, 0,-1, 0] == -k\n"
    "   [0,0,0,1] == +l    [ 0, 0, 0,-1] == -l\n"
    "\n"
    " The median smoothing example can thus be abbreviated as\n"
    "\n"
    "   -a fred+orig -b a+l -c a-l -expr 'median(a,b,c)'\n"
    "\n"
    " When a shift calls for a voxel that is outside of the dataset range,\n"
    " one of three things can happen:\n"
    "\n"
    "   STOP => shifting stops at the edge of the dataset\n"
    "   WRAP => shifting wraps back to the opposite edge of the dataset\n"
    "   ZERO => the voxel value is returned as zero\n"
    "\n"
    " Which one applies depends on the setting of the shifting mode at the\n"
    " time the symbol using differential subscripting is defined.  The mode\n"
    " is set by one of the switches '-dsSTOP', '-dsWRAP', or '-dsZERO'.  The\n"
    " default mode is STOP.  Suppose that a dataset has range 0..99 in the\n"
    " x-direction.  Then when voxel 101 is called for, the value returned is\n"
    "\n"
    "   STOP => value from voxel 99 [didn't shift past edge of dataset]\n"
    "   WRAP => value from voxel 1  [wrapped back through opposite edge]\n"
    "   ZERO => the number 0.0 \n"
    "\n"
    " You can set the shifting mode more than once - the most recent setting\n"
    " on the command line applies when a differential subscript symbol is\n"
    " encountered.\n"
    "\n"
    "N.B.: You can also use program 3dLocalstat to process data from a\n"
    "      spatial neighborhood of each voxel; for example, to compute\n"
    "      the maximum over a sphere of radius 9 mm placed around\n"
    "      each voxel:\n"
    "        3dLocalstat -nbhd 'SPHERE(9)' -stat max -prefix Amax9 A+orig\n"
    "\n"
    "------------------------------------------------------------------------\n"
    "ISSUES:\n"
    "------ \n"
    "\n"
    " * Complex-valued datasets cannot be processed, except via '-cx2r'.\n"
    " * This program is not very efficient (but is faster than it once was).\n"
    " * Differential subscripts slow the program down even more.\n"
    "\n"
    "------------------------------------------------------------------------\n"
   ) ;

   printf(
    "------------------------------------------------------------------------\n"
    "EXPRESSIONS:\n"
    "----------- \n"
    "\n"
    " As noted above, datasets are referred to by single letter variable names.\n"
    PARSER_HELP_STRING
    "\n"
    "** If you modify a statistical sub-brick, you may want to use program\n"
    "  '3drefit' to modify the dataset statistical auxiliary parameters.\n"
    "\n"
    "** Computations are carried out in double precision before being\n"
    "   truncated to the final output 'datum'.\n"
    "\n"
    "** Note that the quotes around the expression are needed so the shell\n"
    "   doesn't try to expand * characters, or interpret parentheses.\n"
    "\n"
    "** Try the 'ccalc' program to see how the expression evaluator works.\n"
    "   The arithmetic parser and evaluator is written in Fortran-77 and\n"
    "   is derived from a program written long ago by RW Cox to facilitate\n"
    "   compiling on an array processor hooked up to a VAX. (It's a mess, but\n"
    "   it works - somewhat slowly - but hey, computers are fast these days.)\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
#define VSIZE 1024

   double * atoz[26] ;
   int ii , ids , jj, kk, kt, ll, jbot, jtop ;
   THD_3dim_dataset * new_dset=NULL ;
   float ** buf;
   double   temp[VSIZE];
   int      nbad ;      /* 09 Aug 2000: check for bad results */

   THD_ivec3 iv ;
   THD_fvec3 fv ;
   float xxx[VSIZE], yyy[VSIZE], zzz[VSIZE] ;
   int   iii,jjj,kkk , nx,nxy ;
   THD_dataxes * daxes ;

   size_t tempnum , tempsiz ;

   /*** read input options ***/

   if( argc < 2 || strncasecmp(argv[1],"-help",4) == 0 ) CALC_Syntax() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dcalc main"); machdep() ;
   PRINT_VERSION("3dcalc") ; AUTHOR("A cast of thousands") ;
   THD_check_AFNI_version("3dcalc") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dcalc",argc,argv) ;

   for (ii=0; ii<26; ii++) ntime[ii] = 0 ;

   if( AFNI_yesenv("AFNI_FLOATIZE") ) CALC_datum = MRI_float ;

   CALC_read_opts( argc , argv ) ;

   /*** make output dataset ***/

   if( ntime_max == 1 || TS_make == 1 ){
      for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   } else {
      for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL &&
                                          ntime[ids] > 1           ) break ;
   }
   if( ids == 26 ) ERROR_exit("Can't find template dataset?!\n") ;

   new_dset = EDIT_empty_copy( CALC_dset[ids] ) ;

   /* 23 May 2005: check input datasets for axis consistency */

   for( iii=0 ; iii < 26 ; iii++ ){
     if( iii            != ids                                &&
         CALC_dset[iii] != NULL                               &&
         !EQUIV_DATAXES(new_dset->daxes,CALC_dset[iii]->daxes)  )
       WARNING_message("dataset '%c'=%s grid mismatch with %s\n",
                      'a'+iii , DSET_BRIKNAME(CALC_dset[iii]) ,
                                DSET_BRIKNAME(CALC_dset[ids]) ) ;
   }

   /** make history for new dataset */

   if( CALC_histpar < 0 ){
     for( iii=jjj=0 ; iii < 26 ; iii++ )       /* count number of input datasets */
       if( CALC_dset[iii] != NULL ) jjj++ ;
   } else {
     ids = CALC_histpar ;
     jjj = 1 ;
   }

   if( jjj == 1 || AFNI_yesenv("AFNI_SIMPLE_HISTORY") ){
      tross_Copy_History( CALC_dset[ids] , new_dset ) ;
   } else {                                               /* 27 Feb 2003 */
      char hbuf[64] ;
      tross_Append_History( new_dset ,
                            "===================================" ) ;
      tross_Append_History( new_dset ,
                            "=== History of inputs to 3dcalc ===" ) ;
      for( iii=0 ; iii < 26 ; iii++ ){
        if( CALC_dset[iii] != NULL ){
          sprintf(hbuf,"=== Input %c:", 'a'+iii ) ;
          tross_Append_History( new_dset , hbuf ) ;
          tross_Addto_History( CALC_dset[iii] , new_dset ) ;
        }
      }
      tross_Append_History( new_dset ,
                            "===================================" ) ;
   }
   tross_Make_History( "3dcalc" , argc,argv , new_dset ) ;

   if( CALC_datum < 0 ) CALC_datum = MRI_float ;  /* 10 Feb 2003 */

   EDIT_dset_items( new_dset ,
                       ADN_prefix         , CALC_output_prefix ,
                       ADN_directory_name , CALC_session ,
                       ADN_datum_all      , CALC_datum ,
                    ADN_none ) ;

   if( DSET_NVALS(new_dset) != ntime_max )
      EDIT_dset_items( new_dset , ADN_nvals , ntime_max , ADN_none ) ;

   /* 17 Apr 1998: if we are making up a 3D+time dataset,
                   we need to attach some time axis info to it */

   if( TS_make ){
      EDIT_dset_items( new_dset ,
                          ADN_ntt    , ntime_max      ,
                          ADN_ttdel  , TS_dt          ,
                          ADN_ttorg  , 0.0            ,
                          ADN_ttdur  , 0.0            ,
                          ADN_tunits , UNITS_SEC_TYPE ,
                       ADN_none ) ;
   }

   if( ISFUNC(new_dset) && ! ISFUNCBUCKET(new_dset) && new_dset->taxis != NULL )
      EDIT_dset_items( new_dset , ADN_func_type , FUNC_FIM_TYPE , ADN_none ) ;
   else if( ISANATBUCKET(new_dset) ) /* 30 Nov 1997 */
      EDIT_dset_items( new_dset , ADN_func_type , ANAT_EPI_TYPE , ADN_none ) ;

   if( THD_deathcon() && THD_is_file(new_dset->dblk->diskptr->header_name) )
     ERROR_exit("Output file %s already exists -- cannot continue!\n",
                new_dset->dblk->diskptr->header_name ) ;

   for (ids=0; ids<26; ids++)
     atoz[ids] = (double *) malloc(sizeof(double) * VSIZE ) ;

   for( ids=0 ; ids < 26 ; ids++ )  /* initialize to all zeros */
     for (ii=0; ii<VSIZE; ii++)
       atoz[ids][ii] = 0.0 ;

   /*** loop over time steps ***/

   nx  =      DSET_NX(new_dset) ;
   nxy = nx * DSET_NY(new_dset) ; daxes = new_dset->daxes ;

   buf = (float **) malloc(sizeof(float *) * ntime_max);

   if( CALC_usetemp ){                  /* 18 Oct 2005: -usetemp? */
     tempfnam    = UNIQ_idcode() ;
     tempfnam[0] = 'C'; tempfnam[1] = 'A';
     tempfnam[2] = 'L'; tempfnam[3] = 'C'; tempfnam[4] = '_' ;
     tempfile    = fopen( tempfnam , "w+b" ) ;
     INFO_message("Creating -usetemp file %s",tempfnam) ;
     atexit(calc_atexit) ;
   }
   tempsiz = ((size_t)CALC_nvox) * sizeof(float) ;

   for( kt=0 ; kt < ntime_max ; kt++ ){

      if( CALC_verbose )
        INFO_message("Computing sub-brick %d\n",kt) ;

      /* 30 April 1998: only malloc output space as it is needed */

      buf[kt] = (float *)calloc(1,tempsiz);
      if( buf[kt] == NULL )
        ERROR_exit("Can't malloc output dataset sub-brick %d!\n",kt) ;

      /*** loop over voxels ***/

      for ( ii = 0 ; ii < CALC_nvox ; ii += VSIZE ) {

          jbot = ii ;
          jtop = MIN( ii + VSIZE , CALC_nvox ) ;

          /* load (x,y,z) coords of these voxels into arrays, if needed */

          if( CALC_has_xyz ){                       /* 17 May 2005 */
            for( jj=jbot ; jj < jtop ; jj++ ){
              LOAD_IVEC3( iv , jj%nx , (jj%nxy)/nx , jj/nxy ) ;
              fv = THD_3dind_to_3dmm( new_dset , iv ) ;
              if( CALC_mangle_xyz )
                fv = THD_3dmm_to_dicomm(new_dset,fv) ;
              UNLOAD_FVEC3(fv,xxx[jj-jbot],yyy[jj-jbot],zzz[jj-jbot]) ;
              if( CALC_mangle_xyz == MANGLE_LPI ){
                xxx[jj-jbot] = -xxx[jj-jbot] ; yyy[jj-jbot] = -yyy[jj-jbot] ;
              }
            }
          }

          /* loop over datasets or other symbol definitions */

          for (ids = 0 ; ids < 26 ; ids ++ ) {  /* the whole alphabet */

            /* 17 Apr 1998: if a time series is used here instead of a dataset,
                            just copy the single value (or zero) to all voxels. */

            if( TS_flim[ids] != NULL ){
              if( jbot == 0 ){  /* only must do this on first vector at each time */
                double tval ;
                if( kt < TS_flim[ids]->nx ) tval = TS_flar[ids][kt] ;
                else                        tval = 0.0 ;

                for (jj =jbot ; jj < jtop ; jj ++ )
                  atoz[ids][jj-ii] = tval ;
              }
            }

            /** 22 Feb 2005: IJKAR 1D arrays **/

            else if( IJKAR_flim[ids] != NULL ){
              int ss , ix=IJKAR_flim[ids]->nx ;

              switch( IJKAR_dcod[ids] ){
                case 8:
                  for( jj=jbot ; jj < jtop ; jj++ ){
                    ss = (jj%nx) ;
                    atoz[ids][jj-jbot] = (ss < ix) ? IJKAR_flar[ids][ss] : 0.0 ;
                  }
                break ;
                case 9:
                  for( jj=jbot ; jj < jtop ; jj++ ){
                    ss = ((jj%nxy)/nx) ;
                    atoz[ids][jj-jbot] = (ss < ix) ? IJKAR_flar[ids][ss] : 0.0 ;
                  }
                break ;
                case 10:
                  for( jj=jbot ; jj < jtop ; jj++ ){
                    ss = (jj/nxy) ;
                    atoz[ids][jj-jbot] = (ss < ix) ? IJKAR_flar[ids][ss] : 0.0 ;
                  }
                break ;
              }
            }

            /** 22 Nov 1999: if a differentially subscripted dataset is here **/

            else if( CALC_dshift[ids] >= 0 ){
               int jds = CALC_dshift[ids] ;     /* actual dataset index */
               int kts , jjs , ix,jy,kz ;
               int id=CALC_dshift_i[ids] , jd=CALC_dshift_j[ids] ,
                   kd=CALC_dshift_k[ids] , ld=CALC_dshift_l[ids] ;
               int ijkd = ((id!=0) || (jd!=0) || (kd!=0)) ;
               int dsx = DSET_NX(CALC_dset[jds]) - 1 ;
               int dsy = DSET_NY(CALC_dset[jds]) - 1 ;
               int dsz = DSET_NZ(CALC_dset[jds]) - 1 ;
               int dst = ntime[jds]              - 1 ;
               int mode = CALC_dshift_mode[ids] , dun=0 ;

               kts = kt + ld ;                        /* t shift */
               if( kts < 0 || kts > dst ){
                 switch( mode ){
                   case DSHIFT_MODE_ZERO:
                     for( jj=jbot ; jj < jtop ; jj++ ) atoz[ids][jj-ii] = 0.0 ;
                     dun = 1 ;
                   break ;
                   default:
                   case DSHIFT_MODE_STOP:
                          if( kts <  0  ) kts = 0   ;
                     else if( kts > dst ) kts = dst ;
                   break ;
                   case DSHIFT_MODE_WRAP:
                     while( kts <  0  ) kts += (dst+1) ;
                     while( kts > dst ) kts -= (dst+1) ;
                   break ;
                 }
               }

               if( !dun ){   /* must get some actual data */
                 for( dun=0,jj=jbot ; jj < jtop ; jj++ ){ /* loop over voxels */
                   jjs = jj ;                  /* nominal voxel spatial index */
                   if( ijkd ){                 /* if spatial shift is ordered */
                     ix = DSET_index_to_ix(CALC_dset[jds],jj) ;
                     jy = DSET_index_to_jy(CALC_dset[jds],jj) ;
                     kz = DSET_index_to_kz(CALC_dset[jds],jj) ;

                     ix += id ;                  /* x shift */
                     if( ix < 0 || ix > dsx ){
                       switch( mode ){
                         case DSHIFT_MODE_ZERO:
                           atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                         break ;
                         default:
                         case DSHIFT_MODE_STOP:
                              if( ix <  0  ) ix = 0   ;
                           else if( ix > dsx ) ix = dsx ;
                         break ;
                         case DSHIFT_MODE_WRAP:
                           while( ix <  0  ) ix += (dsx+1) ;
                           while( ix > dsx ) ix -= (dsx+1) ;
                         break ;
                       }
                     }
                     if( dun ){ dun=0; continue; } /* go to next jj */

                     jy += jd ;                  /* y shift */
                     if( jy < 0 || jy > dsy ){
                       switch( mode ){
                         case DSHIFT_MODE_ZERO:
                           atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                         break ;
                         default:
                         case DSHIFT_MODE_STOP:
                                if( jy <  0  ) jy = 0   ;
                           else if( jy > dsy ) jy = dsy ;
                         break ;
                         case DSHIFT_MODE_WRAP:
                           while( jy <  0  ) jy += (dsy+1) ;
                           while( jy > dsy ) jy -= (dsy+1) ;
                         break ;
                       }
                     }
                     if( dun ){ dun=0; continue; } /* go to next jj */

                     kz += kd ;                  /* z shift */
                     if( kz < 0 || kz > dsz ){
                       switch( mode ){
                         case DSHIFT_MODE_ZERO:
                           atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                         break ;
                         default:
                         case DSHIFT_MODE_STOP:
                                if( kz <  0  ) kz = 0   ;
                           else if( kz > dsz ) kz = dsz ;
                         break ;
                         case DSHIFT_MODE_WRAP:
                           while( kz <  0  ) kz += (dsz+1) ;
                           while( kz > dsz ) kz -= (dsz+1) ;
                         break ;
                       }
                     }
                     if( dun ){ dun=0; continue; } /* go to next jj */

                     jjs = DSET_ixyz_to_index(CALC_dset[jds],ix,jy,kz) ;
                   } /* end of spatial shift index calculation */

                  switch( CALC_type[jds] ) {  /* extract data */
                    case MRI_short:
                      atoz[ids][jj-ii] =  CALC_short[jds][kts][jjs]
                                        * CALC_ffac[jds][kts];
                    break ;
                    case MRI_float:
                      atoz[ids][jj-ii] =  CALC_float[jds][kts][jjs]
                                        * CALC_ffac[jds][kts];
                    break ;
                    case MRI_byte:
                      atoz[ids][jj-ii] =  CALC_byte[jds][kts][jjs]
                                        * CALC_ffac[jds][kts];
                    break ;
                    case MRI_rgb:
                      atoz[ids][jj-ii] = Rfac*CALC_byte[jds][kts][3*jjs  ]
                                        +Gfac*CALC_byte[jds][kts][3*jjs+1]
                                        +Bfac*CALC_byte[jds][kts][3*jjs+2] ;
                    break ;
                    case MRI_complex:{                        /* 10 Mar 2006 */
                      complex cv=CALC_complex[jds][kts][jjs] ;
                      float   xx=cv.r, yy=cv.i , vv ;
                      switch( CALC_cxcode[ids] ){           /* ids, NOT jds! */
                        case CX_REALPART:  vv = xx ;                    break ;
                        case CX_IMAGPART:  vv = yy ;                    break ;
                        case CX_PHASE:     vv = (xx==0.0f && yy==0.0f)
                                                ? 0.0f : atan2(yy,xx) ; break ;
                        default:
                        case CX_MAGNITUDE: vv = sqrt(xx*xx+yy*yy) ;     break ;
                      }
                      atoz[ids][jj-ii] = vv ;
                    }
                  } /* end of data extraction switch */
                } /* end of loop over voxels */
              } /* end of getting actual data */
            } /* end of differential subscripted input */

            /** the case of a 3D dataset (i.e., only 1 sub-brick) **/

            else if ( ntime[ids] == 1 && CALC_type[ids] >= 0 ) {
               switch( CALC_type[ids] ) {
                  case MRI_short:
                     if( CALC_noffac[ids] )                      /* 14 Nov 2003 */
                       for (jj =jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_short[ids][0][jj] ;
                     else
                       for (jj =jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_short[ids][0][jj] * CALC_ffac[ids][0] ;
                  break;

                  case MRI_float:
                     if( CALC_noffac[ids] )                      /* 14 Nov 2003 */
                       for (jj =jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_float[ids][0][jj] ;
                     else
                       for (jj =jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_float[ids][0][jj] * CALC_ffac[ids][0] ;
                  break;

                  case MRI_byte:
                     if( CALC_noffac[ids] )                      /* 14 Nov 2003 */
                       for (jj =jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_byte[ids][0][jj] ;
                     else
                       for (jj =jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_byte[ids][0][jj] * CALC_ffac[ids][0] ;
                  break;

                  case MRI_rgb:
                     for (jj =jbot ; jj < jtop ; jj ++ )
                        atoz[ids][jj-ii] = Rfac*CALC_byte[ids][0][3*jj  ]
                                          +Gfac*CALC_byte[ids][0][3*jj+1]
                                          +Bfac*CALC_byte[ids][0][3*jj+2] ;
                  break;

                  case MRI_complex:{                          /* 10 Mar 2006 */
                    complex cv ; float xx,yy,vv ;
                    for( jj=jbot ; jj < jtop ; jj++ ){
                      cv=CALC_complex[ids][0][jj] ; xx = cv.r ; yy = cv.i ;
                      switch( CALC_cxcode[ids] ){
                        case CX_REALPART:  vv = xx ;                    break ;
                        case CX_IMAGPART:  vv = yy ;                    break ;
                        case CX_PHASE:     vv = (xx==0.0f && yy==0.0f)
                                                ? 0.0f : atan2(yy,xx) ; break ;
                        default:
                        case CX_MAGNITUDE: vv = sqrt(xx*xx+yy*yy) ;     break ;
                      }
                      atoz[ids][jj-ii] = vv ;
                    }
                  }
               }
            } /** end of 3D dataset **/

            /** the case of a 3D+time dataset (or a bucket, etc.) **/

            else if( ntime[ids] > 1 && CALC_type[ids] >= 0 ) {
               switch ( CALC_type[ids] ) {
                  case MRI_short:
                    if( CALC_noffac[ids] )
                      for (jj = jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_short[ids][kt][jj] ;
                    else
                      for (jj = jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_short[ids][kt][jj] * CALC_ffac[ids][kt];
                   break;

                 case MRI_float:
                    if( CALC_noffac[ids] )
                      for (jj = jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_float[ids][kt][jj] ;
                    else
                      for (jj = jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_float[ids][kt][jj] * CALC_ffac[ids][kt];
                 break;

                 case MRI_byte:
                    if( CALC_noffac[ids] )
                      for (jj = jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_byte[ids][kt][jj] ;
                    else
                      for (jj = jbot ; jj < jtop ; jj ++ )
                         atoz[ids][jj-ii] = CALC_byte[ids][kt][jj] * CALC_ffac[ids][kt];
                 break;

                 case MRI_rgb:
                    for (jj =jbot ; jj < jtop ; jj ++ )
                       atoz[ids][jj-ii] = Rfac*CALC_byte[ids][kt][3*jj  ]
                                         +Gfac*CALC_byte[ids][kt][3*jj+1]
                                         +Bfac*CALC_byte[ids][kt][3*jj+2] ;
                 break;

                 case MRI_complex:{                          /* 10 Mar 2006 */
                   complex cv ; float xx,yy,vv ;
                   for( jj=jbot ; jj < jtop ; jj++ ){
                     cv=CALC_complex[ids][kt][jj] ; xx = cv.r ; yy = cv.i ;
                     switch( CALC_cxcode[ids] ){
                       case CX_REALPART:  vv = xx ;                    break ;
                       case CX_IMAGPART:  vv = yy ;                    break ;
                       case CX_PHASE:     vv = (xx==0.0f && yy==0.0f)
                                               ? 0.0f : atan2(yy,xx) ; break ;
                       default:
                       case CX_MAGNITUDE: vv = sqrt(xx*xx+yy*yy) ;     break ;
                     }
                     atoz[ids][jj-ii] = vv ;
                   }
                 }
               }
             }

           /* the case of a voxel (x,y,z) or (i,j,k) coordinate */

           else if( CALC_has_predefined ) {

              switch( ids ){
                 case 23:     /* x */
                   if( HAS_X )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = xxx[jj-ii] ;
                 break ;

                 case 24:     /* y */
                   if( HAS_Y )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = yyy[jj-ii] ;
                 break ;

                 case 25:     /* z */
                   if( HAS_Z )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = zzz[jj-ii] ;
                 break ;

                 case 8:     /* i */
                   if( HAS_I )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = (jj%nx) ;
                 break ;

                 case 9:     /* j */
                   if( HAS_J )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = ((jj%nxy)/nx) ;
                 break ;

                 case 10:    /* k */
                   if( HAS_K )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = (jj/nxy) ;
                 break ;

                 case 19:    /* t */
                   if( HAS_T )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = THD_timeof_vox(kt,jj,new_dset) ;
                 break ;

                 case 11:    /* l */
                   if( HAS_L )
                     for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = kt ;
                 break ;
               } /* end of switch on symbol subscript */

              } /* end of choice over data type (if-else cascade) */
             } /* end of loop over datasets/symbols */

            /**** actually do the calculation work! ****/

            PARSER_evaluate_vector(CALC_code, atoz, jtop-jbot, temp);
            for ( jj = jbot ; jj < jtop ; jj ++ )
              buf[kt][jj] = temp[jj-ii];

         } /*---------- end of loop over space (voxels) ----------*/

         /* 09 Aug 2000: check results for validity */

         nbad = thd_floatscan( CALC_nvox , buf[kt] ) ;
         if( nbad > 0 )
           WARNING_message("%d bad floats replaced by 0 in sub-brick %d\n\a",
                           nbad , kt ) ;

         /* 30 April 1998: purge 3D+time sub-bricks if possible */

         if( ! CALC_has_timeshift ){
            for( ids=0 ; ids < 26 ; ids ++ ){
              if( CALC_dset[ids] != NULL && ntime[ids] > 1 &&
                  CALC_dset[ids]->dblk->malloc_type == DATABLOCK_MEM_MALLOC ){

                 void * ptr = DSET_ARRAY(CALC_dset[ids],kt) ;
                 if( ptr != NULL ) free(ptr) ;
                 mri_clear_data_pointer( DSET_BRICK(CALC_dset[ids],kt) ) ;
              }
           }
         }

         /* 18 Oct 2005: write to a temp file? */

         if( tempfile != NULL ){
           tempnum = fwrite( buf[kt] , 1 , tempsiz , tempfile ) ;
           free( buf[kt] ) ; buf[kt] = NULL ;
           if( tempnum < tempsiz ){
             ERROR_message("-usetemp #%d: only %u bytes written, out of %u\n",
                           kt , (unsigned)tempnum , (unsigned)tempsiz ) ;
             perror("** Unix error message") ;
           } else if( CALC_verbose )
             INFO_message("-usetemp #%d output %u bytes",kt,(unsigned)tempsiz) ;
         }

   } /*-------------- end of loop over time steps -------------*/

   for( ids=0 ; ids < 26 ; ids++ ){
     if( CALC_dset[ids] != NULL ) PURGE_DSET( CALC_dset[ids] ) ;
     if( TS_flim[ids]   != NULL ) mri_free( TS_flim[ids] ) ;
     if( IJKAR_flim[ids]!= NULL ) mri_free( IJKAR_flim[ids] ) ;
   }

   /*** attach new data to output brick ***/

   if( tempfile != NULL ) rewind(tempfile) ;   /* 18 Oct 2005 */

#undef  TGET
#define TGET(q)                                                      \
  if( tempfile != NULL ){                                            \
    buf[q] = (float *)calloc(1,tempsiz) ;                            \
    tempnum = fread( buf[q] , 1 , tempsiz , tempfile ) ;             \
    if( tempnum < tempsiz ){                                         \
      ERROR_message("-usetemp #%d: only %u bytes read, out of %u\n", \
                    (q) , (unsigned)tempnum , (unsigned)tempsiz ) ;  \
      perror("** Unix error message") ;                              \
    }                                                                \
  }

   switch( CALC_datum ){

      default:
        ERROR_exit("Somehow ended up with CALC_datum = %d\n",CALC_datum) ;
      exit(1) ;

      /* the easy case! */

      case MRI_float:{
        for( ii=0 ; ii < ntime_max ; ii++ ){
          TGET(ii) ;                  /* 18 Oct 2005: load from temp file? */

#ifdef ALLOW_SORT
               if( CALC_sort > 0 ) qsort_float    ( CALC_nvox, buf[ii] ) ;
          else if( CALC_sort < 0 ) qsort_float_rev( CALC_nvox, buf[ii] ) ;
#endif

#ifdef ALLOW_FDR  /* only experimental, not useful -- cf. 3dFDR instead */
          if( CALC_fdrize && DSET_BRICK_STATCODE(new_dset,ii) > 0 ){
            MRI_IMAGE *qim=mri_new_vol_empty(CALC_nvox,1,1,MRI_float) ;
            mri_fix_data_pointer( buf[ii] , qim ) ;
            mri_fdrize(qim,DSET_BRICK_STATCODE(new_dset,ii),
                           DSET_BRICK_STATAUX (new_dset,ii) , CALC_fdrize==2 ) ;
            mri_clear_data_pointer(qim); mri_free(qim);
            if( !ISFUNCBUCKET(new_dset) )
              EDIT_dset_items( new_dset ,
                                 ADN_type     , HEAD_FUNC_TYPE,
                                 ADN_func_type, FUNC_BUCK_TYPE, ADN_none ) ;
            EDIT_BRICK_TO_FIZT(new_dset,ii) ;
          }
#endif

          EDIT_substitute_brick(new_dset, ii, MRI_float, buf[ii]);
          DSET_BRICK_FACTOR(new_dset, ii) = 0.0;
        }
      }
      break ;

      /* the harder cases */

      case MRI_byte:         /* modified 31 Mar 1999 to scale each sub-brick  */
      case MRI_short:{       /* with its own factor, rather than use the same */
         void **dfim ;       /* factor for each sub-brick -- RWCox            */
         float gtop=0.0f , fimfac , gtemp ;

         if( CALC_verbose )
           INFO_message("Scaling output to type %s brick(s)\n",
                        MRI_TYPE_name[CALC_datum] ) ;

         dfim = (void **) malloc( sizeof(void *) * ntime_max ) ;

         if( CALC_gscale ){   /* 01 Apr 1999: global scaling */
           gtop = 0.0 ;
           for( ii=0 ; ii < ntime_max ; ii++ ){
             gtemp = MCW_vol_amax( CALC_nvox , 1 , 1 , MRI_float, buf[ii] ) ;
             gtop  = MAX( gtop , gtemp ) ;
             if( gtemp == 0.0 )
               WARNING_message("output sub-brick %d is all zeros!\n",ii) ;
           }
         }

         for( ii=0 ; ii < ntime_max ; ii++ ) {

           TGET(ii) ;   /* 18 Oct 2005: temp load */

#ifdef ALLOW_SORT
               if( CALC_sort > 0 ) qsort_float    ( CALC_nvox, buf[ii] ) ;
          else if( CALC_sort < 0 ) qsort_float_rev( CALC_nvox, buf[ii] ) ;
#endif

           /* get max of this sub-brick, if not doing global scaling */

           if( ! CALC_gscale ){
             gtop = MCW_vol_amax( CALC_nvox , 1 , 1 , MRI_float, buf[ii] ) ;
             if( gtop == 0.0 )
               WARNING_message("output sub-brick %d is all zeros!\n",ii) ;
           }

           /* compute scaling factor for this brick into fimfac */

           if( CALC_fscale ){                    /* 16 Mar 1998: forcibly scale */
             fimfac = (gtop > 0.0) ? MRI_TYPE_maxval[CALC_datum] / gtop : 0.0 ;

           } else if( !CALC_nscale ){            /* maybe scale */

             /* (gtop <= 1.0) to (gtop < 1.0)     23 Mar 2006 [rickr/dglen] */
             fimfac = (gtop > MRI_TYPE_maxval[CALC_datum]
                        || (gtop > 0.0 && gtop < 1.0) )
                      ? MRI_TYPE_maxval[CALC_datum]/ gtop : 0.0 ;

             if( fimfac == 0.0 && gtop > 0.0 ){  /* 28 Jul 2003: check for non-integers */
               float fv,iv ; int kk ;
               for( kk=0 ; kk < CALC_nvox ; kk++ ){
                 fv = buf[ii][kk] ; iv = rint(fv) ;
                 if( fabs(fv-iv) >= 0.01 ){
                   fimfac = MRI_TYPE_maxval[CALC_datum]/ gtop ; break ;
                 }
               }
             }

           } else {                              /* user says "don't scale" */
             fimfac = 0.0 ;
           }

           if( CALC_verbose ){
             if( fimfac != 0.0 )
               INFO_message("Sub-brick %d scale factor = %f\n",ii,fimfac) ;
             else
               INFO_message("Sub-brick %d: no scale factor\n" ,ii) ;
           }

           /* make space for output brick and scale into it */

           dfim[ii] = (void *) malloc( mri_datum_size(CALC_datum) * CALC_nvox ) ;
           if( dfim[ii] == NULL ) ERROR_exit("malloc fails at output[%d]\n",ii);

           if( CALC_datum == MRI_byte ){  /* 29 Nov 2004: check for bad byte-ization */
             int nneg ;
             for( nneg=jj=0 ; jj < CALC_nvox ; jj++ ) nneg += (buf[ii][jj] < 0.0f) ;
             if( nneg > 0 )
               WARNING_message(
                "sub-brick #%d has %d negative values set=0 in conversion to bytes\n",
                ii , nneg ) ;
           }

           EDIT_coerce_scale_type( CALC_nvox , fimfac ,
                                   MRI_float, buf[ii] , CALC_datum,dfim[ii] ) ;

           if( CALC_datum == MRI_short )
             EDIT_misfit_report( DSET_FILECODE(new_dset) , ii ,
                                 CALC_nvox , (fimfac != 0.0f) ? 1.0f/fimfac : 0.0f ,
                                 dfim[ii] , buf[ii] ) ;

           free( buf[ii] ) ; buf[ii] = NULL ;

           /* put result into output dataset */

           EDIT_substitute_brick(new_dset, ii, CALC_datum, dfim[ii] );

           DSET_BRICK_FACTOR(new_dset,ii) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
         }
      }
      break ;
   }

   if( tempfile != NULL ){                   /* 18 Oct 2005 */
     INFO_message("Deleting -usetemp file %s",tempfnam) ;
     fclose(tempfile) ; tempfile = NULL ;
     remove(tempfnam) ; tempfnam = NULL ;
   }

   if( CALC_verbose ) INFO_message("Computing output statistics\n") ;
   THD_load_statistics( new_dset ) ;

   DSET_BRICK_FDRCURVE_ALLKILL(new_dset) ;  /* 24 Jan 2008 */

   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   WROTE_DSET(new_dset) ;

   exit(0) ;
}
