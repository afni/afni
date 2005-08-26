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

Added ability to operate on 3D bucket datasets -- see ALLOW_BUCKETS macro.
  [RW Cox, April 1998]

Added ability to use sub-brick selectors on input datasets -- see ALLOW_SUBV macro.
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

#define ALLOW_BUCKETS
#define ALLOW_SUBV

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

static int                CALC_histpar = -1 ; /* 22 Nov 1999 */

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
         } else if( strcasecmp(argv[nopt],"complex") == 0 ){  /* not listed in help */
            CALC_datum = MRI_complex ;
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
                          strstr(argv[nopt],"1D:") != NULL   ) ){

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

            CALC_has_timeshift = CALC_has_timeshift || (ijkl[4] != 0) ;

            /*- time to trot, Bwana -*/

            free(ijkl) ; nopt++ ; goto DSET_DONE ;

         } /* end of _dshift */

         /*-- meanwhile, back at the "normal" dataset opening ranch --*/

#ifndef ALLOW_SUBV
         dset = THD_open_one_dataset( argv[nopt++] ) ;
         if( dset == NULL )
           ERROR_exit("can't open dataset %s\n",argv[nopt-1]) ;
         if( isub >= DSET_NVALS(dset) )
           ERROR_exit("dataset %s only has %d sub-bricks\n",
                      argv[nopt-1],DSET_NVALS(dset)) ;
#else
         { char dname[512] ;                               /* 02 Nov 1999 */

           if( ids > 2 ){                                  /* mangle name */
              if( strstr(argv[nopt],"[") != NULL ){
                ERROR_exit(
                         "Illegal combination of sub-brick specifiers: "
                         "%s %s\n" ,
                         argv[nopt-1] , argv[nopt] ) ;
              }
              sprintf(dname,"%s[%d]",argv[nopt++],isub) ;  /* use sub-brick */
              isub = 0 ;                                   /* 0 of dname    */
           } else {
              strcpy(dname,argv[nopt++]) ;                 /* don't mangle */
           }
           dset = THD_open_dataset( dname ) ;              /* open it */
           if( dset == NULL )
              ERROR_exit("can't open dataset %s\n",dname) ;
         }
#endif

         /* set some parameters based on the dataset */

#ifdef ALLOW_BUCKETS
         ntime[ival] = DSET_NVALS(dset) ;
#else
         ntime[ival] = DSET_NUM_TIMES(dset);
#endif
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

           DSET_mallocize(dset) ; DSET_load(dset) ;
           if( ! DSET_LOADED(dset) )
             ERROR_exit("can't load %s from disk!\n",argv[nopt-1]);

           for( ii=0 ; ii < ntime[ival] ; ii++ ){
             if( DSET_BRICK_TYPE(dset,ii) != MRI_float ){
               far = calloc( sizeof(float) , nxyz ) ;
               if( far == NULL )
                 ERROR_exit("can't malloc space for conversion\n");
               EDIT_coerce_scale_type( nxyz , DSET_BRICK_FACTOR(dset,ii) ,
                                       DSET_BRICK_TYPE(dset,ii), DSET_ARRAY(dset,ii),
                                       MRI_float , far ) ;
               EDIT_substitute_brick( dset , ii , MRI_float , far ) ;
               DSET_BRICK_FACTOR(dset,ii) = 0.0 ;
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
           THD_load_datablock( dset->dblk ) ;
           if( ! DSET_LOADED(dset) )
             ERROR_exit("Can't read data brick for dataset %s\n",argv[nopt-1]) ;
         }
         /* set pointers for actual dataset arrays */

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

            default:
               ERROR_exit("Dataset %s has illegal data type: %s\n" ,
                         argv[nopt-1] , MRI_type_name[CALC_type[ival]] ) ;

         } /* end of switch over type switch */
         if( CALC_datum < 0 && CALC_type[ival] != MRI_rgb ) CALC_datum = CALC_type[ival] ;

DSET_DONE: continue;

      } /* end of dataset input */

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;

   }  /* end of loop over options */

   /*---------------------------------------*/
   /*** cleanup: check for various errors ***/

   if( nopt < argc )
    ERROR_exit("Extra command line arguments puzzle me! argv[%d]=%s ...\n",nopt,argv[nopt]) ;

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   if( ids == 26 )
     ERROR_exit("No actual input datasets given!\n") ;

   /* 22 Feb 2005: check IJKAR inputs against 1st dataset found */

   for( ii=0 ; ii < 26 ; ii++ ){
     if( IJKAR_flim[ii] != NULL ){
       int siz ;
       switch( IJKAR_dcod[ii] ){
         case  8: siz = DSET_NX(CALC_dset[ids]) ; break ;
         case  9: siz = DSET_NY(CALC_dset[ids]) ; break ;
         case 10: siz = DSET_NZ(CALC_dset[ids]) ; break ;
       }
       if( IJKAR_flim[ii]->nx != siz )
         ERROR_message("dimension mismatch between '-%c' and '%-c'\n", 'a'+ii , 'a'+ids ) ;
     }
   }

   if( CALC_code == NULL ) ERROR_exit("No expression given!\n") ;

   if( CALC_histpar >= 0 && CALC_dset[CALC_histpar] == NULL ){
     WARNING_message("-histpar dataset not defined!\n") ;
     CALC_histpar = -1 ;
   }

   for (ids=0; ids < 26; ids ++)
      if (ntime[ids] > 1 && ntime[ids] != ntime_max ) {
#ifdef ALLOW_BUCKETS
          ERROR_exit("Multi-brick datasets don't match!\n") ;
#else
          ERROR_exit("3D+time datasets don't match!\n") ;
#endif
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
    "     (limited to inter-voxel computation).                              \n"
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
    "6. Same as example #5, but this time create a mask of 8 different values\n"
    "   showing all combinations of activations (i.e., not only where        \n"
    "   everything is active, but also each stimulus individually, and all   \n"
    "   combinations).  The output mask dataset labels voxel values as such: \n"
    "        0 = none active    1 = A only active    2 = B only active       \n"
    "        3 = A and B only   4 = C only active    5 = A and C only        \n" 
    "        6 = B and C only   7 = all A, B, and C active                   \n"
    "                                                                        \n"
    "     3dcalc -a 'func+orig[12]' -b 'func+orig[15]' -c 'func+orig[18]' \\ \n"
    "            -expr 'step(a-4.2)+2*step(b-2.9)+4*step(c-3.1)'          \\ \n"
    "            -prefix mask_8                                              \n"
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
    "            -expr 'step((9-(x-20)*(x-20)-(y-30)*(y-30)-(z-70)*(z-70))'\\\n"
    "            -prefix ball                                                \n"
    "                                                                        \n"
    " 8. Some datsets are 'short' (16 bit) integers with a scalar attached,  \n"
    "    which allow them to be smaller than float datasets and to contain   \n"
    "    fractional values.                                                  \n"
    "                                                                        \n"
    "    Dataset 'a' is always used as a template for the output dataset. For\n"
    "    the examples below, assume that datasets d1+orig and d2+orig consist\n"
    "    of small integers.                                                  \n"
    "                                                                        \n"
    "    a) When dividing 'a' by 'b', the result should be scaled, so that a \n"
    "       value of 2.4 is not truncated to '2'. To avoid this truncation,  \n"
    "       force scaling with the -fscale option:                           \n"
    "                                                                        \n"
    "          3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot -fscale \n"
    "                                                                        \n"
    "    b) If it is preferable that the result is of type 'float', then set \n"
    "       the output data type (datum) to float:                           \n"
    "                                                                        \n"
    "          3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot \\      \n"
    "                 -datum float                                           \n"
    "                                                                        \n"
    "    c) Perhaps an integral division is desired, so that 9/4=2, not 2.24.\n"
    "       Force the results not to be scaled (opposite of example 8b) using\n"
    "       the -nscale option:                                              \n"
    "                                                                        \n"
    "          3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot -nscale \n"
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    "                                                                        \n"
    "ARGUMENTS for 3dcalc (must be included on command line):                \n"
    "--------------------  ----                                              \n"
    "                                                                        \n"
    " -a dname    = Read dataset 'dname' and call the voxel values 'a' in the\n"
    "               expression (-expr) that is input below. Up to 24 dnames  \n"
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
    "                                                                        \n"
    " -expr       = Apply the expression - within quotes - to the input      \n"
    "               datasets (dnames), one voxel at time, to produce the     \n"
    "               output dataset.                                          \n"
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
    "                                                                        \n"
    "  -nscale    = Don't do any scaling on output to byte or short datasets.\n"
    "               This may be especially useful when operating on mask     \n"
    "               datasets whose output values are only 0's and 1's.       \n"
#ifndef ALLOW_SUBV
    "               ** The type and number of sub-bricks in a dataset can be \n"
    "                  printed out using the '3dinfo' program.               \n"
#else
    "               ** Another way to achieve the effect of '-b3' is described\n"
    "                  below in the dataset 'INPUT' specification section.   \n"
#endif
    "                                                                        \n"
    "  -prefix pname = Use 'pname' for the output dataset prefix name.       \n"
    "                  [default='calc']                                      \n"
    "                                                                        \n"
    "  -session dir  = Use 'dir' for the output dataset session directory.   \n"
    "                  [default='./'=current working directory]              \n"
    "                                                                        \n"
    "  -dt tstep     = Use 'tstep' as the TR for manufactured 3D+time datasets.\n"
    "                                                                        \n"
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
#ifdef ALLOW_BUCKETS
    " Multi-brick 'bucket' datasets may also be used.  Note that if multi-brick\n"
    " (bucket or 3D+time) datasets are used, the lowest letter dataset will  \n"
    " serve as the template for the output; that is, '-b fred+tlrc' takes    \n"
    " precedence over '-c wilma+tlrc'.  (The program 3drefit can be used to  \n"
    " alter the .HEAD parameters of the output dataset, if desired.)         \n"
#endif

#ifdef ALLOW_SUBV
    "                                                                        \n"
    "------------------------------------------------------------------------\n"
    MASTER_HELP_STRING
    "                                                                        \n"
    "** WARNING: you cannot combine sub-brick selection of the form          \n"
    "               -b3 bambam+orig       (the old method)                   \n"
    "            with sub-brick selection of the form                        \n"
    "               -b  'bambam+orig[3]'  (the new method)                   \n"
    "            If you try, the Doom of Mandos will fall upon you!          \n"
#endif
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
    "------------------------------------------------------------------------\n"
    "PROBLEMS:\n"
    "-------- \n"
    "\n"
    " * Complex-valued datasets cannot be processed.\n"
    " * This program is not very efficient (but is faster than it once was).\n"
    " * Differential subscripts slow the program down even more.\n"
    "\n"
    "------------------------------------------------------------------------\n"
    "EXPRESSIONS:\n"
    "----------- \n"
    "\n"
    " Arithmetic expressions are allowed, using + - * / ** and parentheses.\n"
    " As noted above, datasets are referred to by single letter variable names.\n"
    " At this time, C relational, boolean, and conditional expressions are\n"
    " NOT implemented.  Built in functions include:\n"
    "\n"
    "   sin  , cos  , tan  , asin  , acos  , atan  , atan2,                  \n"
    "   sinh , cosh , tanh , asinh , acosh , atanh , exp  ,                  \n"
    "   log  , log10, abs  , int   , sqrt  , max   , min  ,                  \n"
    "   J0   , J1   , Y0   , Y1    , erf   , erfc  , qginv, qg ,             \n"
    "   rect , step , astep, bool  , and   , or    , mofn ,                  \n"
    "   sind , cosd , tand , median, lmode , hmode , mad  ,                  \n"
    "   gran , uran , iran , eran  , lran  , orstat,                         \n"
    "   mean , stdev, sem  , Pleg\n"
    "\n"
    " where:\n"
    " * qg(x)    = reversed cdf of a standard normal distribution\n"
    " * qginv(x) = inverse function to qg\n"
    " * min, max, atan2 each take 2 arguments ONLY\n"
    " * J0, J1, Y0, Y1 are Bessel functions (see Watson)\n"
    " * Pleg(m,x) is the m'th Legendre polynomial evaluated at x\n"
    " * erf, erfc are the error and complementary error functions\n"
    " * sind, cosd, tand take arguments in degrees (vs. radians)\n"
    " * median(a,b,c,...) computes the median of its arguments\n"
    " * mad(a,b,c,...) computes the MAD of its arguments\n"
    " * mean(a,b,c,...) computes the mean of its arguments\n"
    " * stdev(a,b,c,...) computes the standard deviation of its arguments\n"
    " * sem(a,b,c,...) computes the standard error of the mean of its arguments,\n"
    "                  where sem(n arguments) = stdev(same)/sqrt(n)\n"
    " * orstat(n,a,b,c,...) computes the n-th order statistic of\n"
    "    {a,b,c,...} - that is, the n-th value in size, starting\n"
    "    at the bottom (e.g., orstat(1,a,b,c) is the minimum)\n"
    " * lmode(a,b,c,...) and hmode(a,b,c,...) compute the mode\n"
    "    of their arguments - lmode breaks ties by choosing the\n"
    "    smallest value with the maximal count, hmode breaks ties by\n"
    "    choosing the largest value with the maximal count\n"
    "    [median,lmode,hmode take a variable number of arguments]\n"
    " * gran(m,s) returns a Gaussian deviate with mean=m, stdev=s\n"
    " * uran(r)   returns a uniform deviate in the range [0,r]\n"
    " * iran(t)   returns a random integer in the range [0..t]\n"
    " * eran(s)   returns an exponentially distributed deviate\n"
    " * lran(t)   returns a logistically distributed deviate\n"
    "\n"
    " You may use the symbol 'PI' to refer to the constant of that name.\n"
    " This is the only 2 letter symbol defined; all input files are\n"
    " referred to by 1 letter symbols.  The case of the expression is\n"
    " ignored (in fact, it is converted to uppercase as the first step\n"
    " in the parsing algorithm).\n"
    "\n"
    " The following functions are designed to help implement logical\n"
    " functions, such as masking of 3D volumes against some criterion:\n"
    "       step(x)    = {1 if x>0        , 0 if x<=0},\n"
    "       astep(x,y) = {1 if abs(x) > y , 0 otherwise} = step(abs(x)-y)\n"
    "       rect(x)    = {1 if abs(x)<=0.5, 0 if abs(x)>0.5},\n"
    "       bool(x)    = {1 if x != 0.0   , 0 if x == 0.0},\n"
    "    notzero(x)    = bool(x),\n"
    "     iszero(x)    = 1-bool(x) = { 0 if x != 0.0, 1 if x == 0.0 },\n"
    "     equals(x,y)  = 1-bool(x-y) = { 1 if x == y , 0 if x != y },\n"
    "   ispositive(x)  = { 1 if x > 0; 0 if x <= 0 },\n"
    "   isnegative(x)  = { 1 if x < 0; 0 if x >= 0 },\n"
    "   and(a,b,...,c) = {1 if all arguments are nonzero, 0 if any are zero}\n"
    "    or(a,b,...,c) = {1 if any arguments are nonzero, 0 if all are zero}\n"
    "  mofn(m,a,...,c) = {1 if at least 'm' arguments are nonzero, 0 otherwise}\n"
    "  argmax(a,b,...) = index of largest argument; = 0 if all args are 0\n"
    "  argnum(a,b,...) = number of nonzero arguments\n"
    "\n"
    "  [These last 5 functions take a variable number of arguments.]\n"
    "\n"
    " The following 27 new [Mar 1999] functions are used for statistical\n"
    " conversions, as in the program 'cdf':\n"
    "   fico_t2p(t,a,b,c), fico_p2t(p,a,b,c), fico_t2z(t,a,b,c),\n"
    "   fitt_t2p(t,a)    , fitt_p2t(p,a)    , fitt_t2z(t,a)    ,\n"
    "   fift_t2p(t,a,b)  , fift_p2t(p,a,b)  , fift_t2z(t,a,b)  ,\n"
    "   fizt_t2p(t)      , fizt_p2t(p)      , fizt_t2z(t)      ,\n"
    "   fict_t2p(t,a)    , fict_p2t(p,a)    , fict_t2z(t,a)    ,\n"
    "   fibt_t2p(t,a,b)  , fibt_p2t(p,a,b)  , fibt_t2z(t,a,b)  ,\n"
    "   fibn_t2p(t,a,b)  , fibn_p2t(p,a,b)  , fibn_t2z(t,a,b)  ,\n"
    "   figt_t2p(t,a,b)  , figt_p2t(p,a,b)  , figt_t2z(t,a,b)  ,\n"
    "   fipt_t2p(t,a)    , fipt_p2t(p,a)    , fipt_t2z(t,a)    .\n"
    "\n"
    " See the output of 'cdf -help' for documentation on the meanings of\n"
    " and arguments to these functions.  (After using one of these, you\n"
    " may wish to use program '3drefit' to modify the dataset statistical\n"
    " auxiliary parameters.)\n"
    "\n"
    " Computations are carried out in double precision before being\n"
    " truncated to the final output 'datum'.\n"
    "\n"
    " Note that the quotes around the expression are needed so the shell\n"
    " doesn't try to expand * characters, or interpret parentheses.\n"
    "\n"
    " (Try the 'ccalc' program to see how the expression evaluator works.\n"
    "  The arithmetic parser and evaluator is written in Fortran-77 and\n"
    "  is derived from a program written long ago by RW Cox to facilitate\n"
    "  compiling on an array processor hooked up to a VAX.  It's a mess,\n"
    "  but it works - somewhat slowly.)\n"
   ) ;
   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
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

   /*** read input options ***/

   if( argc < 2 || strncasecmp(argv[1],"-help",4) == 0 ) CALC_Syntax() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dcalc main"); machdep() ; PRINT_VERSION("3dcalc") ;
   THD_check_AFNI_version() ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dcalc",argc,argv) ;

   for (ii=0; ii<26; ii++) ntime[ii] = 0 ;

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

   if( jjj == 1 ){
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

   if( THD_is_file(new_dset->dblk->diskptr->header_name) )
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

   for ( kt = 0 ; kt < ntime_max ; kt ++ ) {

      if( CALC_verbose )
        INFO_message("Computing sub-brick %d\n",kt) ;

      /* 30 April 1998: only malloc output space as it is needed */

      buf[kt] = (float *) malloc(sizeof(float) * CALC_nvox);
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

            /* 22 Feb 2005: IJKAR 1D arrays */

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

            /* 22 Nov 1999: if a differentially subscripted dataset is here */

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

               if( !dun ){
                  for( dun=0,jj=jbot ; jj < jtop ; jj++ ){
                     jjs = jj ;
                     if( ijkd ){
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
                     }
                     switch( CALC_type[jds] ) {
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
                     }
                  }
               }
            }

            /* the case of a 3D dataset (i.e., only 1 sub-brick) */

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
               }
            }

           /* the case of a 3D+time dataset (or a bucket, etc.) */

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

         } /* end of loop over space (voxels) */

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

   } /* end of loop over time steps */

   for( ids=0 ; ids < 26 ; ids++ ){
      if( CALC_dset[ids] != NULL ) PURGE_DSET( CALC_dset[ids] ) ;
      if( TS_flim[ids]   != NULL ) mri_free( TS_flim[ids] ) ;
      if( IJKAR_flim[ids]!= NULL ) mri_free( IJKAR_flim[ids] ) ;
   }

   /*** attach new data to output brick ***/

   switch( CALC_datum ){

      default:
        ERROR_exit("Somehow ended up with CALC_datum = %d\n",CALC_datum) ;
      exit(1) ;

      /* the easy case! */

      case MRI_float:{
        for( ii=0 ; ii < ntime_max ; ii++ ){
          EDIT_substitute_brick(new_dset, ii, MRI_float, buf[ii]);
          DSET_BRICK_FACTOR(new_dset, ii) = 0.0;
        }
      }
      break ;

      /* the harder cases */

      case MRI_byte:             /* modified 31 Mar 1999 to scale each sub-brick  */
      case MRI_short:{           /* with its own factor, rather than use the same */
         void ** dfim ;          /* factor for each sub-brick -- RWCox            */
         float gtop , fimfac , gtemp ;

         if( CALC_verbose )
           INFO_message("Scaling output to type %s brick(s)\n",
                        MRI_TYPE_name[CALC_datum] ) ;

         dfim = (void ** ) malloc( sizeof( void * ) * ntime_max ) ;

         if( CALC_gscale ){   /* 01 Apr 1999: global scaling */
           gtop = 0.0 ;
           for( ii=0 ; ii < ntime_max ; ii++ ){
             gtemp = MCW_vol_amax( CALC_nvox , 1 , 1 , MRI_float, buf[ii] ) ;
             gtop  = MAX( gtop , gtemp ) ;
             if( gtemp == 0.0 )
               WARNING_message("output sub-brick %d is all zeros!\n",ii) ;
           }
         }

         for (ii = 0 ; ii < ntime_max ; ii ++ ) {

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

             fimfac = (gtop > MRI_TYPE_maxval[CALC_datum] || (gtop > 0.0 && gtop <= 1.0) )
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
           free( buf[ii] ) ;

           /* put result into output dataset */

           EDIT_substitute_brick(new_dset, ii, CALC_datum, dfim[ii] );

           DSET_BRICK_FACTOR(new_dset,ii) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
         }
      }
      break ;
   }

   if( CALC_verbose ) INFO_message("Computing output statistics\n") ;
   THD_load_statistics( new_dset ) ;

   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   if( CALC_verbose ) WROTE_DSET(new_dset) ;

   exit(0) ;
}
