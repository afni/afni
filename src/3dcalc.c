

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
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

static THD_3dim_dataset *  CALC_dset[26] ;
static int                 CALC_type[26] ;
static byte **             CALC_byte[26] ;
static short **            CALC_short[26] ;
static float **            CALC_float[26] ;
static float *             CALC_ffac[26] ;

static int                CALC_verbose = 0 ; /* 30 April 1998 */

static char CALC_output_prefix[THD_MAX_PREFIX] = "calc" ;

static char CALC_session[THD_MAX_NAME]         = "./"   ;

static MRI_IMAGE * TS_flim[26] ;  /* 17 Apr 1998 */
static float *     TS_flar[26] ;
static int         TS_nmax = 0 ;
static int         TS_make = 0 ;

/*--------------------------- prototypes ---------------------------*/
void CALC_read_opts( int , char ** ) ;
void CALC_Syntax(void) ;
int  TS_reader( int , char * ) ;

/*--------------------------------------------------------------------
  Read a time series file into variable number ival.
  Returns -1 if an error occured, 0 otherwise.
----------------------------------------------------------------------*/

int TS_reader( int ival , char * fname )
{
   MRI_IMAGE * tsim ;

   if( ival < 0 || ival >= 26 ) return -1 ;

   tsim = mri_read_ascii( fname ) ;
   if( tsim == NULL ) return -1 ;
   if( tsim->ny < 2 ){ mri_free(tsim) ; return -1 ; }

   TS_flim[ival] = mri_transpose(tsim) ;
   TS_nmax       = MAX( TS_nmax , TS_flim[ival]->nx ) ;
   TS_flar[ival] = MRI_FLOAT_PTR( TS_flim[ival] ) ;
   mri_free(tsim) ;
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
      CALC_type[ids] = -1 ;
      TS_flim[ids]   = NULL ;
   }

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -datum type ****/

      if( strncmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -datum!\n") ; exit(1) ;
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            CALC_datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            CALC_datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            CALC_datum = MRI_byte ;
         } else if( strcmp(argv[nopt],"complex") == 0 ){  /* not listed help */
            CALC_datum = MRI_complex ;
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported in 3dmerge!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -verbose [30 April 1998] ****/

      if( strncmp(argv[nopt],"-verbose",6) == 0 ){
         CALC_verbose = 1 ;
         nopt++ ; continue ;
      }

      /**** -fscale [16 Mar 1998] ****/

      if( strncmp(argv[nopt],"-fscale",6) == 0 ){
         CALC_fscale = 1 ;
         nopt++ ; continue ;
      }

      /**** -gscale [01 Apr 1999] ****/

      if( strncmp(argv[nopt],"-gscale",6) == 0 ){
         CALC_gscale = CALC_fscale = 1 ;
         nopt++ ; continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -prefix!\n") ; exit(1) ;
         }
         MCW_strncpy( CALC_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -session!\n") ; exit(1) ;
         }
         MCW_strncpy( CALC_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -expr expression ****/

      if( strncmp(argv[nopt],"-expr",4) == 0 ){
         if( CALC_code != NULL ){
            fprintf(stderr,"cannot have 2 -expr options!\n") ; exit(1) ;
         }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -expr!\n") ; exit(1) ;
         }
         CALC_code = PARSER_generate_code( argv[nopt++] ) ;
         if( CALC_code == NULL ){
            fprintf(stderr,"illegal expression!\n") ; exit(1) ;
         }
         continue ;
      }

      /**** -<letter>[number] dataset ****/

      ids = strlen( argv[nopt] ) ;

      if( (argv[nopt][1] >= 'a' && argv[nopt][1] <= 'z') &&
          (ids == 2 ||
           (ids > 2 && argv[nopt][2] >= '0' && argv[nopt][2] <= '9')) ){

         int ival , nxyz , isub , ll ;
         THD_3dim_dataset * dset ;

         ival = argv[nopt][1] - 'a' ;
         if( CALC_dset[ival] != NULL || TS_flim[ival] != NULL ){
            fprintf(stderr,"can't open duplicate %s datasets!\n",argv[nopt]) ;
            exit(1) ;
         }

         isub = (ids == 2) ? 0 : strtol(argv[nopt]+2,NULL,10) ;

         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after %s!\n",argv[nopt-1]) ; exit(1) ;
         }

         /*-- 17 Apr 1998: allow for a *.1D filename --*/

         ll = strlen(argv[nopt]) ;
         if( ll >= 4 && strcmp(argv[nopt]+(ll-3),".1D") == 0 ){
            ll = TS_reader( ival , argv[nopt] ) ;
            if( ll == 0 ){ nopt++ ;  goto DSET_DONE ; }

            /* get to here => something bad happened, so try it as a dataset */
         }

         /*-- back to the normal dataset opening routine --*/
#ifndef ALLOW_SUBV
         dset = THD_open_one_dataset( argv[nopt++] ) ;
#else
         dset = THD_open_dataset( argv[nopt++] ) ;
#endif
         if( dset == NULL ){
            fprintf(stderr,"can't open dataset %s\n",argv[nopt-1]) ; exit(1) ;
         }

         if( isub >= DSET_NVALS(dset) ){
            fprintf(stderr,"dataset %s only has %d sub-bricks\n",
                    argv[nopt-1],DSET_NVALS(dset)) ;
            exit(1) ;
         }

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
            fprintf(stderr,"dataset %s differs in size from others\n",argv[nopt-1]);
            exit(1) ;
         }

         CALC_type[ival] = DSET_BRICK_TYPE(dset,isub) ;
         CALC_dset[ival] = dset ;

         /* load floating scale factors */

         CALC_ffac[ival] = (float * ) malloc( sizeof(float) * ntime[ival] ) ;
         if ( ntime[ival] == 1 ) {
            CALC_ffac[ival][0] = DSET_BRICK_FACTOR( dset , isub) ;
            if (CALC_ffac[ival][0] == 0.0 ) CALC_ffac[ival][0] = 1.0 ;
         } else {
             for (ii = 0 ; ii < ntime[ival] ; ii ++ ) {
               CALC_ffac[ival][ii] = DSET_BRICK_FACTOR(dset, ii) ;
               if (CALC_ffac[ival][ii] == 0.0 ) CALC_ffac[ival][ii] = 1.0;
             }
         }

         /* read data from disk */

         if( CALC_verbose )
            fprintf(stderr,"  ++ Reading dataset %s\n",argv[nopt-1]) ;

         THD_load_datablock( dset->dblk , NULL ) ;
         if( ! DSET_LOADED(dset) ){
            fprintf(stderr,"Can't read data brick for dataset %s\n",argv[nopt-1]) ;
            exit(1) ;
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

	 } /* end of switch over type switch */
         if( CALC_datum < 0 ) CALC_datum = CALC_type[ival] ;

DSET_DONE: continue;

      } /* end of dataset input */

      fprintf(stderr,"Unknown option: %s\n",argv[nopt]) ; exit(1) ;

   }  /* end of loop over options */

   /*---------------------------------------*/
   /*** cleanup: check for various errors ***/

   if( nopt < argc ){
     fprintf(stderr,"Extra command line arguments puzzle me! %s ...\n",argv[nopt]) ;
     exit(1) ;
   }

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   if( ids == 26 ){
      fprintf(stderr,"No input datasets given!\n") ; exit(1) ;
   }

   if( CALC_code == NULL ){
      fprintf(stderr,"No expression given!\n") ; exit(1) ;
   }

   for (ids=0; ids < 26; ids ++)
      if (ntime[ids] > 1 && ntime[ids] != ntime_max ) {
#ifdef ALLOW_BUCKETS
	 fprintf(stderr, "Multi-brick datasets don't match!\n") ;
#else
	 fprintf(stderr, "3D+time datasets don't match!\n") ;
#endif
         exit(1) ;
      }

   /* 17 Apr 1998: if all input datasets are 3D only (no time),
                   and if there are any input time series,
                   then the output must become 3D+time itself  */

   if( ntime_max == 1 && TS_nmax > 0 ){
      ntime_max = TS_nmax ;
      TS_make   = 1 ;        /* flag to force manufacture of a 3D+time dataset */
      fprintf(stderr,
              "*** Calculating 3D+time[%d]"
              " dataset from 3D datasets and time series\n" ,
              ntime_max ) ;
   }

   return ;
}

/*------------------------------------------------------------------*/

void CALC_Syntax(void)
{
   printf(
    "Do arithmetic on 3D datasets, voxel-by-voxel [no inter-voxel computation].\n"
    "Usage: 3dcalc [options]\n"
    "where the options are:\n"
   ) ;

   printf(
    "  -verbose    = Makes the program print out various information as\n"
    "                  it progresses.\n"
    "  -datum type = Coerce the output data to be stored as the given type,\n"
    "                  which may be byte, short, or float.\n"
    "                  [default = datum of first input dataset]\n"
    "  -fscale     = Force scaling of the output to the maximum integer\n"
    "                  range.  This only has effect if the output datum\n"
    "                  is byte or short (either forced or defaulted).\n"
    "                  This option is often necessary to eliminate unpleasant\n"
    "                  truncation artifacts.\n"
    "                  [The default is to scale only if the computed values\n"
    "                   seem to need it -- are all less than 1 or there is\n"
    "                   at least one value beyond the integer upper limit.]\n"
    "             ** In earlier versions of 3dcalc, scaling (if used) was\n"
    "                   applied to all sub-bricks equally -- a common scale\n"
    "                   factor was used.  This would cause trouble if the values\n"
    "                   in different sub-bricks were in vastly different scales.\n"
    "                   In this version, each sub-brick gets its own scale factor.\n"
    "                   To override this behaviour, use the '-gscale' option.\n"
    "  -gscale     = Same as '-fscale', but also forces each output sub-brick to\n"
    "                   get the same scaling factor.  This may be desirable\n"
    "                   for 3D+time datasets, for example.\n"
    "  -a dname    = Read dataset 'dname' and call the voxel values 'a'\n"
    "                  in the expression that is input below.  'a' may be any\n"
    "                  single letter from 'a' to 'z'.\n"
    "             ** If some letter name is used in the expression, but not\n"
    "                  present in one of the dataset options here, then that\n"
    "                  variable is set to 0.\n"
    "             ** If the letter is followed by a number, then that number\n"
    "                  is used to select the sub-brick of the dataset which\n"
    "                  will be used in the calculations.  For example,\n"
    "                  '-b3 dname' specifies that the variable 'b' refers to\n"
    "                  sub-brick 3 of the dataset (indexes start at 0).\n"

#ifndef ALLOW_SUBV
    "             ** The type and number of sub-bricks in a dataset can be\n"
    "                  printed out using the '3dinfo' program.\n"
#else
    "            N.B.: Another way to achieve the effect of '-b3' is described\n"
    "                  below in the 'INPUT DATASET SPECIFICATION' section.\n"
#endif

    "  -expr \"expression\"\n"
    "                Apply the expression within quotes to the input datasets,\n"
    "                one voxel at a time, to produce the output dataset.\n"
    "                (\"sqrt(a*b)\" to compute the geometric mean, for example).\n"
    "  -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    "                    [default='calc']\n"
    "  -session dir  = Use 'dir' for the output dataset session directory.\n"
    "                    [default='./'=current working directory]\n"
    "\n"
    "3D+TIME DATASETS:\n"
    " This version of 3dcalc can operate on 3D+time datasets.  Each input dataset\n"
    " will be in one of these conditions:\n"
    "    (A) Is a regular 3D (no time) dataset; or\n"
    "    (B) Is a 3D+time dataset with a sub-brick index specified ('-b3'); or\n"
    "    (C) Is a 3D+time dataset with no sub-brick index specified ('-b').\n"
    " If there is at least one case (C) dataset, then the output dataset will\n"
    " also be 3D+time; otherwise it will be a 3D dataset with one sub-brick.\n"
    " When producing a 3D+time dataset, datasets in case (A) or (B) will be\n"
    " treated as if the particular brick being used has the same value at each\n"
    " point in time.\n"

#ifdef ALLOW_BUCKETS
    " Multi-brick 'bucket' datasets may also be used.  Note that if multi-brick\n"
    " (bucket or 3D+time) datasets are used, the lowest letter dataset will\n"
    " serve as the template for the output; that is, '-b fred+tlrc' takes\n"
    " precedence over '-c wilma+tlrc'.  (The program 3drefit can be used to\n"
    " alter the .HEAD parameters of the output dataset, if desired.)\n"
#endif

#ifdef ALLOW_SUBV
    "\n"
    "INPUT DATASET SPECIFICATION:\n"
    MASTER_HELP_STRING
#endif

    "\n"
    "1D TIME SERIES:\n"
    " You can also input a '*.1D' time series file in place of a dataset.\n"
    " In this case, the value at each spatial voxel at time index n will be\n"
    " the same, and will be the n-th value from the time series file.\n"
    " At least one true dataset must be input.  If all the input datasets\n"
    " are 3D (single sub-brick) or are single sub-bricks from multi-brick\n"
    " datasets, then the output will be a 'manufactured' 3D+time dataset\n"
    " For example, suppose that 'a3D+orig' is a 3D dataset:\n"
    "   3dcalc -a a3D+orig -b b.1D -expr \"a*b\" \n"
    " The output dataset will 3D+time with the value at (x,y,z,t) being\n"
    " computed by a3D(x,y,z)*b(t).  The TR for this dataset will be set\n"
    " to 1 second -- this could be altered later with program 3drefit.\n"
    " Another method to set up the correct timing would be to input an\n"
    " unused 3D+time dataset -- 3dcalc will then copy that dataset's time\n"
    " information, but simply do not use that dataset's letter in -expr.\n"

    "\n"
    "COORDINATES:\n"
    " If you don't use '-x', '-y', or '-z' for a dataset, then the voxel\n"
    " spatial coordinates will be loaded into those variables.  For example,\n"
    " the expression 'a*step(x*x+y*y+z*z-100)' will zero out all the voxels\n"
    " inside a 10 mm radius of the origin.\n"
    "\n"
    "PROBLEMS:\n"
    " ** Complex-valued datasets cannot be processed.\n"
    " ** This program is not very efficient (but is faster than before).\n"
    "\n"
    "EXPRESSIONS:\n"
    " Arithmetic expressions are allowed, using + - * / ** and parentheses.\n"
    " As noted above, datasets are referred to by single letter variable names.\n"
    " At this time, C relational, boolean, and conditional expressions are\n"
    " NOT implemented.  Built in functions include:\n"
    "   sin  , cos  , tan  , asin  , acos  , atan  , atan2,\n"
    "   sinh , cosh , tanh , asinh , acosh , atanh , exp  ,\n"
    "   log  , log10, abs  , int   , sqrt  , max   , min  ,\n"
    "   J0   , J1   , Y0   , Y1    , erf   , erfc  , qginv, qg ,\n"
    "   rect , step , astep, bool  , and   , or    , mofn ,\n"
    "   sind , cosd , tand , median,\n"
    " where qg(x)    = reversed cdf of a standard normal distribution\n"
    "       qginv(x) = inverse function to qg,\n"
    "       min, max, atan2 each take 2 arguments,\n"
    "       J0, J1, Y0, Y1 are Bessel functions (see Watson),\n"
    "       erf, erfc are the error and complementary error functions,\n"
    "       sind, cosd, tand take arguments in degrees (vs. radians),\n"
    "       median(a,b,c,...) computes the median of its arguments\n"
    "         [median takes a variable number of arguments].\n"
    "\n"
    " The following functions are designed to help implement logical\n"
    " functions, such as masking of 3D volumes against some criterion:\n"
    "       step(x)    = {1 if x>0        , 0 if x<=0},\n"
    "       astep(x,y) = {1 if abs(x) > y , 0 otherwise} = step(abs(x)-y)\n"
    "       rect(x)    = {1 if abs(x)<=0.5, 0 if abs(x)>0.5},\n"
    "       bool(x)    = {1 if x != 0.0   , 0 if x == 0.0},\n"
    "   and(a,b,...,c) = {1 if all arguments are nonzero, 0 if any are zero}\n"
    "    or(a,b,...,c) = {1 if any arguments are nonzero, 0 if all are zero}\n"
    "  mofn(m,a,...,c) = {1 if at least 'm' arguments are nonzero, 0 otherwise}\n"
    " These last 3 functions take a variable number of arguments.\n"
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
    "  but it works.)\n"
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

   THD_ivec3 iv ;       /* 05 Feb 1999:                */
   THD_fvec3 fv ;       /* stuff for computing (x,y,z) */
   float xxx,yyy,zzz ;  /* coords for each voxel       */
   int   iii,jjj,kkk , nx,nxy ;
   THD_dataxes * daxes ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) CALC_Syntax() ;

   for (ii=0; ii<26; ii++) ntime[ii] = 0 ;

   CALC_read_opts( argc , argv ) ;

   /*** make output dataset ***/

   if( ntime_max == 1 || TS_make == 1 ){
      for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   } else {
      for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL &&
                                          ntime[ids] > 1           ) break ;
   }
   if( ids == 26 ){fprintf(stderr,"Can't find template dataset?!\n");exit(1);}

   new_dset = EDIT_empty_copy( CALC_dset[ids] ) ;

   for( iii=jjj=0 ; iii < 26 ; iii++ )
      if( CALC_dset[iii] != NULL ) jjj++ ;

   if( jjj == 1 ) tross_Copy_History( CALC_dset[ids] , new_dset ) ;
   tross_Make_History( "3dcalc" , argc,argv , new_dset ) ;

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
                          ADN_ttdel  , 1.0            ,
                          ADN_ttorg  , 0.0            ,
                          ADN_ttdur  , 0.0            ,
                          ADN_tunits , UNITS_SEC_TYPE ,
                       ADN_none ) ;
   }

   if( ISFUNC(new_dset) && ! ISFUNCBUCKET(new_dset) && new_dset->taxis != NULL )
      EDIT_dset_items( new_dset , ADN_func_type , FUNC_FIM_TYPE , ADN_none ) ;
   else if( ISANATBUCKET(new_dset) ) /* 30 Nov 1997 */
      EDIT_dset_items( new_dset , ADN_func_type , ANAT_EPI_TYPE , ADN_none ) ;

   if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "*** Output file %s already exists -- cannot continue!\n",
              new_dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

   for (ids=0; ids<26; ids++)
      atoz[ids] = (double *) malloc(sizeof(double) * VSIZE ) ;
   for( ids=0 ; ids < 26 ; ids++ )
      for (ii=0; ii<VSIZE; ii++)
	 atoz[ids][ii] = 0.0 ;

   /*** loop over time steps ***/

   nx  =      DSET_NX(new_dset) ;
   nxy = nx * DSET_NY(new_dset) ; daxes = new_dset->daxes ;

   buf = (float **) malloc(sizeof(float *) * ntime_max);

   for ( kt = 0 ; kt < ntime_max ; kt ++ ) {

      if( CALC_verbose )
         fprintf(stderr,"  ++ Computing sub-brick %d\n",kt) ;

      /* 30 April 1998: only malloc output space as it is needed */

      buf[kt] = (float *) malloc(sizeof(float) * CALC_nvox);
      if( buf[kt] == NULL ){
         fprintf(stderr,"Can't malloc output dataset sub-brick %d!\n",kt) ;
         exit(1) ;
      }

      /*** loop over voxels ***/

      for ( ii = 0 ; ii < CALC_nvox ; ii += VSIZE ) {

	 jbot = ii ;
	 jtop = MIN( ii + VSIZE , CALC_nvox ) ;

	 for (ids = 0 ; ids < 26 ; ids ++ ) {

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

            /* the case of a 3D dataset (i.e., only 1 sub-brick) */

            else if ( ntime[ids] == 1 && CALC_type[ids] >= 0 ) {
	       switch( CALC_type[ids] ) {
		  case MRI_short:
		     for (jj =jbot ; jj < jtop ; jj ++ ){
			atoz[ids][jj-ii] = CALC_short[ids][0][jj] * CALC_ffac[ids][0] ;
                     }
		  break;

                  case MRI_float:
                     for (jj =jbot ; jj < jtop ; jj ++ ){
                        atoz[ids][jj-ii] = CALC_float[ids][0][jj] * CALC_ffac[ids][0] ;
                     }
                  break;

                  case MRI_byte:
                     for (jj =jbot ; jj < jtop ; jj ++ ){
                        atoz[ids][jj-ii] = CALC_byte[ids][0][jj] * CALC_ffac[ids][0] ;
                     }
                  break;
	      }	
	   }

           /* the case of a 3D+time dataset (or a bucket, etc.) */

	   else if( ntime[ids] > 1 && CALC_type[ids] >= 0 ) {
	      switch ( CALC_type[ids] ) {
	         case MRI_short:
		    for (jj = jbot ; jj < jtop ; jj ++ ) {
		       atoz[ids][jj-ii] = CALC_short[ids][kt][jj] * CALC_ffac[ids][kt];
		    }
		 break;

                 case MRI_float:
                    for (jj = jbot ; jj < jtop ; jj ++ ){
                       atoz[ids][jj-ii] = CALC_float[ids][kt][jj] * CALC_ffac[ids][kt];
                    }
                 break;

                 case MRI_byte:
                    for (jj = jbot ; jj < jtop ; jj ++ ){
                       atoz[ids][jj-ii] = CALC_byte[ids][kt][jj] * CALC_ffac[ids][kt];
                    }
                 break;
	       }
             }

           /* the case of a voxel (x,y,z) coordinate */

           else if( ids >= 23 ){

              switch( ids ){
                 case 23:     /* x */
                    for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = daxes->xxorg + (jj%nx) * daxes->xxdel ;
                 break ;

                 case 24:     /* y */
                    for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = daxes->yyorg + ((jj%nxy)/nx) * daxes->yydel ;
                 break ;

                 case 25:     /* z */
                    for( jj=jbot ; jj < jtop ; jj++ )
                       atoz[ids][jj-ii] = daxes->zzorg + (jj/nxy) * daxes->zzdel ;
                 break ;
               }
	     } /* end of choice over data type switch */
	    } /* end of loop over datasets */

            /**** actually do the work! ****/

            PARSER_evaluate_vector(CALC_code, atoz, jtop-jbot, temp);
	    for ( jj = jbot ; jj < jtop ; jj ++ )
	       buf[kt][jj] = temp[jj-ii];

         } /* end of loop over space */

         /* 30 April 1998: purge 3D+time sub-bricks if possible */

	 for( ids=0 ; ids < 26 ; ids ++ ){
            if( CALC_dset[ids] != NULL && ntime[ids] > 1 &&
                CALC_dset[ids]->dblk->malloc_type == DATABLOCK_MEM_MALLOC ){

               void * ptr = DSET_ARRAY(CALC_dset[ids],kt) ;
               if( ptr != NULL ) free(ptr) ;
               mri_clear_data_pointer( DSET_BRICK(CALC_dset[ids],kt) ) ;
            }
         }

   } /* end of loop over time steps */

   for( ids=0 ; ids < 26 ; ids++ ){
      if( CALC_dset[ids] != NULL ) PURGE_DSET( CALC_dset[ids] ) ;
      if( TS_flim[ids]   != NULL ) mri_free( TS_flim[ids] ) ;
   }

   /*** attach new data to output brick ***/

   switch( CALC_datum ){

      default:
         fprintf(stderr,
                 "*** Fatal Error ***\n"
                 "*** Somehow ended up with CALC_datum = %d\n",CALC_datum) ;
      exit(1) ;

      case MRI_float:{
         for (ii=0; ii< ntime_max; ii++) {
	    EDIT_substitute_brick(new_dset, ii, MRI_float, buf[ii]);
	    DSET_BRICK_FACTOR(new_dset, ii) = 0.0;
         }
      }
      break ;

      case MRI_byte:             /* modified 31 Mar 1999 to scale each sub-brick  */
      case MRI_short:{           /* with its own factor, rather than use the same */
         void ** dfim ;          /* factor for each sub-brick -- RWCox            */
         float gtop , fimfac , gtemp ;

         if( CALC_verbose )
            fprintf(stderr,"  ++ Scaling output to type %s brick(s)\n",
                    MRI_TYPE_name[CALC_datum] ) ;

	 dfim = (void ** ) malloc( sizeof( void * ) * ntime_max ) ;

         if( CALC_gscale ){   /* 01 Apr 1999: allow global scaling */
            gtop = 0.0 ;
            for( ii=0 ; ii < ntime_max ; ii++ ){
               gtemp = MCW_vol_amax( CALC_nvox , 1 , 1 , MRI_float, buf[ii] ) ;
               gtop  = MAX( gtop , gtemp ) ;
               if( gtemp == 0.0 )
                  fprintf(stderr,"  ** Warning: output sub-brick %d is all zeros!\n",ii) ;
            }
         }

	 for (ii = 0 ; ii < ntime_max ; ii ++ ) {

            if( ! CALC_gscale ){
	       gtop = MCW_vol_amax( CALC_nvox , 1 , 1 , MRI_float, buf[ii] ) ;
               if( gtop == 0.0 )
                  fprintf(stderr,"  ** Warning: output sub-brick %d is all zeros!\n",ii) ;
            }

            if( CALC_fscale ){   /* 16 Mar 1998 */
               fimfac = (gtop > 0.0) ? MRI_TYPE_maxval[CALC_datum] / gtop : 0.0 ;
            } else {
               fimfac = (gtop > MRI_TYPE_maxval[CALC_datum] || (gtop > 0.0 && gtop <= 1.0) )
                        ? MRI_TYPE_maxval[CALC_datum]/ gtop : 0.0 ;
            }

            if( CALC_verbose ){
               if( fimfac != 0.0 )
                  fprintf(stderr,"  ++ Sub-brick %d scale factor = %f\n",ii,fimfac) ;
               else
                  fprintf(stderr,"  ++ Sub-brick %d: no scale factor\n" ,ii) ;
            }

            dfim[ii] = (void *) malloc( mri_datum_size(CALC_datum) * CALC_nvox ) ;
            if( dfim[ii] == NULL ){ fprintf(stderr,"malloc fails at output\n");exit(1); }

	    EDIT_coerce_scale_type( CALC_nvox , fimfac ,
                                    MRI_float, buf[ii] , CALC_datum,dfim[ii] ) ;
	    free( buf[ii] ) ;
	    EDIT_substitute_brick(new_dset, ii, CALC_datum, dfim[ii] );

            DSET_BRICK_FACTOR(new_dset,ii) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
	 }
      }
      break ;
   }

   if( CALC_verbose )
      fprintf(stderr,"  ++ Computing output statistics\n") ;
   THD_load_statistics( new_dset ) ;

   if( CALC_verbose )
      fprintf(stderr,"  ++ Writing output to disk\n") ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

   exit(0) ;
}
