/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/
/*---------------------------------------------------------------------------
This program is revised for 3d+time data calculation,
  [Raoqiong Tong, August 1997]
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
static int 		  ntime[26] ;
static int 		  ntime_max = 0 ;
static int                CALC_fscale = 0 ;  /* 16 Mar 1998 */

static THD_3dim_dataset *  CALC_dset[26] ;
static int                 CALC_type[26] ;
static byte **             CALC_byte[26] ;
static short **            CALC_short[26] ;
static float **            CALC_float[26] ;
static float *             CALC_ffac[26] ;

static char CALC_output_prefix[THD_MAX_PREFIX] = "calc" ;

static char CALC_session[THD_MAX_NAME]         = "./"   ;


/*--------------------------- prototypes ---------------------------*/
void CALC_read_opts( int , char ** ) ;
void CALC_Syntax(void) ;

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

      /**** -fscale [16 Mar 1998] ****/

      if( strncmp(argv[nopt],"-fscale",6) == 0 ){
         CALC_fscale = 1 ;
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

         int ival , nxyz , isub ;
         THD_3dim_dataset * dset ;

         ival = argv[nopt][1] - 'a' ;
         if( CALC_dset[ival] != NULL ){
            fprintf(stderr,"can't open duplicate %s datasets!\n",argv[nopt]) ;
            exit(1) ;
         }

         isub = (ids == 2) ? 0 : strtol(argv[nopt]+2,NULL,10) ;

         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after %s!\n",argv[nopt-1]) ; exit(1) ;
         }

         dset = THD_open_one_dataset( argv[nopt++] ) ;
         if( dset == NULL ){
            fprintf(stderr,"can't open dataset %s\n",argv[nopt-1]) ; exit(1) ;
         }

         THD_load_datablock( dset->dblk , NULL ) ;
         ntime[ival] = DSET_NUM_TIMES(dset);

	 if ( ids > 2 ) ntime[ival] = 1 ;
	 ntime_max = MAX( ntime_max, ntime[ival] );

	 for (ii=0; ii<ntime[ival]; ii++)
            if( DSET_ARRAY(dset,ii) == NULL ){
               fprintf(stderr,"can't read data brick for dataset %s\n",argv[nopt-1]) ;
               exit(1) ;
            }

         if( isub >= DSET_NVALS(dset) ){
            fprintf(stderr,"dataset %s only has %d sub-bricks\n",
                    argv[nopt-1],DSET_NVALS(dset)) ;
            exit(1) ;
         }

         nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
         if( CALC_nvox < 0 ){
            CALC_nvox = nxyz ;
         } else if( nxyz != CALC_nvox ){
            fprintf(stderr,"dataset %s differs in size from others\n",argv[nopt-1]);
            exit(1) ;
         }

         CALC_type[ival] = DSET_BRICK_TYPE(dset,isub) ;
	 CALC_dset[ival] = dset ;

         CALC_ffac[ival] = (float * ) malloc( sizeof(float) * ntime[ival] ) ;
         if ( ntime[ival] == 1 ) {
            CALC_ffac[ival][0] = DSET_BRICK_FACTOR( dset , isub) ;
            if (CALC_ffac[ival][0] == 0.0 ) CALC_ffac[ival][0] = 1.0 ;
         }
         else for (ii = 0 ; ii < ntime[ival] ; ii ++ ) {
            CALC_ffac[ival][ii] = DSET_BRICK_FACTOR(dset, ii) ;
            if (CALC_ffac[ival][ii] == 0.0 ) CALC_ffac[ival][ii] = 1.0;
         }

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

	 } /* end of loop over type switch */
         if( CALC_datum < 0 ) CALC_datum = CALC_type[ival] ;
	 continue;

      } /* end of loop over data input */

      fprintf(stderr,"Unknown option: %s\n",argv[nopt]) ; exit(1) ;

   }  /* end of loop over options */

   /*** cleanup ***/

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
	 fprintf(stderr, "3D+time datasets don't match!\n") ; exit(1) ;
      }

   return ;
}

/*------------------------------------------------------------------*/

void CALC_Syntax(void)
{
   printf(
    "Do arithmetic on 3D datasets, voxel-by-voxel.\n"
    "Usage: 3dcalc [options]\n"
    "where the options are:\n"
   ) ;

   printf(
    "  -datum type = Coerce the output data to be stored as the given type,\n"
    "                  which may be byte, short, or float.\n"
    "                  [default = datum of first input dataset]\n"
    "  -fscale     = Force scaling of the output to the maximum integer\n"
    "                  range.  This only has effect if the output datum\n"
    "                  is byte or short (either forced or defaulted).\n"
    "                  [default is to scale only if the computed values\n"
    "                   seem to need it -- are all less than 1 or have\n"
    "                   some value beyond the integer upper limit]\n"
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
    "             ** The type and number of sub-bricks in a dataset can be\n"
    "                  printed out using the '3dinfo' program.\n"
    "  -expr \"expression\"\n"
    "                Apply the expression within quotes to the input datasets,\n"
    "                one voxel at a time, to produce the output dataset.\n"
    "                (\"sqrt(a*b)\" to compute the geometric mean, for example).\n"
    "  -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    "                    [default='calc']\n"
    "  -session dir  = Use 'dir' for the output dataset session directory.\n"
    "                    [default='./'=current working directory]\n"
    "\n"
    "3D+time Datasets:\n"
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
    "   rect , step , astep, bool  , and   , or    , mofn  ,\n"
    " where qg(x)    = reversed cdf of a standard normal distribution\n"
    "       qginv(x) = inverse function to qg,\n"
    "       min, max, atan2 each take 2 arguments,\n"
    "       J0, J1, Y0, Y1 are Bessel functions (see Watson),\n"
    "       erf, erfc are the error and complementary error functions.\n"
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
    " All computations are carried out in double precision before being\n"
    " truncated to the final output 'datum'.\n"
    "\n"
    " Note that the quotes around the expression are needed so the shell\n"
    " doesn't try to expand * characters, or interpret parentheses.\n"
    "\n"
    " (Try the 'ccalc' program to see how the expression evaluator works.\n"
    "  The arithmetic parser and evaluator is written in Fortran-77 and\n"
    "  is derived from a program written long ago by RWCox to facilitate\n"
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
   int ii , ids , jj, kk, kt, ll, jbot, jtop;
   THD_3dim_dataset * new_dset=NULL ;
   float ** buf;
   double   temp[VSIZE];

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) CALC_Syntax() ;

   for (ii=0; ii<26; ii++) ntime[ii] = 0 ;

   CALC_read_opts( argc , argv ) ;

   buf = (float **) malloc(sizeof(float *) * ntime_max);
   for (ii=0; ii<ntime_max; ii++){
      buf[ii]=(float *)malloc(sizeof(float) * CALC_nvox);
      if( buf[ii] == NULL ){
         fprintf(stderr,"Can't malloc output dataset space!\n") ;
         exit(1) ;
      }
   }

   /*** make output dataset ***/

   if( ntime_max == 1 ){
      for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL ) break ;
   } else {
      for( ids=0 ; ids < 26 ; ids++ ) if( CALC_dset[ids] != NULL &&
                                          ntime[ids] > 1           ) break ;
   }
   if( ids == 26 ){fprintf(stderr,"Can't find template dataset?!\n");exit(1);}

   new_dset = EDIT_empty_copy( CALC_dset[ids] ) ;

   EDIT_dset_items( new_dset ,
                       ADN_prefix         , CALC_output_prefix ,
                       ADN_directory_name , CALC_session ,
                       ADN_datum_all      , CALC_datum ,
                       ADN_nvals          , ntime_max ,
                    ADN_none ) ;

   if( ISFUNC(new_dset) )
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
   for ( kt = 0 ; kt < ntime_max ; kt ++ ) {

      /*** loop over voxels ***/
      for ( ii = 0 ; ii < CALC_nvox ; ii += VSIZE ) {

	 jbot = ii ;
	 jtop = MIN( ii + VSIZE , CALC_nvox ) ;

	 for (ids = 0 ; ids < 26 ; ids ++ ) {
	    if ( ntime[ids] == 1 ) {
	       switch( CALC_type[ids] ) {
		  case MRI_short:
		     for (jj =jbot ; jj < jtop ; jj ++ )
			atoz[ids][jj-ii] = CALC_short[ids][0][jj] * CALC_ffac[ids][0] ;
		  break;

                  case MRI_float:
                     for (jj =jbot ; jj < jtop ; jj ++ )
                        atoz[ids][jj-ii] = CALC_float[ids][0][jj] * CALC_ffac[ids][0] ;
                  break;

                  case MRI_byte:
                     for (jj =jbot ; jj < jtop ; jj ++ )
                        atoz[ids][jj-ii] = CALC_byte[ids][0][jj] * CALC_ffac[ids][0] ;
                  break;
	      }	
	   }
	   else {
	      switch ( CALC_type[ids] ) {
	         case MRI_short:
		    for (jj = jbot ; jj < jtop ; jj ++ ) {
		       atoz[ids][jj-ii] = CALC_short[ids][kt][jj] * CALC_ffac[ids][kt];
		    }
		 break;

                 case MRI_float:
                    for (jj = jbot ; jj < jtop ; jj ++ )
                       atoz[ids][jj-ii] = CALC_float[ids][kt][jj] * CALC_ffac[ids][kt];
                 break;

                 case MRI_byte:
                    for (jj = jbot ; jj < jtop ; jj ++ )
                       atoz[ids][jj-ii] = CALC_byte[ids][kt][jj] * CALC_ffac[ids][kt];
                 break;
	       }
	      } /* end of loop over data type switch */
	    } /* end of loop over datasets */

            PARSER_evaluate_vector(CALC_code, atoz, jtop-jbot, temp);
	    for ( jj = jbot ; jj < jtop ; jj ++ )
	       buf[kt][jj] = temp[jj-ii];

         } /* end of loop over space */
      } /* end of loop over time steps */


   for( ids=0 ; ids < 26 ; ids++ )
      if( CALC_dset[ids] != NULL ) PURGE_DSET( CALC_dset[ids] ) ;

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

      case MRI_byte:
      case MRI_short:{
         void ** dfim ;
         float gtop , fimfac , gtemp ;

	 gtop = 0.0 ;
	 for ( ii = 0 ; ii < ntime_max ; ii ++ ) {
	    gtemp = MCW_vol_amax( CALC_nvox , 1 , 1 , MRI_float, buf[ii] ) ;
	    gtop  = MAX( gtop , gtemp ) ;
	 }

         if( gtop == 0.0 ) fprintf(stderr,"Warning: output is all zeros!\n") ;

         if( CALC_fscale ){   /* 16 Mar 1998 */
            fimfac = MRI_TYPE_maxval[CALC_datum]/ gtop ;
         } else {
            fimfac = (gtop > MRI_TYPE_maxval[CALC_datum] || (gtop > 0.0 && gtop <= 1.0) )
                     ? MRI_TYPE_maxval[CALC_datum]/ gtop : 0.0 ;
         }

	 dfim = (void ** ) malloc( sizeof( void * ) * ntime_max ) ;

	 for (ii = 0 ; ii < ntime_max ; ii ++ ) {

            dfim[ii] = (void *) malloc( mri_datum_size(CALC_datum) * CALC_nvox ) ;
            if( dfim[ii] == NULL ){ fprintf(stderr,"malloc fails at output\n");exit(1) ; }

	    EDIT_coerce_scale_type( CALC_nvox , fimfac ,
                                 MRI_float, buf[ii] , CALC_datum,dfim[ii] ) ;
	    EDIT_substitute_brick(new_dset, ii, CALC_datum, dfim[ii] );

            DSET_BRICK_FACTOR(new_dset,ii) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
	    free( buf[ii] ) ;
	 }
      }
      break ;
   }

   THD_load_statistics( new_dset ) ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

   exit(0) ;
}
