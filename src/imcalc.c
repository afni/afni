/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 This software is Copyright 1997 by
     Medical College of Wisconsin
     8701 Watertown Plank Road
     Milwaukee, WI 53226
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "parser.h"
#include "mrilib.h"

/*-------------------------- global data --------------------------*/

static int                CALC_datum = -1 ;
static int                CALC_nvox  = -1 ;
static PARSER_code *      CALC_code  = NULL ;

static MRI_IMAGE * CALC_im[26] ;
static int         CALC_type[26] ;
static byte *      CALC_byte[26] ;
static short *     CALC_short[26] ;
static float *     CALC_float[26] ;

static char CALC_output_name[256] = "imcalc.out" ;

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
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported in 3dmerge!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -output name ****/

      if( strncmp(argv[nopt],"-output",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -output!\n") ; exit(1) ;
         }
         strncpy( CALC_output_name , argv[nopt++] , 255 ) ; CALC_output_name[255] = '\0' ;
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

      /**** -[a-z] filename ****/

      ids = strlen(argv[nopt]) ;

      if( (argv[nopt][1] >= 'a' && argv[nopt][1] <= 'z') && (ids == 2) ){

         int ival , nxyz ;
         MRI_IMAGE * im ;

         ival = argv[nopt][1] - 'a' ;
         if( CALC_im[ival] != NULL ){
            fprintf(stderr,"can't open duplicate %s images!\n",argv[nopt]) ;
            exit(1) ;
         }

         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after %s!\n",argv[nopt-1]) ; exit(1) ;
         }

         im = mri_read_just_one( argv[nopt++] ) ;
         if( im == NULL ){
            fprintf(stderr,"can't open image %s\n",argv[nopt-1]) ; exit(1) ;
         }

         nxyz = im->nvox ;
         if( CALC_nvox < 0 ){
            CALC_nvox = nxyz ;
         } else if( nxyz != CALC_nvox ){
            fprintf(stderr,"image %s differs in size from others\n",argv[nopt-1]);
            exit(1) ;
         }

         CALC_type[ival] = im->kind ;
         CALC_im[ival]   = im ;

         switch( CALC_type[ival] ){
            default: fprintf(stderr,"illegal datum in image %s\n",argv[nopt-1]);
                     exit(1) ;

            case MRI_short: CALC_short[ival] = MRI_SHORT_PTR(im) ; break ;
            case MRI_float: CALC_float[ival] = MRI_FLOAT_PTR(im) ; break ;
            case MRI_byte : CALC_byte [ival] = MRI_BYTE_PTR(im)  ; break ;
         }

         if( CALC_datum < 0 ) CALC_datum = CALC_type[ival] ;
         continue ;
      }

      fprintf(stderr,"Unknown option: %s\n",argv[nopt]) ; exit(1) ;

   }  /* end of loop over options */

   /*** cleanup ***/

   if( nopt < argc ){
     fprintf(stderr,"Extra command line arguments puzzle me! %s ...\n",argv[nopt]) ;
     exit(1) ;
   }

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_im[ids] != NULL ) break ;

   if( ids == 26 ){
      fprintf(stderr,"No input images given!\n") ; exit(1) ;
   }

   if( CALC_code == NULL ){
      fprintf(stderr,"No expression given!\n") ; exit(1) ;
   }

   return ;
}

/*------------------------------------------------------------------*/

void CALC_Syntax(void)
{
   printf(
    "Do arithmetic on 2D images, pixel-by-pixel.\n"
    "Usage: imcalc options\n"
    "where the options are:\n"
   ) ;

   printf(
    "  -datum type = Coerce the output data to be stored as the given type,\n"
    "                  which may be byte, short, or float.\n"
    "                  [default = datum of first input image]\n"
    "  -a dname    = Read image 'dname' and call the voxel values 'a'\n"
    "                  in the expression.  'a' may be any letter from 'a' to 'z'.\n"
    "               ** If some letter name is used in the expression, but not\n"
    "                  present in one of the image options here, then that\n"
    "                  variable is set to 0.\n"
    "  -expr \"expression\"\n"
    "                Apply the expression within quotes to the input images,\n"
    "                  one voxel at a time, to produce the output image.\n"
    "                  (\"sqrt(a*b)\" to compute the geometric mean, for example)\n"
    "  -output name = Use 'name' for the output image filename.\n"
    "                  [default='imcalc.out']\n"
    "\n"
    "PROBLEMS:\n"
    " ** Complex-valued images cannot be processed (byte, short, and float are OK).\n"
    " ** Evaluation of expressions is not very efficient.\n"
    "\n"
    "EXPRESSIONS:\n"
    " Arithmetic expressions are allowed, using + - * / ** and parentheses.\n"
    " As noted above, images are referred to by single letter variable names.\n"
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
    " functions, such as masking of images against some criterion:\n"
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
    " (Try the 'ccalc' program to see how the expression evaluator works.)\n"
   ) ;
   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   double atoz[26] ;
   int ii , ids ;
   MRI_IMAGE * new_im ;
   float * fnew ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) CALC_Syntax() ;

   CALC_read_opts( argc , argv ) ;

   /*** make output image (always float datum) ***/

   if( mri_filesize(CALC_output_name) > 0 ){
      fprintf(stderr,
              "*** Output file %s already exists -- cannot continue!\n",
              CALC_output_name ) ;
      exit(1) ;
   }

   for( ids=0 ; ids < 26 ; ids++ ) if( CALC_im[ids] != NULL ) break ;
   new_im = mri_new_conforming( CALC_im[ids] , MRI_float ) ;
   fnew   = MRI_FLOAT_PTR(new_im) ;

   for( ids=0 ; ids < 26 ; ids++ ) atoz[ids] = 0.0 ;

   /*** loop over voxels and compute result ***/

   for( ii=0 ; ii < CALC_nvox ; ii++ ){

      for( ids=0 ; ids < 26 ; ids++ ){
         switch( CALC_type[ids] ){
            case MRI_short: atoz[ids] = CALC_short[ids][ii] ; break;
            case MRI_float: atoz[ids] = CALC_float[ids][ii] ; break;
            case MRI_byte : atoz[ids] = CALC_byte [ids][ii] ; break;
         }
      }

      fnew[ii] = PARSER_evaluate_one( CALC_code , atoz ) ;
   }

   /*** toss input images ***/

   for( ids=0 ; ids < 26 ; ids++ )
      if( CALC_im[ids] != NULL ) mri_free( CALC_im[ids] ) ;

   /*** scale to output image, if needed ***/

   switch( CALC_datum ){

      case MRI_short:{
         float top ;
         MRI_IMAGE * shim ;

         top = mri_maxabs( new_im ) ;
         if( top < 32767.0 ) shim = mri_to_short( 1.0 , new_im ) ;
         else                shim = mri_to_short_scl( 0.0 , 10000.0 , new_im ) ;

         mri_free(new_im) ; new_im = shim ;
      }
      break ;

      case MRI_byte:{
         float top ;
         MRI_IMAGE * bim ;

         top = mri_maxabs( new_im ) ;
         if( top < 255.0 ) bim = mri_to_byte_scl( 1.0 ,   0.0 , new_im ) ;
         else              bim = mri_to_byte_scl( 0.0 , 255.0 , new_im ) ;

         mri_free(new_im) ; new_im = bim ;
      }
      break ;
   }

   /*** done ***/

   mri_write( CALC_output_name , new_im ) ;
   exit(0) ;
}
