/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <string.h>
#include "mrilib.h"

/*** global data ***/

#define MAX_NAME 64

static float FT_tval  = 32000.0 ;
static float FT_bval  = 0.0 ;
static float FT_sval  = 0.0 ;
static int   FT_start = 1 ;
static int   FT_step  = 1 ;
static int   FT_nsize = 0 ;

static char FT_pname[MAX_NAME] = "sh." ;
static char FT_sname[MAX_NAME] = "\0" ;

static MRI_IMARR * FT_imts = NULL ;

/*** prototypes ***/

void FTOSH_syntax( char * ) ;
void FTOSH_getopts( int , char * argv[] ) ;

/*** actual program code ***/

int main( int argc , char *argv[] )
{
   int ii , nx , ny , npix , kk ;
   MRI_IMAGE * tim , * sim ;
   float     * tar ;
   short     * sar ;
   char name[256] ;

   /*----- read inputs -----*/

   printf(
     "ftosh: convert float images to shorts, by RW Cox\n") ;

   machdep() ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) FTOSH_syntax(NULL) ;

   FTOSH_getopts( argc , argv ) ;

   /*----- convert and write each image -----*/

   for( kk=0 ; kk < FT_imts->num ; kk++ ){
      tim = FT_imts->imarr[kk] ; tar = mri_data_pointer( tim ) ;
      nx  = tim->nx ; ny = tim->ny ; npix = nx*ny ;
      sim = mri_new( nx , ny , MRI_short ) ; sar = mri_data_pointer( sim ) ;

      for( ii=0 ; ii < npix ; ii++ )
         sar[ii] = (short) ( FT_sval * ( tar[ii] - FT_bval ) + 0.499 ) ;

      mri_free( tim ) ;
      if( FT_nsize ){
         tim = mri_nsize( sim ) ; mri_free( sim ) ; sim = tim ;
      }

      if( strlen(FT_pname) > 0 && strlen(FT_sname) > 0 ){
         sprintf( name , "%s%04d.%s" ,
                  FT_pname , FT_start + kk*FT_step , FT_sname ) ;
      } else if( strlen(FT_pname) > 0 ){
         sprintf( name , "%s%04d" , FT_pname , FT_start + kk*FT_step ) ;
      } else if( strlen(FT_sname) > 0 ){
         sprintf( name , "%04d.%s" , FT_start + kk*FT_step , FT_sname ) ;
      } else {
         sprintf( name , "%04d" , FT_start + kk*FT_step ) ;
      }

      printf("-- writing %dx%d image %s\n",nx,ny,name) ;
      mri_write( name , sim ) ;
      mri_free( sim ) ;
   }

   exit(0) ;
}

/*----------------------------------------------------------------------*/

void FTOSH_getopts( int argc , char *argv[] )
{
   int nopt = 1 , kk , nx,ny , ii,npix ;
   MRI_IMAGE * tim ;
   float     * tar ;
   float       bmax , bdif ;

   /*---- scan options that start with a - -----*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      /** -prefix pname **/

      if( strncmp(argv[nopt],"-prefix",5) == 0 ){
         if( ++nopt >= argc ) FTOSH_syntax("-prefix needs a name!") ;
         strcpy( FT_pname , argv[nopt] ) ;
         kk = strlen(FT_pname) ;
         if( kk > 0 && FT_pname[kk-1] != '.' ){
            FT_pname[kk]   = '.' ;
            FT_pname[kk+1] = '\0' ;
         }
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -suffix pname **/

      if( strncmp(argv[nopt],"-suffix",5) == 0 ){
         if( ++nopt >= argc ) FTOSH_syntax("-suffix needs a name!") ;
         strcpy( FT_sname , argv[nopt] ) ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -scale sval **/

      if( strncmp(argv[nopt],"-scale",5) == 0 ){
         char * ch ;
         if( ++nopt >= argc ) FTOSH_syntax("-scale needs a value!");
         FT_sval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' || FT_sval == 0.0 )
            FTOSH_syntax("value after -scale is illegal!") ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -base bval **/

      if( strncmp(argv[nopt],"-base",5) == 0 ){
         char * ch ;
         if( ++nopt >= argc ) FTOSH_syntax("-base needs a value!");
         FT_bval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' ) FTOSH_syntax("value after -base is illegal!") ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -top tval **/

      if( strncmp(argv[nopt],"-top",4) == 0 ){
         char * ch ;
         if( ++nopt >= argc ) FTOSH_syntax("-top needs a value!");
         FT_tval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' || FT_tval == 0.0 )
            FTOSH_syntax("value after -top is illegal!") ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -start si **/

      if( strncmp(argv[nopt],"-start",5) == 0 ){
         char * ch ;
         if( ++nopt >= argc ) FTOSH_syntax("-start needs a value!");
         FT_start = strtol( argv[nopt] , &ch , 10 ) ;
         if( *ch != '\0' || FT_start < 0 )
            FTOSH_syntax("value after -start is illegal!") ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -step ss **/

      if( strncmp(argv[nopt],"-step",5) == 0 ){
         char * ch ;
         if( ++nopt >= argc ) FTOSH_syntax("-step needs a value!");
         FT_step = strtol( argv[nopt] , &ch , 10 ) ;
         if( *ch != '\0' || FT_step < 0 )
            FTOSH_syntax("value after -step is illegal!") ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -nsize **/

      if( strncmp(argv[nopt],"-nsize",5) == 0 ){
         FT_nsize = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** illegal option **/

      fprintf(stderr,"*** illegal command line option: %s\n",argv[nopt]) ;
      FTOSH_syntax("type ftosh -help for more details") ;

   }

   /*----- rest of inputs are image files -----*/

   FT_imts = mri_read_many_files( argc-nopt , argv+nopt ) ;
   if( FT_imts == NULL ) FTOSH_syntax("cannot continue without input images!") ;

#if 0
   /* check images for consistency */

   nx = FT_imts->imarr[0]->nx ;
   ny = FT_imts->imarr[0]->ny ;

   for( kk=1 ; kk < FT_imts->num ; kk++ ){
      if( nx != FT_imts->imarr[kk]->nx || ny != FT_imts->imarr[kk]->ny ){
         fprintf(stderr,"*** image %d not conformant to image 0\n",kk) ;
         FTOSH_syntax("cannot continue with images whose sizes differ!") ;
      }
   }
#endif

   /* convert all input images to floats, if needed; ALSO, if   */
   /* FT_sval is zero, must find the maximum abs(input-FT_bval) */

   bmax = 0.0 ;
   for( kk=0 ; kk < FT_imts->num ; kk++ ){

      if( FT_imts->imarr[kk]->kind != MRI_float ){
         tim = mri_to_float( FT_imts->imarr[kk] ) ;
         mri_free( FT_imts->imarr[kk] ) ;
         FT_imts->imarr[kk] = tim ;
      }

      if( FT_sval == 0.0 ){
         tim = FT_imts->imarr[kk] ; tar = mri_data_pointer( tim ) ;
         nx  = tim->nx ; ny = tim->ny ; npix = nx*ny ;
         for( ii=0 ; ii < npix ; ii++ ){
            bdif = fabs( tar[ii]-FT_bval ) ;
            if( bmax < bdif ) bmax = bdif ;
         }
      }
   }

   if( FT_sval == 0.0 ){
      if( bmax == 0.0 ){
         fprintf(stderr,"*** all input images are == %g\n",FT_bval) ;
         FTOSH_syntax("can't autoscale them!") ;
      }
      FT_sval = FT_tval / bmax ;
      printf("** max abs(input-%g) = %g --> scale factor = %g/%g = %g\n",
             FT_bval , bmax , FT_tval,bmax , FT_sval ) ;
   }

   return ;
}

/*----------------------------------------------------------------------*/

void FTOSH_syntax( char * str )
{
   if( str != NULL ){
      fprintf(stderr,"*** %s\a\n",str) ;
      exit(-1) ;
   }

   printf(
    "Usage: ftosh [options] image_files ...\n"
    "\n"
    " where the image_files are in the same format to3d accepts\n"
    " and where the options are\n"
    "\n"
    "  -prefix pname:  The output files will be named in the format\n"
    "  -suffix sname:  'pname.index.sname' where 'pname' and 'sname'\n"
    "  -start  si:     are strings given by the first 2 options.\n"
    "  -step   ss:     'index' is a number, given by 'si+(i-1)*ss'\n"
    "                  for the i-th output file, for i=1,2,...\n"
    "              *** Default pname = 'sh'\n"
    "              *** Default sname = nothing at all\n"
    "              *** Default si    = 1\n"
    "              *** Default ss    = 1\n"
    "\n"
    "  -nsize:         Enforce the 'normal size' option, to make\n"
    "                  the output images 64x64, 128x128, or 256x256.\n"
    "\n"
    "  -scale sval:    'sval' and 'bval' are numeric values; if\n"
    "  -base  bval:    sval is given, then the output images are\n"
    "  -top   tval:    formed by scaling the inputs by the formula\n"
    "                  'output = sval*(input-bval)'.\n"
    "              *** Default sval is determined by finding\n"
    "                  V = largest abs(input-bval) in all the input\n"
    "                  images and then sval = tval / V.\n"
    "              *** Default tval is 32000; note that tval is only\n"
    "                  used if sval is not given on the command line.\n"
    "              *** Default bval is 0.\n"
   ) ;
   exit(0) ;
}
