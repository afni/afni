
#include "mrilib.h"

#define FFT_ABS     1
#define FFT_PHASE   2
#define FFT_COMPLEX 3

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_in , *dset_out ;
   int Lxx=-1 , Lyy=-1 , Lzz=-1 , Mode=FFT_ABS ;
   char *prefix = "FFT" ;
   int iarg ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dFFT [options] dataset\n"
       "Does the FFT of the input dataset in 3 directions (x,y,z) and\n"
       "produces the output dataset.\n"
       "\n"
       "Options\n"
       "=======\n"
       " -abs       = Outputs the magnitude of the FFT [default]\n"
       " -phase     = Outputs the phase of the FFT (-PI..PI)\n"
       " -complex   = Outputs the complex FFT\n"
       " -Lx xx     = Use FFT of length 'xx' in the x-direction\n"
       " -Ly yy     = Use FFT of length 'yy' in the y-direction\n"
       " -Lz zz     = Use FFT of length 'zz' in the z-direction\n"
       "              * Set a length to 0 to skip\n"
       "                the FFT in that direction\n"
       " -prefix pp = Use 'pp' for the output dataset prefix.\n"
       "\n"
       "Notes\n"
       "=====\n"
       " * The program can only do FFT lengths that are factorable\n"
       "    into powers of 2, 3, and 5.\n"
       " * For -abs and -phase, the output dataset is in float format.\n"
     ) ;
     exit(0) ;
   }

   mainENTRY("3dFFT main") ; machdep() ;

   /*--- scan args ---*/

   iarg = 1 ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-abs") == 0 ){
       Mode = FFT_ABS ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-phase") == 0 ){
       Mode = FFT_PHASE ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-complex") == 0 ){
       Mode = FFT_COMPLEX ; iarg++ ; continue ;
     }

     if( strlen(argv[iarg]) == 3 && strncmp(argv[iarg],"-L",2) == 0 ){
       int lll=-1 ; char *ept ;
       iarg++ ;
       if( iarg >= argc ){
         fprintf(stderr,"** ERROR: need an argument after %s\n",argv[iarg-1]) ;
         exit(1) ;
       }
       lll = strtol( argv[iarg] , &ept , 10 ) ;
       if( *ept != '\0' ){
         fprintf(stderr,"** ERROR: bad argument after %s\n",argv[iarg-1]) ;
         exit(1) ;
       }
       switch( argv[iarg-1][2] ){
         case 'x': Lxx = lll ; break ;
         case 'y': Lyy = lll ; break ;
         case 'z': Lzz = lll ; break ;
         default:
           fprintf(stderr,"** ERROR: unknown option '%s'\n",argv[iarg-1]) ;
           exit(1) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       iarg++ ;
       if( iarg >= argc ){
         fprintf(stderr,"** ERROR: need an argument after %s\n",argv[iarg-1]) ;
         exit(1) ;
       }
       prefix = strdup( argv[iarg] ) ;
       if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** ERROR: bad argument after %s\n",argv[iarg-1]) ;
         exit(1) ;
       }
       iarg++ ; continue ;
     }

     fprintf(stderr,"** ERROR: unknown option '%s'\n",argv[iarg]) ;
     exit(1) ;
   }

   if( iarg >= argc ){
     fprintf(stderr,"** ERROR: no input dataset on command line?!\n") ;
     exit(1) ;
   }
   if( Lxx == 0 && Lyy == 0 && Lzz == 0 ){
     fprintf(stderr,"** ERROR: -Lx, -Ly, -Lz all given as zero?!\n") ;
     exit(1) ;
   }

   /* open input dataset */

   dset_in = THD_open_dataset( argv[iarg] ) ;
   if( dset_in == NULL ){
     fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[iarg]) ;
     exit(1) ;
   }
}
