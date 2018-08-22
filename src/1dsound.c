#include "mrilib.h"

#include "cs_playsound.c"

void usage_1dsound(int detail)
{
   printf(
     "Usage: 1dsound [options] tsfile\n"
     "\n"
     "-------\n"
     "OPTIONS\n"
     "-------\n"
     " -prefix p  = Output filename\n"
   ) ;

   return;
}

/*---------------------------------------------------------------------------*/
/* This program is a very elaborate wrapper for the plot_ts.c functions. */
/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg ;
   char *prefix = "sound.au" ;
   MRI_IMAGE *inim , *phim ;
   float *far ;
   /*---------- startup bureaucracy ----------*/

#if 0
   mainENTRY("1dsound main"); machdep();
   PRINT_VERSION("1dsound"); AUTHOR("RWC et al.");
#endif

   /*------------ scan arguments that X11 didn't eat ------------*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-- help? --*/

     if(strcmp(argv[iarg],"-help") == 0 ||
        strcmp(argv[iarg],"-h")    == 0   ){
       usage_1dsound(strlen(argv[iarg])>3?2:1);
       exit(0) ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = strdup(argv[++iarg]) ; iarg++ ; continue ;
     }

     /*--- symplectically stoopid user ---*/

     ERROR_message("Unknown option: %s\n",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);

   } /*--------- end of scan over command line args ----------*/

   if( argc < 2 ){ usage_1dsound(0); exit(0) ; }

   if( iarg >= argc )
      ERROR_exit("No time series file on command line!\n") ;

   inim = mri_read_1D( argv[iarg] ) ;
   if( inim == NULL )
     ERROR_exit("Can't read input file '%s' iarg=%d\n",argv[iarg],iarg) ;

   phim = mri_sound_1D_to_FM( inim ,
                              0.0f , 0.0f , 8000 , 4000 ) ;

   if( phim == NULL )
     ERROR_exit("mri_sound_1D_to_FM fails") ;

   if( phim != NULL ){
     char fname[1024] ;
     sprintf(fname,"%s.au",prefix) ;
#if 0
     sound_write_au_ulaw( fname, phim->nx, MRI_FLOAT_PTR(phim), 8000, 0.05f ) ;
#else
     sound_write_au_8bitPCM( fname, phim->nx, MRI_FLOAT_PTR(phim), 8000, 0.05f ) ;
#endif
   }

   exit(0) ;
}
