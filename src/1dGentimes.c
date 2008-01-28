#include "mrilib.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

/*---------------------------------------------------------------------------*/

static float pick_one( int nn , float *ar )
{
   int ii ; float val ;

   while(1){
     ii = (int)( lrand48() % nn ) ;
     if( ar[ii] > 0.0f ){
       val = ar[ii] ; ar[ii] = -666.0f ; return val ;
     }
   }
}

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , ii,nzs ;
   MRI_IMAGE *gim=NULL,*tim ; float *gar ; int ngx,ngy , nphas=0 ; char *gnam ;
   float first=0.0f , s1,s2, *g1,*g2 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 1dGentimes [options]\n"
      "Generates timing files for -stim_times\n"
      "\n"
      "Options:\n"
      "=======\n"
      " -first ff  = Set first stimulus time to 'ff' seconds.\n"
      "                [Default=0]\n"
      " -1phase gg = Each stimulus has only 1 phase.  'gg' is\n"
      "                the file specifying the allowable gaps\n"
      "                between stimuli, which will be chosen\n"
      "                randomly without replacement.  This\n"
      "                gap file should have 1 row.\n"
      " -2phase gg = Each stimulus has 2 phases.  'gg' is the\n"
      "                gap file.  The first row is the gap\n"
      "                between phase #1 and phase #2.  The\n"
      "                second row is the gap between phase #2\n"
      "                and the next phase #1.\n"
      "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc ){

     if( strcmp(argv[iarg],"-first") == 0 ){
       if( ++iarg == argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       first = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-1phase") == 0 ){
       if( ++iarg == argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       if( gim    != NULL ) ERROR_exit("Can't specify 2 gap files!") ;
       tim = mri_read_1D( argv[iarg] ) ; gnam = argv[iarg] ;
       if( tim    == NULL ) ERROR_exit("Can't read gap file %s",argv[iarg]) ;
       gim = mri_transpose(tim) ; mri_free(tim) ;
       ngx = gim->nx ; ngy = gim->ny ; nphas = 1 ; gar = MRI_FLOAT_PTR(gim) ;
       if( ngy > 1 )        ERROR_exit("Gap file %s doesn't have exactly 1 row",argv[iarg]) ;
       if( ngx < 2 )        ERROR_exit("Gap file %s has too few columns",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-2phase") == 0 ){
       if( ++iarg == argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       if( gim    != NULL ) ERROR_exit("Can't specify 2 gap files!") ;
       tim = mri_read_1D( argv[iarg] ) ; gnam = argv[iarg] ;
       if( tim    == NULL ) ERROR_exit("Can't read gap file %s",argv[iarg]) ;
       gim = mri_transpose(tim) ; mri_free(tim) ;
       ngx = gim->nx ; ngy = gim->ny ; nphas = 2 ; gar = MRI_FLOAT_PTR(gim) ;
       if( ngy != 2 )       ERROR_exit("Gap file %s doesn't have exactly 2 rows",argv[iarg]) ;
       if( ngx < 2 )        ERROR_exit("Gap file %s has too few columns",argv[iarg]) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: '%s'",argv[iarg]) ;
   }

   /*-- check for errors --*/

   if( gim == NULL ) ERROR_exit("No -1phase or -2phase option given!") ;

   for( nzs=ii=0 ; ii < gim->nvox ; ii++ ) if( gar[ii] <= 0.0f ) nzs++ ;
   if( nzs > 0 ) ERROR_exit("Found %d non-positive values in gap file %s",nzs,gnam) ;

   /*-- do the work --*/

   srand48((long)time(NULL)+(long)getpid());

   s1 = first ;
   switch( nphas ){
     default: ERROR_exit("WTF?") ;

     case 1:
       g1 = gar ;
       printf("%g\n",s1) ;
       for( ii=0 ; ii < ngx-1 ; ii++ ){
         s1 += pick_one(ngx,g1) ;
         printf("%g\n",s1) ;
       }
     break ;

     case 2:
       g1 = gar ; g2 = gar + ngx ;
       s1 = first ; s2 = s1 + pick_one(ngx,g1) ;
       printf("%g  %g\n",s1,s2) ;
       for( ii=0 ; ii < ngx-1 ; ii++ ){
         s1 = s2 + pick_one(ngx,g2) ;
         s2 = s1 + pick_one(ngx,g1) ;
         printf("%g  %g\n",s1,s2) ;
       }
     break ;
   }

   exit(0) ;
}
