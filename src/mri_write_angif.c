#include "mrilib.h"

static char *gfilt=NULL ;
static char *ganim=NULL ;
static int   setup=-1    ;

/*** N.B.: The code in this file has not been tested!!! -- RWCox ***/

/*-----------------------------------------------------------------
  Return 1 if animated gif output is setup, 0 if it can't be
-------------------------------------------------------------------*/

int setup_mri_write_angif( void )
{
   char *pq,*pg,*pgs,*pwg=NULL ;

   if( setup >= 0 ) return setup ;

   /*-- make the commands: gfilt to produce 1 gif
                           ganim to animate a bunch of them --*/

   pg = THD_find_executable( "ppmtogif" ) ;
   if( pg == NULL ){ setup = 0; return setup; }
   pq = THD_find_executable( "ppmquant" ) ;
   if( pq == NULL ){ setup = 0; return setup; }
   pgs = THD_find_executable( "gifsicle" ) ;
   if( pgs == NULL ){
     pwg = THD_find_executable( "whirlgif" ) ;
     if( pwg == NULL ){ setup = 0; return setup; }
   }

   gfilt = AFMALL(char, strlen(pg)+strlen(pq)+32) ;
   sprintf( gfilt , "%s 127 | %s > %%s" , pq,pg ) ;

   if( pgs != NULL ){
      ganim = AFMALL(char, strlen(pgs)+64) ;
      sprintf(ganim,"%s -d 10 -l -O1 -k 127 --method median-cut",pgs) ;
   } else {
      ganim = AFMALL(char, strlen(pwg)+64) ;
      sprintf(ganim,"%s -loop -time 10",pwg) ;
   }

   setup = 1; return setup;
}

/*-----------------------------------------------------------------
  Return 1 if write worked, 0 if not
-------------------------------------------------------------------*/

int mri_write_angif( char *fname , MRI_IMARR *imar )
{
   char gnam[32] , gt[16] , filt[512] ;
   MRI_IMAGE *tim , *qim ;
   int ii , nim ;
   FILE *fp ;

   /*-- sanity check --*/

   if( fname == NULL || fname[0] == '\0'      ||
       imar  == NULL || IMARR_COUNT(imar) < 2   ) return 0 ;

   if( setup <  0 ) setup_mri_write_angif() ;
   if( setup == 0 ) return 0 ;

   /*-- make individual gifs from each file --*/

   nim = IMARR_COUNT(imar) ;

   sprintf(gt,"%x",(unsigned int)PTOI(imar)) ; gt[5] = '\0' ;

   for( ii=0 ; ii < nim ; ii++ ){
      sprintf(gnam,"Elvis%s.%05d.gif",gt,ii) ;

      tim = IMARR_SUBIMAGE(imar,ii) ; if( tim == NULL ) continue ;
      qim = tim ;
      if( tim->kind != MRI_rgb ) qim = mri_to_rgb( tim ) ;

      sprintf( filt , gfilt , gnam ) ;
      fp = popen( filt , "w" ) ;
      if( fp == NULL ){
         fprintf(stderr,"** Can't open output filter %s\n",filt) ;
         return 0 ;
      }

      fprintf(fp,"P6\n%d %d\n255\n" , qim->nx,qim->ny ) ;
      fwrite( MRI_RGB_PTR(qim), sizeof(byte), 3*qim->nvox, fp ) ;
      pclose(fp) ;

      if( qim != tim ) mri_free(qim) ;
   }

   /*-- make the animated gif output --*/

   sprintf( filt , "%s Elvis.%s.*.gif > %s" , ganim , gt , fname ) ;
   system( filt ) ;

   /*-- remove individual gif files --*/

   for( ii=0 ; ii < nim ; ii++ ){
      sprintf(gnam,"Elvis%s.%05d.gif",gt,ii) ;
      remove(gnam) ;
   }

   return 1 ; /* indicate success */
}
