/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"


/*------------------------------------------------------------------------
   Program to check (or write) the AFNI version information.
   -- RWCox, 10 January 2000
--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nbuf , verb=1 ;
   char *vbuf=NULL , vv[128]="none" ;

   /*-- for my use only: write out the new version file --*/

   if( argc > 1 && strcmp(argv[1],"-write") == 0 ){
     FILE *fp ;

     /* removed -write option     28 Dec 2015 [rickr] */
     fprintf(stderr,"** -write functionality was moved to dist scripts\n");
     return 1;

#if 0
     /* was: #define VERSION_FILE \
             "/Volumes/afni/var/www/html/pub/dist/AFNI.version" */

     fp = fopen(VERSION_FILE,"w") ;
     if( fp == NULL ){
       fprintf(stderr,"** Failed to open %s!\n",VERSION_FILE); EXIT(1);
     }
     fprintf( fp , "%s\n%s\n" , AVERZHN , "no date given" ) ;
     fclose(fp) ;
     fprintf(stderr,"Wrote out %s\n",VERSION_FILE) ;
     exit(0) ;
#endif
   }

   machdep() ;

   /*-- help the poor user? --*/

   if( argc > 1 && strcmp(argv[1],"-help") == 0 ){
     printf("Usage: afni_vcheck\n"
            "Overview ~1~\n"
            " Prints out the AFNI version with which it was compiled,\n"
            " and checks across the Web for the latest version available.\n"
            "N.B. ~1~\n"
            " Doing the check across the Web will mean that your\n"
            " computer's access to our server will be logged here.\n"
            " If you don't want this, don't use this program!\n" ) ;
     exit(0) ;
   }

   if( argc > 1 && strcmp(argv[1],"-q") == 0 ) verb = 0 ;

   /*-- internal information --*/

   if( verb ) printf("This program was compiled with the following settings:\n"
                     "  Version ID   = %s\n" , AVERZHN ) ;

   if( verb ) fprintf(stderr,"++ now fetching %s",AFNI_VERSION_URL) ;

   /*-- get information from the master computer --*/

   nbuf = read_URL( AFNI_VERSION_URL , &vbuf ) ;  /* see thd_http.c */
   if( verb ) fprintf(stderr,"\n") ;

   if( nbuf <= 0 || vbuf == NULL || vbuf[0] == '\0' ){
     if( verb ) printf("** Error fetching %s!\n",AFNI_VERSION_URL);
     exit(0);
   }

   sscanf( vbuf , "%127s" , vv ) ;

   if( verb ) printf("Latest version listed at AFNI web site:\n"
                     "  Version ID   = %s\n" , vv ) ;

   /* exit status is 0 if versions compare same, 1 if not the same */

   exit( (strcmp(vv,AVERZHN) != 0) ) ;
}
