#include "afni.h"

#define VERSION_URL "http://141.106.106.221/~cox/AFNI.version"

#define VERSION_FILE "www/AFNI.version"

/*------------------------------------------------------------------------
   Program to check (or write) the AFNI version information.
   -- RWCox, 10 January 2000
--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int nbuf ;
   char * vbuf=NULL ,
          vv[128]="none" ,
          r1[128]="none" , r2[128]="\0" , r3[128] ="\0" ;

   /*-- for my use only: write out the new version file --*/

   if( argc == 2 && strcmp(argv[1],"-write") == 0 ){
      FILE * fp = fopen(VERSION_FILE,"w") ;
      if( fp == NULL ){
         fprintf(stderr,"** Failed to open %s!\n",VERSION_FILE); exit(1);
      }
      fprintf( fp , "%s\n%s\n" , VERSION , RELEASE ) ;
      fclose(fp) ;
      fprintf(stderr,"Wrote out %s\n",VERSION_FILE) ;
      exit(0) ;
   }

   /*-- otherwise, any options means the user needs help --*/

   if( argc > 1 ){
      printf("Usage: afni_version\n"
             " Prints out the AFNI version with which it was compiled,\n"
             " and checks across the Web for the latest version available.\n"
             "N.B.: Doing the check across the Web will mean that your\n"
             "      computer's access to our server will be logged here.\n"
             "      If you don't want this, don't use this program!\n" ) ;
      exit(0) ;
   }

   /*-- internal information --*/

   printf("This program was compiled with the following settings:\n"
          "  Version ID   = %s\n"
          "  Release date = %s\n" , VERSION , RELEASE ) ;

   fprintf(stderr,"++ now fetching %s",VERSION_URL) ;

   /*-- get information from the master computer --*/

   nbuf = read_URL( VERSION_URL , &vbuf ) ;  /* see thd_http.c */
   fprintf(stderr,"\n") ;

   if( nbuf <= 0 || vbuf == NULL || vbuf[0] == '\0' ){
      fprintf(stderr,"** Error in fetch!\n"); exit(1);
   }

   sscanf( vbuf , "%127s %127s %127s %127s" , vv , r1,r2,r3 ) ;

   printf("Latest version listed at AFNI web site:\n"
          "  Version ID   = %s\n"
          "  Release date = %s %s %s\n" , vv , r1,r2,r3 ) ;

   exit(0) ;
}
