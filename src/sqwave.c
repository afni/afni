/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*** program SQWAVE:
     generate a file of a square wave time series
***/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(argc, argv)
     int argc;
     char **argv;
{
   int narg ;
   int num_on=0 , num_off=0 , num_len=0 , num_cycles=0 , num_init=0 ,
       num_on_kill=0 , num_off_kill=0 , num_init_kill=0 ;
   static char fname[128] = "REF.ts" ;
   FILE *sqfile ;
   int ii , nout ;

/*** process switches ***/

#define ERROR {printf("FatalError\n");exit(1);}

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){
      fprintf( stderr ,
    "Usage: %s [-on #] [-off #] [-length #] [-cycles #]\n" , argv[0] ) ;
      fprintf( stderr ,
    "      [-init #] [-onkill #] [-offkill #] [-initkill #] [-name name]\n");
      ERROR ;
   }

   narg = 1 ;
   do {

      if( strncmp(argv[narg],"-initkill",7) == 0 ){
         num_init_kill = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_init_kill < 0 ){
            fprintf(stderr,"num_init_kill = %d\n",num_init_kill) ;
            ERROR ;
         }
         continue ;
      }

      if( strncmp(argv[narg],"-onkill",5) == 0 ||
          strncmp(argv[narg],"-killon",7) == 0   ){

         num_on_kill = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_on_kill < 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-offkill",6) == 0 ||
          strncmp(argv[narg],"-killoff",8) == 0   ){

         num_off_kill = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_off_kill < 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-on",3) == 0 ){
         num_on = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_on <= 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-off",4) == 0 ){
         num_off = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_off <= 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-length",4) == 0 ){
         num_len = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_len <= 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-cycles",4) == 0 ){
         num_cycles = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_cycles <= 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-init",4) == 0 ){
         num_init = strtol( argv[++narg] , NULL , 10 ) ;
         if( num_init < 0 ) ERROR ;
         continue ;
      }

      if( strncmp(argv[narg],"-name",4) == 0 ){
         strcpy( fname , argv[++narg] ) ;
         continue ;
      }

      fprintf( stderr , "illegal option in sqwave: %s\n" , argv[narg] ) ;
      ERROR ;

   } while( ++narg < argc ) ;

/*** prepare for output ***/

   if( num_on <= 0 && num_off <= 0 ) ERROR ;

   if( num_on <= 0 ){
      num_on = num_off ;
   } else if( num_off <= 0 ){
      num_off = num_on ;
   }

   if( num_on   <= num_on_kill   ) ERROR ;
   if( num_off  <= num_off_kill  ) ERROR ;
   if( num_init <  num_init_kill ) ERROR ;

   if( num_len <= 0 ){
      if( num_cycles > 0 ){
         num_len = num_cycles * ( num_on + num_off ) ;
         if( num_init > 0 ) num_len = num_len + num_init ;
      } else {
         ERROR ;
      }
   }

   if( strncmp(fname,"stdout",6) == 0 || fname[0] == '\0' || fname[0] == '-' ){
      sqfile = stdout ;
   } else {
      sqfile = fopen( fname , "w" ) ;
      if( sqfile == NULL ) ERROR ;
   }

/*** output ***/

   nout = 0 ;

#define POFF {fprintf(sqfile,"0\n")    ;nout++;if(nout>=num_len)goto DONE;}
#define PON  {fprintf(sqfile,"10\n")   ;nout++;if(nout>=num_len)goto DONE;}
#define PKIL {fprintf(sqfile,"99999\n");nout++;if(nout>=num_len)goto DONE;}

   for( ii=0 ; ii < num_init ; ii++ ){
      if( ii < num_init_kill ) PKIL else POFF
   }

   do{

      for( ii=0 ; ii < num_on ; ii++ ){
         if( ii < num_on_kill ) PKIL else PON
      }

      for( ii=0 ; ii < num_off; ii++ ){
         if( ii < num_off_kill ) PKIL else POFF
      }

   } while( 1 ) ;

DONE:
   if( sqfile != stdout ) fclose( sqfile ) ;
   printf("%s\n" , fname ) ;
   exit(0) ;
}
