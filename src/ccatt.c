#include "stdio.h"
#include "string.h"

int main( int argc , char * argv[] )
{
   char line1[2560] , line2[256] ;
   char starter[256] = "t" ;
   char killer[256] = "bytes" ;
   char * cpt ;
   int idone, narg ;

   if( argc > 1 && strcmp(argv[1],"-help")==0 ){
      fprintf(stderr,
              "Usage: ccatt [options] < input > output\n"
              "Copies stdin to stdout, concatenating lines that don't begin with\n"
              "the 'starter' string.  By default, the starter string is \"t\".\n"
              "Options:\n"
              "  -starter string  --> Use 'string' for the starter.\n"
              "  -killer  string  --> Use 'string' for the killer.\n"
             ) ;
      exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-starter") == 0 ){
         strcpy(starter,argv[++narg]) ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-killer") == 0 ){
         strcpy(killer,argv[++narg]) ;
         narg++ ; continue ;
      }

      fprintf(stderr,"Don't know option: %s\n",argv[narg]) ;
      exit(1) ;
   }

   cpt = gets(line1) ; if( cpt == NULL ) exit(0) ;
   idone = 0 ;

   do{
      cpt = gets(line2) ;
      if( cpt == NULL ){ printf("%s\n",line1) ; break ; }

      if( strstr(line2,killer) == line2 ) continue ;  /* skip this line */

      if( strstr(line2,starter) == line2 ){  /* next line starts OK */
         printf("%s\n",line1) ;              /* so write line1 out */
         strcpy(line1,line2) ;               /* and move line2 up to be line1 */

      } else {                               /* next line doesn't start OK */
         strcat(line1,line2) ;               /* so tack it onto line1 */
         idone++ ;
      }
   } while(1) ;

   fprintf(stderr,"%d lines catenated\n",idone) ;
   exit(0) ;
}
