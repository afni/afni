/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "cs.h"

#define NBUF 256

int main( int argc , char * argv[] )
{
   char buf[NBUF] , buf2[NBUF*2];
   int ll , num=0 , ii , jj ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage ~1~"
             "\n"
             "To run:  quotize name < input > output\n"
             "\n"
             "Turns a text file into a C array of strings\n"
             "initialized into an array 'char *name[]'.\n"
             "\n"
             "For example, his program is used to (re)generate readme_env.h\n"
             "in the main AFNI codebase, which is displayed to users so\n"
             "they know about environment variables.\n"
             "\n"
             "Updating AFNI environment variable descriptions ~1~\n"
             "\n"
             "NB: You should NOT edit readme_env.h directly, but instead\n"
             "edit the file afni/doc/README.environment with the env var\n"
             "info, and then use THIS program to regenerate readme_env.h.\n"
             "That new readme_env.h should then be committed+pushed to the\n"
             "main afni repository.\n"
             "\n"
             "So, if you update the afni/doc/README.environment text file\n"
             "with fun, useful information, then you can cd into the main\n"
             "AFNI source code folder (e.g., 'afni/src/'), and then run the\n"
             "above command, noting:\n"
             "+ the '<' and '>' are literally included on the cmd line call\n"
             "+ 'name' should be 'readme_env' (without quotes is fine)\n"
             "+ 'input' should be the path to: afni/doc/README.environment'\n"
             "+ 'output' should be the new text file, eventually readme_env.h\n"
             "\n"
             "Therefore, an enterprising youth might run:\n"
"  quotize readme_env < ~/AFNI/afni/doc/README/README.environment > NEW.txt\n"
             "... and then check the NEW.txt, perhaps comparing it to\n"
             "the existing readme_env.h for good luck.\n"
             "If happy with the updates, then replace the earlier form\n"
             "with this new creation:\n"
             "  mv NEW.txt readme_env.h\n"
             "... and commit+push the changes in the afni repository.\n"
             ) ;
      exit(0) ;
   }

   printf("/** automatically generated by AFNI program quotize **/\n") ;
   printf("static char *%s[] = {\n",argv[1]) ;
   while( afni_fgets(buf,NBUF,stdin) != NULL ){
      ll = strlen(buf) ; if( ll == 0 ) break ;
      if( buf[ll-1] == '\n' ) buf[ll-1] = '\0' ;
      for( ii=0,jj=0 ; buf[ii] != '\0' ; ){
         if( buf[ii] == '"' || buf[ii] == '\\' ) buf2[jj++] = '\\' ;
         buf2[jj++] = buf[ii++] ;
      }
      buf2[jj] = '\0' ;
      printf("   \"%s\\n\" ,\n" , buf2 ) ;
      num++ ;
   }
   printf("   NULL } ;\n") ;
   printf("#define NUM_%s %d\n",argv[1],num) ;
   exit(0) ;
}
