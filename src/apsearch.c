/*** Whereami.c modified 1/11/05 -- main function by Mike Angstadt of U Chicago ***/

#define MAIN
#define SUMA_noFunc

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "thd_atlas.h"
#include "matrix.h"
#include "suma_suma.h"



#define zischar(ch) ( ( ((ch) >= 'A' && (ch) <= 'Z' ) || ((ch) >= 'a' && (ch) <= 'z' ) ) ? 1 : 0 )
#define isnakedarg(s) ( ( (s)[0] == '-' && strlen(s) > 1 && zischar((s)[1]) ) ? 0 : 1 )



void apsearch_usage() 
{
   int i = 0;
   
   ENTRY("apsearch_usage");
   /* print help message in three sections */
   fprintf(stdout,
   "\n"
   "A program to perform simple approximate string searching. It's primary\n"
   "purpose is to test string matching for atlas area locations.\n"
   "\n"
   "  apsearch <-word WORD> <[-file FILE] | [-text TEXT] | [-phelp PROG]> \n"
   "           [OPTIONS]\n"
   "\n"
   "Parameters:\n"
   "===========\n"
   "  -word WORD: WORD being sought\n"
   "  -file FILE: Search for WORD in text file FILE\n"
   "  -text TEXT: Search for WORD in string TEXT\n"
   "  -phelp PROG: Search for WORD in output of command PROG -help\n"
   "  -ci: Case insensitive search (default)\n"
   "  -cs: Case sensitive search\n"
   "  -help: You're looking at it.\n"
   "  -max_hits MH: Return best MH hits only. Default MH = 3.\n"
   "  -func_test: Run sample function testing and quit.\n"
   "\n"
   "Examples:\n"
   "=========\n"
   "  apsearch -ci -phelp whereami -word -atlas\n"
   "\n"
   ); 
   
   PRINT_COMPILE_DATE ;
   EXRETURN;
}


/*----------------------------------------------------------------------------*/

int main(int argc, char **argv)
{
   int iarg, N_ws, i, max_hits, test_only;
   float *ws_score=NULL;
   char *fname=NULL, *text=NULL, *prog=NULL, *str=NULL, **ws=NULL;
   byte ci = 1;
   
   max_hits = 3;
   test_only=0;
   
   if (argc <= 1) {
      apsearch_usage();
      return(1); 
   }
   
   iarg = 1 ; 
   while( iarg < argc ){
      if (strcmp(argv[iarg],"-ci") == 0) { 
         ci = 1; 
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-cs") == 0) { 
         ci = 0; 
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-func_test") == 0) { 
         test_only = 0; 
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-help") == 0 ) { 
         apsearch_usage();
         return(1); 
         continue; 
      }

      if (strcmp(argv[iarg],"-file") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need text file after -file\n"); return(1);
         }

         fname = argv[iarg];
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-text") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need string after -text\n"); return(1);
         }

         text = argv[iarg];
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-phelp") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need program name after -phelp\n"); return(1);
         }

         prog = argv[iarg];
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-word") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need text file after -file\n"); return(1);
         }

         str = argv[iarg];
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-max_hits") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need an integer after -max_hits\n"); return(1);
         }

         max_hits = atoi(argv[iarg]); 
         ++iarg;
         continue; 
      }
      { /* bad news in tennis shoes */
         fprintf(stderr,"** Error: bad option %s\n", argv[iarg]);
         return 1;
      }

   } 
   
   if (fname && text) {
      ERROR_message("-file and -text are mutually exclusive");
      return 1;
   }
   
   if (test_only) {
      test_approx_str_match();
      return 0;
   }

   if (str && (fname || text || prog)) {
      if (fname) {
         ws = approx_str_sort_tfile(fname, &N_ws, str, 
                            ci, &ws_score,
                            NULL);
      } else if (text) {
         ws = approx_str_sort_text(text, &N_ws, str, 
                            ci, &ws_score,
                            NULL);
      } else if (prog) {
         ws = approx_str_sort_phelp(prog, &N_ws, str, 
                            ci, &ws_score,
                            NULL);
      }
      for (i=0; i<N_ws; ++i) {
         if (i<max_hits) {
            fprintf(stdout,"%02f- %s\n", ws_score[i], ws[i]);
         }
         free(ws[i]);
      } free(ws);  free(ws_score); ws_score=NULL;
   } 
   
   return 0;  
}
