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


/*--- Move to thd_getpathprogs.c when ready ------------*/
/*! Get all executables in directory where afni resides */
extern THD_string_array * THD_get_all_afni_executables(void );

THD_string_array * THD_get_all_afni_executables(void )
{
   THD_string_array *outar=NULL, *elist=NULL;
   char *af=NULL, *etr=NULL;
   int N_af, N_afni=strlen("afni"), iaf=0, ii=0, smode;
   char scomm[256]={""};
   
   ENTRY("THD_get_all_afni_executables");
   
   if (!(elist = get_elist()) ||
       !(af = THD_find_executable("afni"))) {
      ERROR_message("Could not find afni, we're doomed daddy!");
      RETURN(outar);
   }
   
   /* remove afni from the end to get the path */
   N_af = strlen(af);
   if (strcmp(af+N_af-N_afni,"afni")) {
      ERROR_message("This should not be (%s)!", af+N_af-N_afni);
      RETURN(outar);
   }
   af[strlen(af)-N_afni]='\0'; N_af = strlen(af);
   if (af[N_af-1] != '/') {
      af[N_af] = '/'; af[N_af+1] = '\0';
      ++N_af;
   }
   /* Now get all executables under af */
   INIT_SARR( outar );
   for (ii=0, iaf=0; ii<elist->num ; ii++ ){
      smode = storage_mode_from_filename(elist->ar[ii]);
      etr = THD_trailname( elist->ar[ii] , 0 ) ; 
      if (
          !THD_is_directory(elist->ar[ii]) &&
          !strncmp(af, elist->ar[ii], N_af) &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".jpg") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".a") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".so") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".txt") &&
    !(STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".py") && !strncmp(etr,"lib_",4)) &&
          (smode <= STORAGE_UNDEFINED || smode >= LAST_STORAGE_MODE)  &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".sumarc") &&
          !STRING_HAS_SUFFIX_CASE(elist->ar[ii], ".afnirc")
              )  {
         ADDTO_SARR( outar , elist->ar[ii] ) ; ++iaf;
         /* fprintf(stderr," %d- %s\n", iaf, etr); */
      } else {
         /* fprintf(stderr," skip %s (%s)\n", elist->ar[ii], af); */
      }
   } 
   
   if( SARR_NUM(outar) == 0 ) DESTROY_SARR(outar) ;

   RETURN( outar );

}

int list_afni_programs(void)
{
   int nprogs=0, ii=0;
   char *etr=NULL;
   THD_string_array *progs=NULL;
   
   if (!(progs = THD_get_all_afni_executables())) {
      ERROR_message("Cannot get list of programs");
      RETURN(0);
   }
   for (ii=0; ii<progs->num ; ii++ ){
      etr = THD_trailname( progs->ar[ii] , 0 ) ;
      fprintf(stdout,"  %3d.   %s\n", ii, etr);
   }
   nprogs = progs->num;
   
   DESTROY_SARR(progs);
   
   return(nprogs);
}

int update_help_for_afni_programs(int force_recreate, 
                                  byte verb, THD_string_array **hlist )
{
   int ii, iaf, icomm, estat;
   char hout[128], scomm[256], *etr=NULL, *hdir=NULL, *etm=NULL;
   THD_string_array *progs=NULL;
   
   
   ENTRY("produce_help_for_afni_programs");
   
   if (hlist && *hlist) {
      ERROR_message("if hlist then must have *hlist = NULL\n");
      RETURN(0);   
   }
   
   hdir = THD_helpdir();
   if (hdir[0] == '\0') {
      ERROR_message("Have no help directory\n");
      RETURN(0);
   }
   if (!THD_mkdir(hdir)) {
      ERROR_message("Cannot create %s directory\n", hdir);
      RETURN(0);
   }
   
   if (!(progs = THD_get_all_afni_executables())) {
      ERROR_message("Cannot get list of programs");
      RETURN(0);
   }
   
   if (hlist) INIT_SARR((*hlist));
   icomm=0;
   for (ii=0, iaf=0; ii<progs->num ; ii++ ){
      etr = THD_trailname( progs->ar[ii] , 0 ) ; 
      if (!etr || strlen(etr) < 2) {
         WARNING_message("Fishy executable named %s\n",progs->ar[ii]);
         continue;
      }
      etm = THD_filetime(progs->ar[ii]);
      if (etm[0] == '\0') {
         etm = "NoTimeStamp";
      }
      snprintf(hout, 120*sizeof(char),
               "%s/%s.%s.help", hdir, etr, etm);
      if (!force_recreate && THD_is_file(hout)) {
         if (verb) fprintf(stderr,"Reusing %s \n", hout); 
      } else {
         if (verb) fprintf(stderr,"Creating %s \n", hout); 
         if (icomm > 25) { /* sleep a little to allow 
                              forked processes to end */
            NI_sleep(250); icomm = 0;
         }
         /* The echo below is there to make programs that
            don't like -help and expect stdin to shut up and quit
            As a result, it is hard to get the status of -help
            command and use it wisely here without risking 
            trouble */
         snprintf(scomm, 250*sizeof(char),
               "echo '' | %s -help >& %s &", etr, hout);
         system(scomm); ++icomm;
      }
      
      if (hlist) ADDTO_SARR((*hlist), hout);
      
   }
   /* cleanup hdir */
   if (verb) fprintf(stderr,"Cleaning help directory %s\n",hdir);
   if (system("@clean_help_dir")) {
      WARNING_message("Failed cleaning help directory");
   }
   
   DESTROY_SARR(progs) ;
   
   RETURN(1);
}


         
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
   "  -popt PROG: Search for possible options of PROG that match WORD\n"
   "              Make sure you add the '-' to WORD if you are looking\n"
   "              for an actual option.\n"
   "  -all_popts PROG: TRY to guess at all the options for PROG\n"
   "                  The list of options is not guaranteed to be full\n"
   "                  or accurate. It is created by parsing the program's\n"
   "                  -help output for likely candidates. \n"
   "                  It is meant to act as an aid in locating\n"
   "                  certain options.\n"  
   "  -ci: Case insensitive search (default)\n"
   "  -cs: Case sensitive search\n"
   "  -help: You're looking at it.\n"
   "  -max_hits MH: Return best MH hits only. Default MH = 3.\n"
   "                Use -1 to get all results back.\n"
   "  -min_different_hits mDH: Keep outputing hits until you have dDH\n"
   "                           dissimilar matches. \n"
   "                           Default is -1 (no minimum).\n"
   "  -unique_hits_only: Restrict output to novel hits only.\n"
   "  -show_score: Show matching word's distance.\n"
   "  -show_score_detail: That's right.\n"
   "  -all_afni_progs: List all executables in AFNI's bin directory\n"
   "  -all_afni_help: Build/update -help output under directory:\n"
   "                     %s\n"
   "                  If older help files differ by little they are deleted\n"
   "                  Little differences would be the compile date or the\n"
   "                  version number. See @clean_help_dir code for details.\n"
   "  -afni_help_dir: Print afni help directory location and quit.\n"
   "\n"
   "  NOTE: The maximum number of results depends on the combination of\n"
   "        -max_hits, -min_different_hits, and -unique_hits_only. \n"
   "        Withoug -unique_hits_only, the output will continue \n"
   "        while neither -max_hits or -min_different_hits conditions \n"
   "        are met.\n"
   "\n"
   "  -func_test: Run sample function testing and quit. Debugging only.\n"
   "\n"
   "Examples:\n"
   "=========\n"
   " 1- Search help output of program whereami for the word '-atlas'\n"
   "        apsearch -ci -phelp whereami -word -atlas\n"
   " 2- Search all atlas area names for some name (mistakes on purpose)\n"
   "        whereami -show_atlas_code > all_atlas_area_names.txt\n"
   "        apsearch -file all_atlas_area_names.txt -word hepp\n"
   "        apsearch -file all_atlas_area_names.txt -word zipp \\\n"
   "                  -min_different_hits 5 -unique_hits_only \n"
   "        apsearch -file all_atlas_area_names.txt -word hipp \\\n"
   "                  -min_different_hits 5 -unique_hits_only \n"
   " 3- Debug stupid string matcher:\n"
   "        apsearch -text 'u:Hippocampus' -word hipp -show_score_detail\n"
   "        apsearch -text 'u:IPC' -word hipp -show_score_detail\n"
   " 4- Search help of AFNI programs:\n"
   "        apsearch -phelp afni -word port\n"
   "        apsearch -phelp 3dSkullStrip -word hull\n"
   "        apsearch -phelp afni  -word xt\n"
   " 5- Suggest a valid option from a program:\n"
   "        apsearch -popt afni -word xt\n"
   "        apsearch -popt @ROI_Corr_Mat -word sel\n"
   "        apsearch -popt @ROI_Corr_Mat -word -sel\n"
   " 6- Show all(*) options for a program:\n"
   "        apsearch -all_popts 3dSkullStrip\n"
   "    (*) see -all_popts in help section\n"
   "\n", 
   THD_helpdir()); 
   
   PRINT_COMPILE_DATE ;
   EXRETURN;
}


/*----------------------------------------------------------------------------*/

int main(int argc, char **argv)
{
   int iarg, N_ws, i, max_hits, test_only, new_score=0,
       i_unique_score=0, min_different_hits=0, unq_only=0,
       show_score=0;
   float *ws_score=NULL, last_score=-1.0;
   char *fname=NULL, *text=NULL, *prog=NULL, *str=NULL, **ws=NULL, 
         *all_popts=NULL, *popt=NULL;
   APPROX_STR_DIFF *D=NULL;
   byte ci = 1;
   
   mainENTRY("apsearch main"); machdep() ; 
      
   max_hits = 3;
   test_only=0;
   min_different_hits = -1;
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

      if (strcmp(argv[iarg],"-afni_help_dir") == 0) { 
         fprintf(stdout,"%s\n", THD_helpdir());
         return(0);
      }

      if (strcmp(argv[iarg],"-show_score") == 0) { 
         show_score=1; 
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-show_score_detail") == 0) { 
         show_score=2; 
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
      
      if (strcmp(argv[iarg],"-phelp") == 0 ||
          strcmp(argv[iarg],"-popt") == 0 ) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
               "** Error: Need program name after -phelp or -popt\n"); return(1);
         }

         if (strcmp(argv[iarg-1],"-popt") == 0) popt = argv[iarg];
         else prog = argv[iarg];

         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-all_popts") == 0 ) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
               "** Error: Need program name after -all_popts\n"); return(1);
         }

         all_popts = argv[iarg];
         max_hits = -1;
         str = "-";
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
      
      if (strcmp(argv[iarg],"-min_different_hits") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need an integer after -min_different_hits\n"); 
                     return(1);
         }

         min_different_hits = atoi(argv[iarg]); 
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-unique_hits_only") == 0) { 
         unq_only = 1;
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-all_afni_help") == 0) { 
         update_help_for_afni_programs(0, 1, NULL); return(0);
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-all_afni_progs") == 0) { 
         list_afni_programs(); return(0);
         ++iarg;
         continue; 
      }
      
      { /* bad news in tennis shoes */
         fprintf(stderr,"** Error %s: bad option %s\n", argv[0], argv[iarg]);
         suggest_best_prog_option(argv[0], argv[iarg]);
         return 1;
      }

   } 
   
   if (fname && text) {
      ERROR_message("-file and -text are mutually exclusive");
      return 1;
   }
   
   if (min_different_hits<=0) min_different_hits = -1;
   if (max_hits <=0) max_hits = MRI_maxint;
   
   if (test_only) {
      test_approx_str_match();
      return 0;
   }

   if (str && (fname || text || prog || popt || all_popts)) {
      if (fname) {
         ws = approx_str_sort_tfile(fname, &N_ws, str, 
                            ci, &ws_score,
                            NULL, &D);
      } else if (text) {
         ws = approx_str_sort_text(text, &N_ws, str, 
                            ci, &ws_score,
                            NULL, &D);
      } else if (prog) {
         ws = approx_str_sort_phelp(prog, &N_ws, str, 
                            ci, &ws_score,
                            NULL, &D);
      } else if (popt) {
         suggest_best_prog_option(popt, str);
         return 0;
      } else if (all_popts) {
         /* one can also use print_prog_options(all_popts); return(0); */
         ws = approx_str_sort_all_popts(all_popts, &N_ws, str, 
                            ci, &ws_score,
                            NULL, &D);
      }
      
      i_unique_score = 0; last_score = -1.0; new_score=1;
      for (i=0; i<N_ws; ++i) {
         if (ws[i]) {
            new_score=0;
            if (ws_score[i] != last_score) {
               last_score = ws_score[i];
               new_score=1;
               ++i_unique_score;
            }
            if (i<max_hits || i_unique_score<=min_different_hits) {
               if (!unq_only || new_score) {
                  if (show_score) fprintf(stdout,"%03f ", ws_score[i]);
                  if (show_score > 1) 
                     fprintf(stdout,"%s ", approx_string_diff_info(D+i, NULL));
                  if (!show_score) fprintf(stdout,"   ");
                  fprintf(stdout,"%s\n", ws[i]);
               }
            }
            free(ws[i]); ws[i]=NULL;
         }
      } free(ws);  if (ws_score) free(ws_score); ws_score=NULL;
      if (D) free(D); D=NULL;
   } 
   
   return 0;  
}
