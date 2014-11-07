#define MAIN
#define SUMA_noFunc

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include "suma_suma.h"



#define zischar(ch) ( ( ((ch) >= 'A' && (ch) <= 'Z' ) || ((ch) >= 'a' && (ch) <= 'z' ) ) ? 1 : 0 )
#define isnakedarg(s) ( ( (s)[0] == '-' && strlen(s) > 1 && zischar((s)[1]) ) ? 0 : 1 )


int update_help_for_afni_programs(int force_recreate, 
                                  byte verb, byte clean, 
                                  THD_string_array **hlist )
{
   int ii, iaf, icomm, estat;
   char hout[128], houtc[128], scomm[256], *etr=NULL, *hdir=NULL, *etm=NULL;
   THD_string_array *progs=NULL;
   
   
   ENTRY("produce_help_for_afni_programs");
   
   if (hlist && *hlist) {
      ERROR_message("if hlist then must have *hlist = NULL\n");
      RETURN(0);   
   }
   
   if (!(hdir = THD_get_helpdir(0))) {
      ERROR_message("Have no help directory\n");
      RETURN(0);
   }
   
   if (!(progs = THD_get_all_afni_executables())) {
      ERROR_message("Cannot get list of programs");
      RETURN(0);
   }
   
   if (hlist) INIT_SARR((*hlist));
   icomm=0;
   for (ii=0, iaf=0; ii<progs->num ; ii++ ){
      /* ------------------------------------------------------------*/
      /* THIS BLOCK is essentially get_updated_help_file()
         But I keep it separate because of the need for 
         nap time. Knowing when to sleep in get_updated_help_file()
         is not that simple.                                   */
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
      snprintf(houtc, 120*sizeof(char),
               "%s/%s.complete", hdir, etr);
      if (!force_recreate && THD_is_file(hout)) {
         if (verb) 
            fprintf(stderr,"Reusing %s (%d/%d)\n", hout, ii, progs->num );
         if (!THD_is_file(houtc)) {
            prog_complete_command(etr, houtc, -1);
         }      
      } else {
         if (verb) 
            fprintf(stderr,"Creating %s (%d/%d)\n", hout, ii, progs->num); 
         if (icomm > 25) { /* sleep a little to allow 
                              forked processes to end */
            NI_sleep(250); icomm = 0;
         }
         /* The echo below is there to make programs that
            don't like -help and expect stdin to shut up and quit
            As a result, it is hard to get the status of -help
            command and use it wisely here without risking 
            trouble. 
            */
         if (THD_is_file( hout)) {
            snprintf(scomm, 250*sizeof(char),
               "chmod u+w %s", hout);
            system(scomm); 
         }
         snprintf(scomm, 250*sizeof(char),
               "\\echo '' 2>&1 | %s -help > %s 2>&1 ", etr, hout);
         system(scomm); 
         snprintf(scomm, 250*sizeof(char),
               "chmod a-w %s", hout);
         system(scomm); 
         prog_complete_command(etr, houtc, -1);
         ++icomm;
      }
      /* ------------------------------------------------------------*/
      
      if (hlist) ADDTO_SARR((*hlist), hout);
      
   }
   /* cleanup hdir */
   if (verb) fprintf(stderr,"Cleaning help directory %s\n",hdir);
   if (clean && system("@clean_help_dir")) {
      WARNING_message("Failed cleaning help directory");
   }
   
   DESTROY_SARR(progs) ;
   
   RETURN(1);
}


         
void apsearch_usage(int detail) 
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
   "\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
"Parameters:\n"
"===========\n"
"  -word WORD: WORD being sought\n"
"  -w WORD: Abbreviated version of -word WORD\n"
"  -file FILE: Search for WORD in text file FILE\n"
"  -files FILE1 FILE2 ...: Search for WORD in text files FILE1 FILE2 ...\n"
"  -text TEXT: Search for WORD in string TEXT\n"
"  -stdin: Search for WORD in text from stdin\n"
"  -: Same as -stdin\n"
"  -phelp PROG: Search for WORD in output of command PROG -help\n"
"  -popt PROG: Search for possible options of PROG that match WORD\n"
"              Make sure you add the '-' to WORD if you are looking\n"
"              for an actual option.\n"
"  -sphinx_phelp PROG: Format the output of PROG -help in a sphinxized way.\n"
"  -doc_2_txt: Format AFNI markups from  -file/-files/-stdin content for text\n"
"              output.\n"
"  -doc_2_spx: Format AFNI markups from  -file/-files/-stdin content for\n"
"              Sphinx output.\n"
"  -doc_markup_sample: Shown an example of the types of markups available for\n"
"                      the documentation.\n"
"  -all_afni_help: Search for WORD in all afni help files.\n"
"                  This option is not all that clever at the moment.\n"
"  -all_popts PROG: TRY to guess at all the options for PROG\n"
"                  The list of options is not guaranteed to be full\n"
"                  or accurate. It is created by parsing the program's\n"
"                  -help output for likely candidates. \n"
"                  It is meant to act as an aid in locating\n"
"                  certain options.\n"  
"  -list_popts PROG: Like -all_popts, but preserve unique set of options\n"
"                    only, no chunks of help output are preserved.\n"
"  -popts_complete_command PROG: Generate a csh command that can be sourced\n"
"                                to allow option autocompletion for program\n"
"                                PROG.\n"
"                          See also option -bash and -update_all_afni_help\n" 
"  -bash: Use bash format for the complete command. Default is csh/tcsh\n"
"         This option MUST PRECEDE option -popts_complete_command\n"
"  -ci: Case insensitive search (default)\n"
"  -cs: Case sensitive search\n"
"  -global_help: Show help for global options.\n"
"  -gopts_help:  Show help for global options.\n"
"  -help: You're looking at it.\n"
"  -max_hits MH: Return best MH hits only. Default MH = 3.\n"
"                Use -1 to get all results back.\n"
"  -m MH: Abbreviated version of -max_hits MH.\n"
"  -min_different_hits mDH: Keep outputing hits until you have dDH\n"
"                           dissimilar matches. \n"
"                           Default is -1 (no minimum).\n"
"  -unique_hits_only: Restrict output to novel hits only.\n"
"  -show_score: Show matching word's distance.\n"
"  -show_score_detail: That's right.\n"
"  -list_all_afni_progs: List all executables in AFNI's bin directory\n"
"  -list_all_afni_P_progs: Same as -list_all_afni_progs but with path\n"
"  -list_all_afni_readmes: List all README files in AFNI's bin directory\n"
"  -list_all_afni_P_readmes: Same as -list_all_afni_readmes but with path\n"
"  -list_all_afni_dsets: List all datasets in AFNI's bin directory\n"
"  -list_all_afni_P_dsets: Same as -list_all_afni_dsets but with path\n"
"  -update_all_afni_help: Build/update -help output under directory:\n"
"                     %s\n"
"                  If older help files differ by little they are deleted\n"
"                  Little differences would be the compile date or the\n"
"                  version number. See @clean_help_dir code for details.\n"
"                  This option also creates autocompletion code for \n"
"                  csh/tcsh and bash shells.\n"
"  -recreate_all_afni_help: Like -update_all_afni_help but force receration\n"
"                           even if nothing changed in the help\n"
"  -afni_help_dir: Print afni help directory location and quit.\n"
"  -afni_data_dir: Print afni data directory location and quit.\n"
"  -afni_bin_dir: Print afni's binaries directory location and quit.\n"
"  -afni_home_dir: Print afni's home directory and quit.\n"
"  -afni_rc_file: Pathname to .afnirc. You'll get one even if none exists.\n"
"  -afni_custom_atlas_dir: Print your afni's custom atlas directory \n"
"                          and quit.\n"
"  -afni_custom_atlas_file: Print your afni's custom atlas file (if any)\n"
"                          and quit.\n"
"  -afni_text_editor: Print the name of the GUI editor. Priority goes to \n"
"                     env. variable AFNI_GUI_EDITOR, otherwise afni\n"
"                     will try to find something suitable.\n"
"  -afni_web_browser: Print the name of the browser used by AFNI. \n"
"                     Priority goes to env. variable AFNI_WEB_BROWSER, \n"
"                     otherwise afni will try to find something suitable.\n"
"  -afni_web_downloader: Print the name of the downloader used by AFNI. \n"
"                     Priority goes to env. variable AFNI_WEB_DOWNLOADER, \n"
"                     otherwise afni will try to find something suitable.\n"
"  -view_text_file FILE: Open FILE with editor of -afni_text_editor\n"
"  -view_readme SOMETHING: Find a readme.SOMETHINGISH and open it\n" 
"  -apsearch_log_file: Print the name of the logfile that is used to save\n"
"                      some results of apsearch's functions. This option\n"
"                      is for debugging purposes and is only activated if\n"
"                      the environment variable AFNI_LOG_BEST_PROG_OPTION\n"
"                      is set to YES.\n"
"  -view_prog_help PROG: Open the help file for PROG in a GUI editor.\n"
"                        This is like the option -h_view in C programs.\n"
"  -web_prog_help PROG: Open the help file for PROG in a web brower.\n"
"                       This is like the option -h_web in C programs.\n"
"              Use ALL to view the page containing help for all programs.\n"
"  -web_class_docs: Open the webpage with latest class pdfs.\n"
"\n"
"  NOTE: The maximum number of results depends on the combination of\n"
"        -max_hits, -min_different_hits, and -unique_hits_only. \n"
"        Withoug -unique_hits_only, the output will continue \n"
"        while neither -max_hits or -min_different_hits conditions \n"
"        are met.\n"
"\n"
"  -func_test: Run sample function testing and quit. Debugging only.\n"
"\n"
"Wildcard expansion tools:\n"
"=========================\n"
"-wild_files 'PAT1 PAT2 ...' : Find files matching PAT1, or PAT2, etc.\n"
"                              Should include PAT1, etc. between quotes or \n"
"                              the shell will do the expansion for you.\n"
"                              Note that in addition to wildcard expansion, \n"
"                              the function also sorts the output so the order\n"
"                              is alphabetical. It also dumps duplicate names\n"
"                              which can happen when you start to remove \n"
"                              extensions known to AFNI. See -wild* options\n"
"                              below.\n" 
"        Example: -wild_files '*.do *.HEAD'\n"
"-wild_files_noAext: After compiling list, remove all known AFNI extensions \n"
"                    and preserve unique set of resultant names\n"
"-wild_files_noAext_noAview: After compiling list, remove all known AFNI\n"
"                    extensions and any view such as +tlrc, +orig, +acpc, \n"
"                    and preserve unique set of resultant names\n"
"-wild_files_orig_name: Output unique list using orignal (full) filename, \n"
"                       rather than the names after extensions or views were\n"
"                       removed. This option makes a difference when using\n"
"                       one of -wild_files_noAext* options.\n"
"-wild_all_files: Show all files from wildcard expansion. Do not sort, do not\n"
"                 trim names, etc.\n"
"-wild_files_debug: Output results in debugging mode.\n"
"-wild_files_ci: When searching for unique set, use case insensitive matching\n"
"-test_unique_str: Run debugging tests for function unique_str().\n"
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
" 7- Look for some area named something or other in some atlas:\n"
"        whereami -show_atlas_code -atlas DKD_Desai_MPM |\\\n"
"                                apsearch -stdin -word insola\n"
"    If you really screw up the spelling, you should help the search\n"
"    program a little as in:\n"
"        whereami -show_atlas_code -atlas DKD_Desai_MPM |\\\n"
"                                sed 's/[-_]/ /g' |\\\n"
"                                apsearch -stdin -word insolent\n"
" 8- Find 10 afni programs with something like 'Surface' in their names:\n"
"        apsearch -list_all_afni_progs | \\\n"
"             apsearch -stdin -word surface -max_hits 10\n"
" 9- Open the readme for driving AFNI:\n"
"        apsearch -view_readme driv\n"
" 10- Wildcard expansion and sorting:\n"
"        apsearch -wild_files '*.1D*' '*.HEAD *.BRIK*' \\\n"
"                 -wild_all_files \n"
"        apsearch -wild_files '*.1D*' '*.HEAD *.BRIK*' \\\n"
"                 -wild_files_noAext_noAview \n"
"        apsearch -wild_files '*.1D*' '*.HEAD *.BRIK*' \\\n"
"                 -wild_files_noAext_noAview -wild_files_orig_name \n"
"\n"
"Global Options:\n"
"===============\n"
"%s", 
   THD_helpdir(0),
   detail > 1 ? get_gopt_help():""); 
   PRINT_COMPILE_DATE ;
   }
   return;
}


/*----------------------------------------------------------------------------*/
char *text_from_stdin(int *nread) 
{
   int N_lbuf = 30000;
   char lbuf[N_lbuf+1];
   char *txt=NULL, *cpt=NULL;
   int ex = 0, i, N_alloc=0, nchar=0, nnew;
   
   
   if (nread) *nread = -1;
   
   i = 0; N_alloc=0; nchar=0, nnew=0;
   do{  
      cpt = afni_fgets(lbuf,N_lbuf,stdin) ;
      lbuf[N_lbuf] = '\0';
      ex = feof(stdin);
      if( cpt==NULL && !ex){ 
         free(txt);
         ERROR_message("Failure reading from stdin");
         return(NULL); 
      }
      if (!ex) {
         nnew = strlen(lbuf);
         if (nchar+nnew >= N_alloc) {
            N_alloc += (nnew+10000);
            txt = (char *)realloc(txt, sizeof(char)*N_alloc);
         }
         /* fprintf(stderr,"%d- %s",nchar, lbuf);*/
         strcat(txt, lbuf); nchar += nnew;
      }
   } while (!ex);

   txt = (char *)realloc(txt, sizeof(char)*nchar);
   if (nread) *nread = nchar;
   
   return(txt); 
}

/* A function to test function unique_str. Use for debugging only */
int Test_unique_str(void)
{
   /* dirty test for sorting */
   char **ww, **ws;
   int i, N_words=10, N_unq, *isrt=NULL;
   ww = (char **)calloc(N_words, sizeof(char *));
   i = 0;
   ww[i++] = strdup("hello+orig.HEAD");
   ww[i++] = strdup("hello+orig.BRIK.gz");
   ww[i++] = NULL;
   ww[i++] = strdup("HELLO+orig.HEAD");
   ww[i++] = NULL;
   ww[i++] = strdup("hello.nii");
   ww[i++] = strdup("HELLO+orig.HEAD");
   ww[i++] = strdup("james.nii");
   if (i >= N_words) {
      ERROR_message("Too many strings for allocated space"); return(1);
   }

   fprintf(stdout,
            "\nInitial list of %d strings:\n", N_words);
   for (i=0; i<N_words; ++i) {
      fprintf(stdout,"%d %s\n", i, ww[i]?ww[i]:"NULL");
   }

   if ((ws = unique_str(ww, N_words, 0, 0, &N_unq, &isrt))) {
      fprintf(stdout,
            "\n%d/%d unique strings, case sensisitve, names as is:\n", 
                     N_unq, N_words);
      for (i=0; i<N_words; ++i) {
         fprintf(stdout,"%d %s (ww[%d]=%s)\n",
                  i, ws[i]?ws[i]:"NULL - what follows is legit garbage", 
                  isrt[i], ww[isrt[i]]?ww[isrt[i]]:"NULL" );
      }
      if (isrt) free(isrt); isrt = NULL;
      for (i=0; i<N_words; ++i) if (ws[i]) free(ws[i]);
      free(ws); ws = NULL;
   }
   if ((ws = unique_str(ww, N_words, 1, 0, &N_unq, &isrt))) {
      fprintf(stdout,
               "\n%d/%d unique strings, case insensitive, names as is:\n",
                     N_unq, N_words);
      for (i=0; i<N_words; ++i) {
         fprintf(stdout,"%d %s (ww[%d]=%s)\n",
                  i, ws[i]?ws[i]:"NULL - what follows is legit garbage", 
                  isrt[i], ww[isrt[i]]?ww[isrt[i]]:"NULL" );
      }
      if (isrt) free(isrt); isrt = NULL;
      for (i=0; i<N_words; ++i) if (ws[i]) free(ws[i]);
      free(ws); ws = NULL;
   }
   if ((ws = unique_str(ww, N_words, 1, 1, &N_unq, &isrt))) {
      fprintf(stdout,"\n%d/%d unique strings, case insensitive, noext\n",
                     N_unq, N_words);
      for (i=0; i<N_words; ++i) {
         fprintf(stdout,"%d %s (ww[%d]=%s)\n",
                  i, ws[i]?ws[i]:"NULL - what follows is legit garbage", 
                  isrt[i], ww[isrt[i]]?ww[isrt[i]]:"NULL" );
      }
      if (isrt) free(isrt); isrt = NULL;
      for (i=0; i<N_words; ++i) if (ws[i]) free(ws[i]);
      free(ws); ws = NULL;
   }
   if ((ws = unique_str(ww, N_words, 1, 2, &N_unq, &isrt))) {
      fprintf(stdout,
               "\n%d/%d unique strings, case insensitive, noext noview\n",
                     N_unq, N_words);
      for (i=0; i<N_words; ++i) {
         fprintf(stdout,"%d %s (ww[%d]=%s)\n",
                  i, ws[i]?ws[i]:"NULL - what follows is legit garbage", 
                  isrt[i], ww[isrt[i]]?ww[isrt[i]]:"NULL" );
      }
      if (isrt) free(isrt); isrt = NULL;
      for (i=0; i<N_words; ++i) if (ws[i]) free(ws[i]);
      free(ws); ws = NULL;
   }

   if ((ws = unique_str(ww, N_words, 0, 2, &N_unq, &isrt))) {
      fprintf(stdout,
               "\n%d/%d unique strings, case sensitive, noext noview\n",
                     N_unq, N_words);
      for (i=0; i<N_words; ++i) {
         fprintf(stdout,"%d %s (ww[%d]=%s)\n",
                  i, ws[i]?ws[i]:"NULL - what follows is legit garbage", 
                  isrt[i], ww[isrt[i]]?ww[isrt[i]]:"NULL" );
      }
      if (isrt) free(isrt); isrt = NULL;
      for (i=0; i<N_words; ++i) if (ws[i]) free(ws[i]);
      free(ws); ws = NULL;
   }

   for (i=0; i<N_words; ++i) if (ww[i]) free(ww[i]);
      free(ww); ww = NULL;
   return(0);
}

int main(int argc, char **argv)
{
   int iarg, N_ws, i, max_hits, test_only, new_score=0,
       i_unique_score=0, min_different_hits=0, unq_only=0,
       show_score=0, N_fnamev=0, MAX_FNAMES = 0, uopts=0,
       compcom = 0, shtp = 0;
   float *ws_score=NULL, last_score=-1.0;
   char *fname=NULL, *text=NULL, *prog=NULL, *word="Ma fich haga", **ws=NULL, 
         *all_popts=NULL, *popt=NULL, stdinflag[] = " [+.-STDIN-.+] ";
   THD_string_array *fnamev = NULL;
   APPROX_STR_DIFF *D=NULL;
   THD_string_array *sar = NULL;
   byte ci = 1;
   char *wild_list=NULL, **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL, wild_noext=0, spx_tar = 0,
       wild_all_files = 0, wild_orig_name = 0, wild_ci=0;
   

   mainENTRY("apsearch main"); machdep() ; 
      
   max_hits = 3;
   test_only=0;
   min_different_hits = -1;
   if (argc <= 1) {
      apsearch_usage(0);
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
         fprintf(stdout,"%s\n", THD_helpdir(0));
         return(0);
      }

      if (strcmp(argv[iarg],"-test_unique_str") == 0) { 
         Test_unique_str();
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_data_dir") == 0) { 
         fprintf(stdout,"%s\n", THD_datadir(0));
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_rc_file") == 0) { 
         fprintf(stdout,"%s\n", THD_afnirc());
         return(0);
      }
      
      if (strcmp(argv[iarg],"-afni_home_dir") == 0) { 
         fprintf(stdout,"%s\n", THD_homedir(0));
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_custom_atlas_dir") == 0) { 
         fprintf(stdout,"%s\n", THD_custom_atlas_dir(0));
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_custom_atlas_file") == 0) { 
         fprintf(stdout,"%s\n", THD_custom_atlas_file(NULL));
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_bin_dir") == 0) { 
         fprintf(stdout,"%s\n", THD_abindir(0));
         return(0);
      }
      
      if (strcmp(argv[iarg],"-apsearch_log_file") == 0) { 
         fprintf(stdout,"%s\n", THD_helpsearchlog(0));
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_text_editor") == 0) { 
         char *ss = GetAfniTextEditor();
         fprintf(stdout,"%s\n", ss?ss:"NONE FOUND");
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_web_downloader") == 0) { 
         char *ss = GetAfniWebDownloader();
         fprintf(stdout,"%s\n", ss?ss:"NONE FOUND");
         return(0);
      }

      if (strcmp(argv[iarg],"-afni_web_browser") == 0) { 
         char *ss = GetAfniWebBrowser();
         fprintf(stdout,"%s\n", ss?ss:"NONE FOUND");
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

      /* 3 Feb 2014 [rickr] */
      if (strcmp(argv[iarg],"-global_help") == 0 ||
          strcmp(argv[iarg],"-gopts_help") == 0) { 
         printf(
      "--------------------------------------------------------------------\n"
      "Global Options: options available to most AFNI programs, but usually\n"
      "                not found in the -help output.\n"
      "--------------------------------------------------------------------\n"
             "%s", get_gopt_help());
         return(0); 
      }

      if (strcmp(argv[iarg],"-help") == 0 ||
          strcmp(argv[iarg],"-h") == 0) { 
         apsearch_usage(strlen(argv[iarg]) > 3 ? 2:1);
         return(0); 
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

      if (strcmp(argv[iarg],"-sphinx_phelp") == 0) { 
         char *s=NULL;
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need a program name after -sphinx_phelp\n"); 
            return(1);
         }
         
         if ((s = sphinxize_prog_help(argv[iarg],0))) {
            fprintf(stdout, "%s", s);
            free(s); s = NULL;
         }
         ++iarg;
         return(0);
         continue; 
      }
      
      if (!strcmp(argv[iarg],"-doc_markup_sample")) {
         SUMA_Sphinx_String_Edit_Help(NULL);
         ++iarg;
         return(0);
         continue; 
      }
      
      if (!strcmp(argv[iarg],"-doc_2_txt")) {
         spx_tar = 1;
         ++iarg; continue;
      }
      
      if (!strcmp(argv[iarg],"-doc_2_spx")) {
         spx_tar = 2;
         ++iarg; continue;
      }
      
      if (strcmp(argv[iarg],"-files") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need text files after -files\n"); return(1);
         }
         while (iarg < argc && argv[iarg][0] != '-') {
            if ( ! fnamev ) INIT_SARR(fnamev);
            if (!THD_is_file(argv[iarg])) {
               ERROR_exit("Argument %s for -files is not a file on disk\n", 
                             argv[iarg]); 
            }
            ADDUTO_SARR(fnamev, argv[iarg]); 
            ++iarg;
         }
         continue; 
      }
      
      if (!strcmp(argv[iarg],"-wild_files_noAext")) {
         wild_noext = 1;
         ++iarg; continue;
      }
      
      if (!strcmp(argv[iarg],"-wild_files_noAext_noAview")) {
         wild_noext = 2;
         ++iarg; continue;
      }
      
      if (!strcmp(argv[iarg],"-wild_files_orig_name")) {
         wild_orig_name = 1;
         ++iarg; continue;
      }

      if (!strcmp(argv[iarg],"-wild_files_debug")) {
         wild_orig_name = -1;
         ++iarg; continue;
      }
      
      if (!strcmp(argv[iarg],"-wild_all_files")) {
         wild_all_files = 1;
         ++iarg; continue;
      }
      if (!strcmp(argv[iarg],"-wild_files_ci")) {
         wild_ci = 1;
         ++iarg; continue;
      }
      if (!strcmp(argv[iarg],"-wild_files")) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
               "** Error: Need wildcards files after -wild_files\n"); return(1);
         }
         while (iarg < argc && argv[iarg][0] != '-') {
            wild_list = 
               SUMA_append_replace_string(wild_list, argv[iarg], " ", 1); 
            ++iarg;
         }
         continue; 
      }
      
      if (strcmp(argv[iarg],"-stdin") == 0 || strcmp(argv[iarg],"-") == 0) { 
         fname = stdinflag;
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-view_prog_help") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need program name after -view_prog_help\n"); 
                     return(1);
         }
         view_prog_help(argv[iarg]);
         return(0);
         continue; 
      }
      
      if (strcmp(argv[iarg],"-web_prog_help") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need program name after -web_prog_help\n"); 
                     return(1);
         }
         web_prog_help(argv[iarg]);
         return(0);
         continue; 
      }
      
      if (strcmp(argv[iarg],"-web_class_docs") == 0) { 
         web_class_docs(NULL);
         return(0);
         continue; 
      }
      
      if (strcmp(argv[iarg],"-view_readme") == 0) { 
         char *rout=NULL, **ws=NULL;
         int N_ws, i;
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need README name after -view_readme\n"); 
                     return(1);
         }
         if ((rout = find_readme_file(argv[iarg]))) {
            view_text_file(rout); free(rout);
         } else {
            fprintf( stderr,
                     "** Error: Could not find solid match for readme %s\n"
               "Try to pick a good one using apsearch -list_all_afni_readmes\n",
                     argv[iarg]);
            ws = approx_str_sort_readmes(argv[iarg], &N_ws);
            if (N_ws) {
                  fprintf( stderr, "   Perhaps you're looking for:\n");
               for (i=0; i< N_ws; ++i) {
                  if (i<max_hits) fprintf( stderr, "   %d- %s\n", i, ws[i]);
                  free(ws[i]); 
               }
               free(ws);
            }
         }  
         return(0);
      }
      
      if (strcmp(argv[iarg],"-view_text_file") == 0) { 
         char *rout=NULL, **ws=NULL;
         int N_ws, i;
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need a filename after -view_text_file\n"); 
                     return(1);
         }
         if (!THD_is_file(argv[iarg])) {
            fprintf( stderr,
                     "** Error: File %s not found.\n", argv[iarg]); 
                     return(1);
         }
         view_text_file(argv[iarg]); 
         return(0);
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
         uopts = 0;
         max_hits = -1;
         word = "-";
         ++iarg; 
         continue; 
      }

      if (strcmp(argv[iarg],"-list_popts") == 0 ) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
               "** Error: Need program name after -list_popts\n"); return(1);
         }

         all_popts = argv[iarg];
         uopts = 1;
         max_hits = -1;
         word = "-";
         ++iarg; 
         continue; 
      }
      
      if (strcmp(argv[iarg],"-bash") == 0) {
         shtp = 1;
         ++iarg; 
         continue;
      }
      
      if (strcmp(argv[iarg],"-popts_complete_command") == 0 ) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
               "** Error: Need program name after -popts_complete_command\n"); 
            return(1);
         }

         all_popts = argv[iarg];
         if (1) {
            prog_complete_command(all_popts, NULL, shtp);
            return(0);
         }         

         uopts = 1;
         compcom = 1;
         max_hits = -1;
         word = "-";
         
         ++iarg; 
         continue; 
      }
      
      if (strcmp(argv[iarg],"-word") == 0 || strcmp(argv[iarg],"-w") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need text file after -file\n"); return(1);
         }

         word = argv[iarg];
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-max_hits") == 0 || strcmp(argv[iarg],"-m") == 0) { 
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
      
      if (strcmp(argv[iarg],"-update_all_afni_help") == 0) { 
         update_help_for_afni_programs(0, 1, 1, NULL); return(0);
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-recreate_all_afni_help") == 0) { 
         update_help_for_afni_programs(1, 1, 1, NULL); return(0);
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-all_afni_help") == 0) { 
         update_help_for_afni_programs(0, 0, 0, &fnamev); 
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-list_all_afni_progs") == 0) { 
         list_afni_programs(0, 0); return(0);
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-list_all_afni_P_progs") == 0) { 
         list_afni_programs(1, 0); return(0);
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-list_all_afni_readmes") == 0) { 
         list_afni_readmes(0, 0); return(0);
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-list_all_afni_P_readmes") == 0) { 
         list_afni_readmes(1, 0); return(0);
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-list_all_afni_dsets") == 0) { 
         list_afni_dsets(0, 0); return(0);
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-list_all_afni_P_dsets") == 0) { 
         list_afni_dsets(1, 0); return(0);
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

   if (wild_list) { /* some globbing action only */
      MCW_wildcards(wild_list, &nglob, &wglob);
      if (!wglob || nglob == 0) {
         SUMA_ifree(wild_list);
         exit(1); /* no files found */
      }
      if (wild_all_files) { /* No sorting */
         for (i=0; i<nglob; ++i) {
            fprintf(stdout,"%s\n", wglob[i]);
         }
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
         exit(0);
      }
      /* some sorting needed */
      if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                              &nsort, &isrt))) {
         if (wild_orig_name == -1) {/* verbose output */
            fprintf(stdout, "\n%d/%d unique strings, noext = %d:\n",  
                     nsort, nglob, wild_noext); 
            for (i=0; i<nsort; ++i) {
               fprintf(stdout,"%d %s (wglob[%d]=%s)\n",
                 i, wsort[i]?wsort[i]:"NULL - what followsort is legit garbage", 
                 isrt[i], wglob[isrt[i]]?wglob[isrt[i]]:"NULL" );
            }
         } else {
            if (wild_orig_name) { /* output original strings */
               for (i=0; i<nsort; ++i) {
                  fprintf(stdout,"%s\n", wglob[isrt[i]]);
               }
            } else { /* output trimmed strings */
               for (i=0; i<nsort; ++i) {
                  fprintf(stdout,"%s\n", wsort[i]);
               }
            }
         }
         if (isrt) free(isrt); isrt = NULL;
         for (i=0; i<nglob; ++i) if (wsort[i]) free(wsort[i]);
         free(wsort); wsort = NULL;
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
         exit(0);
      } else {
         ERROR_message("Failed to sort");
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
         exit(1);
      }
   }
   
   if ((fnamev || fname || text || prog || popt || all_popts)) {
      if (!strcmp(word,"Ma fich haga") && !spx_tar) {
         ERROR_message(
            "I'd search the world over for you, if only you gave me -word");
         return 1;
      }
      if (fnamev) {
         fprintf(stderr,"Have %d files\n", fnamev->num);
         if (spx_tar) {
            for (i=0; i<fnamev->num; ++i) {
               char *sso=SUMA_Sphinx_File_Edit(fnamev->ar[i], spx_tar-1, 0);
               if (sso) {
                  fprintf(stdout,"%s", sso); free(sso); sso = NULL;
               } else {
                  ERROR_message("Failed to sphinx edit string in %s", 
                                 fnamev->ar[i]);
                  return 1;
               }
            }
            return 0;
         } else {
            sar = approx_str_sort_Ntfile(
                        fnamev->ar, fnamev->num, word, ci, &ws_score,
                               NULL,
                               &D, 0, '\0');
            ws = sar->ar; N_ws = sar->num;
         }
      } else if (fname) {
         if (strcmp(fname,stdinflag)) {
            if (spx_tar) {
               char *sso=SUMA_Sphinx_File_Edit(fname, spx_tar-1, 0);
               if (sso) {
                  fprintf(stdout,"%s", sso); free(sso); sso = NULL;
               } else {
                  ERROR_message("Failed to sphinx edit string in %s", fname);
                  return 1;
               }
               return 0;
            } else {
               ws = approx_str_sort_tfile(fname, &N_ws, word, 
                         ci, &ws_score,
                         NULL, &D, 1, '\0');
            }
         } else {
            char *stdtext=NULL;
            if (!(stdtext = text_from_stdin(&N_ws))) {
               ERROR_message("Failed to read from stdin");
               return 0;
            }
            if (spx_tar) {
               stdtext = SUMA_Sphinx_String_Edit(&stdtext, spx_tar-1, 0);
               fprintf(stdout,"%s", stdtext);
               free(stdtext); stdtext=NULL;
               return 0;
            } else {
               ws = approx_str_sort_text(stdtext, &N_ws, word, 
                               ci, &ws_score,
                               NULL, &D, '\0');
               free(stdtext); stdtext=NULL;
            } 
         }
      } else if (text) {
         ws = approx_str_sort_text(text, &N_ws, word, 
                            ci, &ws_score,
                            NULL, &D, '\0');
      } else if (prog) {
         ws = approx_str_sort_phelp(prog, &N_ws, word, 
                            ci, &ws_score,
                            NULL, &D, 1, '\\');
      } else if (popt) {
         suggest_best_prog_option(popt, word);
         return 0;
      } else if (all_popts) {
         /* one can also use print_prog_options(all_popts); return(0); */
         ws = approx_str_sort_all_popts(all_popts, &N_ws, 
                            ci, &ws_score,
                            NULL, &D, uopts, 1, '\\');
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
                  switch(show_score) {
                     case 0:
                        fprintf(stdout,"   ");
                        if ((D+i)->srcfile && 
                            strncmp((D+i)->srcfile,APSEARCH_TMP_PREF,
                                             strlen(APSEARCH_TMP_PREF))) 
                           fprintf(stdout,"(%s) ",(D+i)->srcfile);
                        break;
                     case 1: 
                        fprintf(stdout,"%03f ", 
                                    ws_score[i]);
                        if ((D+i)->srcfile && 
                            strncmp((D+i)->srcfile,APSEARCH_TMP_PREF,
                                             strlen(APSEARCH_TMP_PREF))) 
                           fprintf(stdout,"(%s) ",(D+i)->srcfile);
                        break;
                     case 2:
                        fprintf(stdout,"%s ", 
                           approx_string_diff_info(D+i, NULL));
                        break;
                     default:
                        ERROR_exit("Bad show_score value");
                        break;
                  }
                  fprintf(stdout,"%s\n", ws[i]);
               }
            }
            free(ws[i]); ws[i]=NULL;
         }
      } free(ws);  if (ws_score) free(ws_score); ws_score=NULL;
      if (D) free(D); D=NULL;
   } 
   
   if (fnamev) DESTROY_SARR(fnamev); fnamev=NULL;
   return 0;  
}
