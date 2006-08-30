/*USE This sample to start writing standalone programs.
Change AnalyzeTrace to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_AnalyzeTrace (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_AnalyzeTrace"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: A program to analyze SUMA (and AFNI's perhaps) stack output\n"
               "       The program can detect functions that return with RETURN without\n"
               "       bothering to go on the stack.\n" 
               "   AnaylzeTrace [options] FILE \n"
               "       where FILE is obtained by redirecting program's trace output.\n" 
               "Optional Param:\n"
               "   -max_func_lines N: Set the maximum number of code lines before a function\n"
               "                      returns. Default is no limit.\n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_AnalyzeTrace_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_AnalyzeTrace_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->N_it = 10000000;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_AnalyzeTrace(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-max_func_lines") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -max_func_lines \n");
            exit (1);
         }
         
         Opt->N_it = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			if (kar != argc-1) {
            fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood (kar=%d, argc=%d). Try -help for usage\n", FuncName, argv[kar], kar, argc);
			   exit (1);
         } else {
            Opt->in_name = argv[kar];
            ++kar;
         }
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}

typedef struct {
   char func[100];
   char file[100];
   int line;
   int level;
   int io;
} SUMA_TRACE_STRUCT;

char *SUMA_NextEntry(char *ss, int *level, int *io, char *func, char *file, int *line) {
   static char FuncName[]={"SUMA_NextEntry"};
   char *ss_tmp = NULL;
   int cnt = 0, found = 0;
   double num = 0.0;
   char *ss_func = NULL, *ss_entry = NULL, *ss_level= NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   *level = -1;
   *io = 0;
   
   /* search from ss for a + or a - */
   cnt = 0;
   while (ss[cnt]) {
      if (ss[cnt] == '+' || ss[cnt] == '-') break;
      ++cnt;
   }
   
   if (ss[cnt] == '+') {
      *io = 1;
   } else if (ss[cnt] == '-') {
      *io = -1;
   } else {
      *io = 0;
      SUMA_RETURN(ss); /* Failed to find an entry */
   }
   
   ss_entry = ss+cnt; /* beginning of entry line */
   
   /* have entry, skip to funcname */
   ss_func = ss_entry;
   while ( (*ss_func == '+' || *ss_func == '-') && *ss_func != '\0') ++ss_func;
   
   /* get level */
   ss_level = ss_func;
   while (*ss_level != '[' && *ss_level != '\0') ++ss_level;
   SUMA_ADVANCE_PAST_INT(ss_level, *level, found);
   if (!found) {
      SUMA_S_Err("Could not read level");
      SUMA_RETURN(ss);
   }
   /* copy the function name */
   func[0] = '\0';
   cnt = 0;
   while (ss_func <= ss_level && cnt < 99) {
      func[cnt] = *ss_func; ++ss_func; ++cnt;
   } 
   func[cnt] = '\0';
   
   if (func[0] == '\0') {
      fprintf(SUMA_STDERR, "%s:\n"
                           "func is empty at:\n", FuncName);
      ss_tmp = ss_entry;
      while (ss_tmp < ss_level) { fprintf(SUMA_STDERR, "%c", *ss_tmp); ++ss_tmp; }
      fprintf(SUMA_STDERR, "\n");
   }
   
   /* get the file name */
   ss_tmp = ss_level;
   SUMA_ADVANCE_PAST(ss_tmp, (ss_level+100), "file=", found, 0);
   if (!found) {
      SUMA_S_Err("Could not read file");
      SUMA_RETURN(ss);
   }
   file[0] = '\0';
   cnt = 0;
   while (cnt < 99) {
      if (*ss_tmp == '\0' || SUMA_IS_BLANK(*ss_tmp)) break;
      file[cnt] = *ss_tmp; ++ss_tmp; ++cnt;
   } 
   file[cnt] = '\0';
   
   /* get the line */
   SUMA_ADVANCE_PAST(ss_tmp, (ss_level+100), "line=", found, 0);
   if (!found) {
      SUMA_S_Err("Could not read line");
      SUMA_RETURN(ss);
   }
   SUMA_ADVANCE_PAST_NUM(ss_tmp, num, found);
   if (!found) {
      SUMA_S_Err("Could not read line");
      SUMA_RETURN(ss);
   }
   *line = (int)num;
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "funcname: %s\n"
                           "file: %s\n"
                           "line: %d\n"
                           "io: %d\n"
                           "level: %d\n", FuncName, func, file, *line, *io, *level);
                          
   }
   /* skip the muck until next + or - */
   ss = ss_level;
   while (*ss != '\0') {
      if (*ss == '+' || *ss == '-') break;
      /* fprintf(SUMA_STDERR,"%c   ", *ss); */ 
      ++ss;
   }
   
   SUMA_RETURN(ss);
}

void SUMA_ShowTraceStack(SUMA_TRACE_STRUCT *TS, int its) {
   int i, j;
   fprintf(SUMA_STDERR, "Current Stack:\n"
                        "--------------\n");
   for (i=0; i<its; ++i) {
      for (j=0; j<i; ++j) { fprintf(SUMA_STDERR, "  "); }
      fprintf(SUMA_STDERR, "func %s: file %s: line %d: level %d: io %d\n", TS[i].func, TS[i].file, TS[i].line, TS[i].level, TS[i].io);
   } 
   if (its==0) {
      fprintf(SUMA_STDERR, "Stack empty.\n");
   } 
   return;
}

void SUMA_ShowFromTo(char *f, char *t){
   fprintf(SUMA_STDERR, "Chunk in question:\n"
                        "------------------\n");
   while (f<t) {
      fprintf(SUMA_STDERR, "%c", *f); ++f;
   }
   fprintf(SUMA_STDERR, "\n");
   return;
}
int SUMA_AnalyzeTraceFunc(SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt) {
   static char FuncName[]={"SUMA_AnalyzeTraceFunc"};
   char *fl = NULL, *flc = NULL, *fls = NULL, *flo = NULL, *fln = NULL, *fle = NULL, func[100], file[100];
   int level, cur_level, io, nread, its, line;
   SUMA_TRACE_STRUCT TS[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 1) LocalHead = YUP;
   
   its = 0;
   
   /* suck the trace */
   nread = SUMA_suck_file( Opt->in_name , &fl ) ;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Read in %d chars\n", FuncName, nread);
   
   fle = fl+nread; /* end of string */
   fls = flc+50; /* set current stop location */
    
   /* go to first entry */
   cur_level = 0;
   fln = fl;
   do {
      flc = fln; /* set current location */
      fln = SUMA_NextEntry(flc, &level, &io, func, file, &line) ;
      if (fln == flc) {
         SUMA_S_Note("Done with analysis.");
         SUMA_ShowTraceStack(TS, its);
         SUMA_RETURN(YUP);
      }
      if (cur_level == 0) {
         /* first time */
         if (io != 1) {
            SUMA_S_Err("First entry is neg!");
         } else {
            cur_level = level-1; /* initialization */ 
         }
      }
      if (level > 0) {
         if (io == 1) { /* entry, make sure it is more than current level */
            if (level != cur_level + 1) {
               fprintf(SUMA_STDERR, "Entering level %d from current level of %d!\n", level, cur_level);
               /* Show me the trace */
               SUMA_ShowTraceStack(TS, its);
               SUMA_RETURN(NOPE);
            } else {
               cur_level = level;
               snprintf(TS[its].func, 99*sizeof(char),  "%s", func);
               snprintf(TS[its].file, 99*sizeof(char),  "%s", file);
               /*fprintf(SUMA_STDERR,">>>>>>>>>>>%s<<<<<<<\n", TS[its].func);*/
               TS[its].level = level;
               TS[its].io = io;
               TS[its].line = line;
               ++its;
            }
         } else if (io == -1) { /* exit, make sure level is current and function is same */
            if (its < 1) {
                  fprintf(SUMA_STDERR, "Leaving function %s but with its = %d!\n", func, its); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln);
                  SUMA_RETURN(NOPE);
            } 
            if (level != cur_level) {
               fprintf(SUMA_STDERR, "Leaving level %d from current level of %d!\n", level, cur_level);
               /* Show me the trace */
               SUMA_ShowTraceStack(TS, its);
               SUMA_ShowFromTo(flc, fln);
               SUMA_RETURN(NOPE);
            } else {
               /* make sure func at leaving is same as one entering */
               if (strcmp(func, TS[its-1].func) != 0) {
                  fprintf(SUMA_STDERR, "Leaving func %s from level current func %s, its = %d!\n", func, TS[its-1].func, its-1); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln);
                  SUMA_RETURN(NOPE);
               }  
               /* make sure file at leaving is same as one entering */
               if (strcmp(file, TS[its-1].file) != 0) {
                  fprintf(SUMA_STDERR, "Leaving purported function (%s) from file %s which is different from entry file %s, its = %d!\n",
                                        func, file, TS[its-1].func, its-1); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln);
                  SUMA_RETURN(NOPE);
               }
               /* make sure  leaving after entrance*/
               if (line < TS[its-1].line) {
                  fprintf(SUMA_STDERR, "Leaving purported function (%s) at line %d which is before entry line %d, its = %d!\n"
                                       "Check function that is returning in %s:%d , perhaps it has no SUMA_ENTRY (or ENTRY).\n",
                                        func, line, TS[its-1].line, its-1,
                                        file, line); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln);
                  SUMA_RETURN(NOPE);
               }
               if (line - TS[its-1].line > Opt->N_it) {
                  fprintf(SUMA_STDERR, "Note: Leaving purported function (%s) at line %d more than %d lines from entry line %d, its = %d!\n",
                                        func, line, Opt->N_it, TS[its-1].line, its-1); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln);
               }
               {
                  /* OK, cleanup last its*/
                  --its;
                  TS[its].func[0] = '\0';
                  TS[its].level = -1;
                  TS[its].io = 0;
                  cur_level = level - 1;
               }
            }
         }   
      } else {
         SUMA_S_Err("Error in level!");
      }
   } while (fln > flc);
   
   SUMA_RETURN(YUP);
   
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"AnalyzeTrace"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;");
   
   if (argc < 2) {
      usage_AnalyzeTrace(ps);
      exit (1);
   }
   
   Opt = SUMA_AnalyzeTrace_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   SUMA_AnalyzeTraceFunc(Opt);
   

   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
