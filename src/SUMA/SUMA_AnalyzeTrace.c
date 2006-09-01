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
      
      SUMA_ENTRY;
      
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
               "   -suma_c: FILE is a SUMA_*.c file. It is analyzed for functions that use SUMA_ RETURN \n"
               "            (typo on purpose to avoid being caught here) without ENTRY\n"
               "       Note: The file for this program has special strings (in comments at times)\n"
               "            to avoid false alarms when processing it.\n"
               "            \n"
               "\n"
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
   Opt->obj_type = 0;
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
      
      if (!brk && (strcmp(argv[kar], "-suma_c") == 0))
      {
         Opt->obj_type = 1;
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			/* Assume the rest is input data */
			while (kar < argc) {
            if (Opt->n_in_namev < SUMA_GENERIC_PROG_MAX_IN_NAME) {
               Opt->in_namev[Opt->n_in_namev] = argv[kar];
               ++Opt->n_in_namev; ++kar;
            } else {
               SUMA_S_Err("Too many input dsets on command line");
            }
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
   fprintf(SUMA_STDERR, "\n");
   
   return;
}

void SUMA_ShowFromTo(char *f, char *t, char *head){
   if (head) {
      fprintf(SUMA_STDERR, "%s", head);
   } else {
      fprintf(SUMA_STDERR, "Chunk in question:\n"
                           "------------------\n");
   }
   while (f<t) {
      fprintf(SUMA_STDERR, "%c", *f); ++f;
   }
   fprintf(SUMA_STDERR, "\n");
   return;
}

char *SUMA_NextEntry(char *ss, int *level, int *io, char *func, char *file, int *line, int *error) {
   static char FuncName[]={"SUMA_NextEntry"};
   char *ss_tmp = NULL;
   int cnt = 0, found = 0;
   double num = 0.0;
   char *ss_func = NULL, *ss_entry = NULL, *ss_level= NULL, *ss_init=NULL, ctmp;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   *level = -1;
   *io = 0;
   *line = -1;
   *error = -1;
   ss_init = ss;
   
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
      SUMA_ShowFromTo(ss_init, ss_level+100, "Could not find 'file=' at:");
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
      SUMA_ShowFromTo(ss_init, ss_level+100, "Could not find 'line=' at:");
      SUMA_RETURN(ss);
   }
   SUMA_ADVANCE_PAST_NUM(ss_tmp, num, found);
   if (!found) {
      SUMA_ShowFromTo(ss_init, ss_tmp, "Could not read line number at:");
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
   /* skip the muck until next + or - that is preceeded by new line*/
   ss = ss_level;
   while (*ss != '\0') {
      if (*ss == '+' || *ss == '-') {
         if (SUMA_IS_LINE_END(*(ss-1))) break;
      }
      /* fprintf(SUMA_STDERR,"%c   ", *ss); */ 
      ++ss;
   }
   /* scan for errors */
   ctmp = *ss; *ss = '\0';
   if (strstr(ss_init,"Error")) *error = 1;
   else *error = 0;
   
   *ss = ctmp;
   
   SUMA_RETURN(ss);
}

int SUMA_AnalyzeTraceFunc(char *fname, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt) {
   static char FuncName[]={"SUMA_AnalyzeTraceFunc"};
   char *fl = NULL, *flc = NULL, *fls = NULL, *flo = NULL, 
         *fln = NULL, *fle = NULL, func[100],  file[100],
         *comp_fl=NULL, stmp[300];
   int level, cur_level, io, nread, its, line, error, cnt, N_comp_fl, Nrep;
   SUMA_TRACE_STRUCT TS[100];
   SUMA_Boolean Res = NOPE;
   FILE *fff=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 1) LocalHead = YUP;
   
   its = 0;
   
   /* suck the trace */
   nread = SUMA_suck_file( fname , &fl ) ;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Read in %d chars\n", FuncName, nread);
   
   if (0) { /* not ready yet */
      comp_fl = (char *)SUMA_calloc(nread, sizeof(char));
      N_comp_fl = 0;
   }
   
   fle = fl+nread; /* end of string */
   fls = flc+50; /* set current stop location */
    
   /* go to first entry */
   cur_level = 0;
   func[0] = '\0';
   Nrep = 0;
   fln = fl;
   do {
      flc = fln; /* set current location */
      fln = SUMA_NextEntry(flc, &level, &io, func, file, &line, &error) ;
      if (fln == flc) {
         SUMA_S_Note("\nDone Checking.\nTrace Looks OK (exit() calls are not popped off the stack).\n");
         SUMA_ShowTraceStack(TS, its);
         Res = YUP;
         goto GETOUT;
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
               Res = NOPE;
               goto GETOUT;
            } else {
               cur_level = level;
               snprintf(TS[its].func, 99*sizeof(char),  "%s", func);
               snprintf(TS[its].file, 99*sizeof(char),  "%s", file);
               /*fprintf(SUMA_STDERR,">>>>>>>>>>>%s<<<<<<<\n", TS[its].func);*/
               TS[its].level = level;
               TS[its].io = io;
               TS[its].line = line;
               ++its;
               if (error) {
                  SUMA_ShowFromTo(flc, fln, "\n"
                                       "Encountered error here in this chunk:\n"
                                       "-------------------------------------\n");
                  SUMA_ShowTraceStack(TS, its);
                  fprintf(SUMA_STDERR, "\n\n");
               }
            }
         } else if (io == -1) { /* exit, make sure level is current and function is same */
            if (its < 1) {
                  fprintf(SUMA_STDERR, "Leaving function %s but with its = %d!\n", func, its); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln, NULL);
                  Res = NOPE;
                  goto GETOUT;
            } 
            if (level != cur_level) {
               fprintf(SUMA_STDERR, "Leaving level %d from current level of %d!\n", level, cur_level);
               /* Show me the trace */
               SUMA_ShowTraceStack(TS, its);
               SUMA_ShowFromTo(flc, fln, NULL);
               Res = NOPE;
               goto GETOUT;
            } else {
               /* make sure func at leaving is same as one entering */
               if (strcmp(func, TS[its-1].func) != 0) {
                  fprintf(SUMA_STDERR, "Leaving func %s from level current func %s, its = %d!\n", func, TS[its-1].func, its-1); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln, NULL);
                  Res = NOPE;
                  goto GETOUT;
               }  
               /* make sure file at leaving is same as one entering */
               if (strcmp(file, TS[its-1].file) != 0) {
                  fprintf(SUMA_STDERR, "Leaving purported function (%s) from file %s which is different from entry file %s, its = %d!\n",
                                        func, file, TS[its-1].func, its-1); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln, NULL);
                  Res = NOPE;
                  goto GETOUT;
               }
               /* make sure  leaving after entrance*/
               if (line < TS[its-1].line) {
                  fprintf(SUMA_STDERR, "Leaving purported function (%s) at line %d which is before entry line %d, its = %d!\n"
                                       "Check function that is returning in %s:%d , perhaps it has no SUMA_ENTRY (or ENTRY).\n",
                                        func, line, TS[its-1].line, its-1,
                                        file, line); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln, NULL);
                  Res = NOPE;
                  goto GETOUT;
               }
               if (line - TS[its-1].line > Opt->N_it) {
                  fprintf(SUMA_STDERR, "Note: Leaving purported function (%s) at line %d more than %d lines from entry line %d, its = %d!\n",
                                        func, line, Opt->N_it, TS[its-1].line, its-1); 
                  /* Show me the trace */
                  SUMA_ShowTraceStack(TS, its);
                  SUMA_ShowFromTo(flc, fln, NULL);
               }
               {
                  /* OK, cleanup last its*/
                  --its;
                  TS[its].func[0] = '\0';
                  TS[its].level = -1;
                  TS[its].io = 0;
                  cur_level = level - 1;
                  if (error) {
                     SUMA_ShowFromTo(flc, fln, "\n"
                                          "Encountered error here in this chunk:\n"
                                          "-------------------------------------\n");
                     SUMA_ShowTraceStack(TS, its);
                     fprintf(SUMA_STDERR, "\n\n");
                  }
               }
            }
         }   
      } else {
         SUMA_S_Err("Error in level!");
      }
   } while (fln > flc);
   
   GETOUT:
   /* seal comp_fl and write to disk */
   if (0){
      comp_fl[N_comp_fl] = '\0'; 
      fopen("CompactTrace","w");
      fprintf(fff,"%s",comp_fl);
      SUMA_free(comp_fl); comp_fl = NULL;
      fclose(fff); fff = NULL;
   }
   
   SUMA_free(fl); fl = NULL;
   
   SUMA_RETURN(Res);
   
}

char *SUMA_NextFunc(char *ss, char *sslim, int *io, char *func, char *file, int *line, int *error) {
   char *key[] = { "static", "char", "FuncName", "=", "{" , "}", ";", NULL};
   int max_gap[] = {  -1,     5,       5,         5,    5,   -1, 20, -1 };  
   /* this comment with SUMA_ENTRY  is placed here to avoid having the program trip on the next line
   Leave key as the first line .
   Same for this one SUMA_RETURN */
   static char FuncName[]={"SUMA_NextFunc"}; 
   char *ss_tmp = NULL;
   int cnt = 0, found = 0, ok=0;
   double num = 0.0;
   char *ss_func = NULL, *ss_entry = NULL, *ss_level= NULL, *ss_init=NULL, ctmp;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   *io = 0;
   *line = -1;
   *error = -1;
   ss_init = ss;
   
   #if 0   /* doing search via strstr */
   /* search from ss for key sequence */
   found = 0;
   ss_entry = ss;
   ok = 1;
   while (key[found] && ok) {
      if (LocalHead) fprintf(SUMA_STDERR,"key[%d]=%s ", found, key[found]); 
      ss_func = strstr(ss, key[found]);
      if (ss_func && ss_func >= ss) {
         if (LocalHead) fprintf(SUMA_STDERR," Found ");
         if (!found || max_gap[found] < 0 || ss_func - ss < max_gap[found]) { /* inside gap limit, OK */
            if (LocalHead) fprintf(SUMA_STDERR," in gap ");
            ss = ss_func+strlen(key[found]);
            if (!found) ss_entry = ss_func;
            ++found;
         } else {
            if (LocalHead) fprintf(SUMA_STDERR," out of gap (%d, Augment by %d) ", ss_func - ss, (int)strlen(key[0]));
            /* move SS past first key found */
            found = 0;
            ss = ss_entry + strlen(key[0]);
            ss_entry = ss;
         }
      } else {
         { 
            ok = 0;
         }
      } 
   }
   if (!ok) {
         SUMA_LH("Done.");
         SUMA_RETURN(ss_init);
   } else {
      if (ss_func) {
         ss = ss_func+strlen(key[found-1]);
      } else {
         SUMA_S_Err("WTF?");
         exit(1);
      }
   }
   #else /* doing search via more flexible macro to test the latter. (both should give same results)*/
   SUMA_ADVANCE_PAST_SEQUENCE(ss, sslim, ss_entry, key, max_gap,  found, 0);
   if (!found) {
      SUMA_LH("Done.");
      SUMA_RETURN(ss_init);
   } else {
      
   }
   #endif
   
   /* copy the function name */
   func[0] = '\0';
   cnt = 0;
   while (ss_entry < ss && cnt < 99) {
      func[cnt] = *ss_entry; ++ss_entry; ++cnt;
   } 
   func[cnt] = '\0';
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "\n"
                           "Next Function:\n"
                           "ss_entry -ss = %d\n"
                           "func = %s\n", ss_entry - ss, func);
   }      
   SUMA_RETURN(ss);
}

int SUMA_AnalyzeSumaFunc(char *fname, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt) {
   static char FuncName[]={"SUMA_AnalyzeSumaFunc"};
   char *fl = NULL, rkey[100], *flc = NULL, *fls = NULL, *flo = NULL, *fln = NULL, *fle = NULL, func[100], file[100], ctmp, *sret, *sent;
   int level, cur_level, io, nread, its, line, error;
   SUMA_TRACE_STRUCT TS[100];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 1) LocalHead = YUP;
   
   its = 0;
   
   /* suck the file */
   nread = SUMA_suck_file( fname , &fl ) ;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(NOPE);
   }

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Read in %d chars\n", FuncName, nread);
   
   fle = fl+nread; /* end of string */
   fls = flc+50; /* set current stop location */
    
   /* go to first entry */
   cur_level = 0;
   flc = fl;
   fln = SUMA_NextFunc(flc, fle, &io, func, file, &line, &error);
   do {
      flc = fln; /* set current location */
      fprintf(SUMA_STDERR,"Analyzing function %s, ", func);
      fln = SUMA_NextFunc(flc, fle, &io, func, file, &line, &error) ;
      fprintf(SUMA_STDERR,"(next function %s): ", func); 
      if (fln == flc) {
         /* No more functions*/ 
         fln = fle;
      }
      /* look for next RETURN */
      sprintf(rkey, "SUMA_RETURN");
      sret = strstr(flc, rkey);
      if ( !(sret > flc) || sret > fln) { /* look for exit */
         sprintf(rkey, "exit");
         sret = strstr(flc, rkey);
      }
      if (sret > flc && sret < fln) { /* OK, look for ENTRY in between */
         ctmp = *sret; *sret = '\0';
         sent = strstr(flc, "ENTRY");
         *sret = ctmp;
         if (sent > flc) { /* found entry, make sure it is before return */
            if (sent < sret) { /* function is OK */
               fprintf(SUMA_STDERR,"  OK\n");
            } else {
               fprintf(SUMA_STDERR,"  BAD\n");
               SUMA_S_Err("Function has RETURN (or exit) before ENTRY\n");
               SUMA_ShowFromTo(flc, sret, NULL);
               SUMA_RETURN(NOPE);
            }
         } else {
            if (strcmp(rkey, "SUMA_RETURN") == 0) {
               fprintf(SUMA_STDERR,"  Very BAD, RETURN with No ENTRY\n");
                  SUMA_ShowFromTo(flc, sret, NULL);
                  SUMA_RETURN(NOPE);
            } else if (strcmp(rkey, "exit") == 0) {
               fprintf(SUMA_STDERR,"  Naughty, exit with No ENTRY (use -debug 1 to see code chunk)\n");
                  if (Opt->debug) SUMA_ShowFromTo(flc, sret, NULL);
            } else {
               fprintf(SUMA_STDERR,"  STRANGE rkey=%s\n", rkey);
            }
         }
      } else {
         /* is there a return before the next function ? */
         sret = strstr(flc, "return");
         if (sret > flc && sret < fln) { /* Yes, make sure there is no ENTRY */
            ctmp = *sret; *sret = '\0';
            sent = strstr(flc, "ENTRY");
            *sret = ctmp;
            if (sent > flc) { /* found entry, make sure it NOT  before return */
               if (sent < sret) { /* using lower case return with ENTRY */
                  fprintf(SUMA_STDERR,"  using return with ENTRY!\n");
                  SUMA_ShowFromTo(flc, fln, NULL);
                  SUMA_RETURN(NOPE); 
               } else {
                  fprintf(SUMA_STDERR,"  Note: not using ENTRY or RETURN\n");
               }
            } else {
               fprintf(SUMA_STDERR,"  Note: not using ENTRY or RETURN\n");
            }
         } else { 
            fprintf(SUMA_STDERR,"  No RETURN or exit here\n");

            SUMA_ShowFromTo(flc, fln, NULL);
            SUMA_RETURN(NOPE);
         }
      } 
      
   } while (fln > flc && fln < fle);
   
   SUMA_RETURN(YUP);
   
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"AnalyzeTrace"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;");
   
   if (argc < 2) {
      usage_AnalyzeTrace(ps);
      exit (1);
   }
   
   Opt = SUMA_AnalyzeTrace_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   for (i=0; i<Opt->n_in_namev; ++i) {
      if (Opt->obj_type == 0) {
         fprintf( SUMA_STDOUT,
                  "\n"
                  "Processing file %s\n"
                  , Opt->in_namev[i]);
         if (!SUMA_AnalyzeTraceFunc(Opt->in_namev[i], Opt)) break;
      } else {
         fprintf( SUMA_STDOUT,
                  "\n"
                  "Processing file %s\n"
                  , Opt->in_namev[i]);
         if (!SUMA_AnalyzeSumaFunc(Opt->in_namev[i], Opt)) break;
      }
   }

   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
