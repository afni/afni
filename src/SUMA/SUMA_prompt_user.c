#include "SUMA_suma.h"

/* local proto */
static char * read_file_text(FILE * fp);  /* 29 Jun 2012 [rickr] */

void usage_prompt_user (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_prompt_user"};
      int i;
      printf ( 
         "\n"
         "Mostly replaced by prompt_popup for more customization.\n"
         "Usage: prompt_user <-pause MESSAGE> \n"
         "  -pause MESSAGE: Pops a window prompting the user with MESSAGE.\n"
         "                  Program does not return until user responds.\n"
         "                  note: if MESSAGE is '-', it is read from stdin\n"
         "  -timeout TT: Timeout in seconds of prompt message. Default answer\n"
         "               is returned if TT seconds elapse without user\n"
         "               input.\n"
         "  -to TT: Same as -timeout TT\n"
         "\n");

      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_prompt_user_ParseInput(
   char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_prompt_user_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct(); 
   Opt->ps = ps;  /* just hold it there for convenience */
   Opt->flt1 = -1.0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 ps->hverb = 1;
          usage_prompt_user(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-pause") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -pause \n");
            exit (1);
         }
         
         Opt->b1 = 1;
         Opt->in_name = argv[++kar];
         brk = YUP;

         if (!strcmp(Opt->in_name, "-")) Opt->in_name = read_file_text(stdin);
      }

      if (!brk && (
            (strcmp(argv[kar], "-timeout") == 0) ||
            (strcmp(argv[kar], "-to") == 0)) )
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, 
                     "need a time in seconds after -timeout/-to \n");
            exit (1);
         }
         
         Opt->flt1 = atof(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			SUMA_S_Errv("Option %s not understood.\n"
                     "Try -help for usage\n", 
                     argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}

/* return all of fp (stdin, probably) in a string */
static char * read_file_text(FILE * fp)
{
   static char FuncName[]={"read_file_text"}; 
   char * str, ch;
   int    i, len, nalloc;

   SUMA_ENTRY;

   if ( ! fp ) SUMA_RETURN(NULL);

   str = NULL;
   len = 0;
   nalloc = 1;  /* add space for nul term */
   while ( ! feof(fp) ) {
      nalloc += 100; /* read block size */
      str = realloc(str, nalloc * sizeof(char));
      if( !str ) {
         fprintf(stderr,"** RFT alloc fail on len %d\n", nalloc);
         SUMA_RETURN(NULL);
      }
      for( i=0; i < 100 && !feof(fp); i++ )
         str[len++] = fgetc(fp);
      if( feof(fp) ) len--;
   }
   str[len] = '\0'; /* terminate */

   SUMA_RETURN(str);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"prompt_user"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   char * esc_str = NULL;
   int ii;
   Widget w=NULL;
   XtAppContext    app;
   XEvent ev;
   XtInputMask pp;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "");
   
   if (argc < 2) {
      usage_prompt_user(ps);
      exit (1);
   }
   
   Opt = SUMA_prompt_user_ParseInput (argv, argc, ps);

   w = XtOpenApplication(&app, "prompt_user", 
                           NULL, 0, &argc, argv,
                           SUMA_get_fallbackResources(), 
                           topLevelShellWidgetClass, NULL, 0); 
   
   switch (Opt->b1) {
      case 1:
         /* apply some escape characters     31 Jul 2009 [rickr] */
         esc_str = unescape_unix_str(Opt->in_name);
         ii = SUMA_PauseForUser(w, esc_str, SWP_POINTER_LEFT_BOTTOM, 
                                &app, 1, Opt->flt1);
         fprintf(SUMA_STDOUT,"%d\n", ii);
         break;
      default:
         SUMA_S_Err("Bad opt");
         exit(1);
         
   }
   
   /* because you have no XtAppMainLoop, you'll need to process the next 
   event for when the XtDestroy command on w's child takes effect. So you'll
   just have this zombie widget that stares at you.
   In this simple command line program, the widget dies anyway when you
   exit the program, so the call below is a teaching moment for when
   functions like SUMA_PauseForUser are called from programs without an
   XtAppMainLoop. 
   See also SUMA_PAUSE_PROMPT macro */
   
   while ((pp = XtAppPending(app))) {  
      XtAppProcessEvent(app, pp);
   } 
   
   if (Opt->debug > 2) LocalHead = YUP;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   if( esc_str ) free(esc_str);
   
   exit(0);
   
} 
