#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_prompt_user (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_prompt_user"};
      int i;
      printf ( 
         "\n"
         "Usage: prompt_user <-pause MESSAGE> \n"
         "  -pause MESSAGE: Pops a window prompting the user with MESSAGE.\n"
         "                  Program does not return until user responds.\n"
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
   Opt->ps = ps; /* for convenience */
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
         ii = SUMA_PauseForUser(w, esc_str, SWP_POINTER_OFF, &app, 1);
         fprintf(SUMA_STDOUT,"%d\n", ii);
         break;
      default:
         SUMA_S_Err("Bad opt");
         exit(1);
         
   }
   
   /* because you have no XtAppMainLoop, you'll need to process the next 
   event for the XtDestroy command on w's child takes effect. So you'll
   just have this zombie widget that stares at you.
   In this simple command line program, the widget dies anyway when you
   exit the program, so the call below is a teaching moment for when
   functions like SUMA_PauseForUser are called from programs without an
   XtAppMainLoop. 
   See also SUMA_PAUSE_PROMPT macro */
   
   while ((pp = XtAppPending(app))) {  \
      XtAppProcessEvent(app, pp); \
   } 
   
   if (Opt->debug > 2) LocalHead = YUP;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   if( esc_str ) free(esc_str);
   
   exit(0);
   
} 
