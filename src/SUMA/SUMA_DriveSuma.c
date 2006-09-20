/*USE This sample to start writing standalone programs.
Change DriveSuma to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_DriveSuma (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_DriveSuma"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: A template code for writing SUMA programs.\n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_DriveSuma_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_DriveSuma_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->com = NULL;
   Opt->N_com = 0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_DriveSuma(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-com") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->com = (char **)SUMA_realloc(Opt->com, sizeof(char *)*(Opt->N_com+1));
         Opt->com[Opt->N_com] = NULL;
         ++kar;
         do { 
            Opt->com[Opt->N_com] = SUMA_append_replace_string (Opt->com[Opt->N_com], argv[kar], " ", 1);
            ++kar;
            brk = NOPE;
            if ( kar >= argc ) brk = YUP;
            else if (strcmp(argv[kar], "-com") == 0) {
               --kar; brk = YUP;
            }
         } while (!brk);
         ++Opt->N_com;
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}

SUMA_SurfaceObject *SUMA_ShowSurfComToSO(char *com)
{
   static char FuncName[]={"SUMA_ShowSurfComToSO"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_GENERIC_ARGV_PARSE *pst=NULL;
   char **argt=NULL, *pos, *tp=NULL;
   int argtc = 0;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int  i = -1, ii, jj, kk, il, N_Spec=0;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   /* change com to a bunch of arguments */
   /* get the type */
   SUMA_GET_BETWEEN_BLANKS(com, NULL, pos);
   tp = NULL; SUMA_COPY_TO_STRING(com, pos, tp); com = pos;
   /* tp = SUMA_append_replace_string("-i_", tp, "", 2); */
   SUMA_LHv("Adding >>>%s<<<\n", tp);
   argt = (char **)SUMA_realloc(argt, sizeof(char *)*(argtc+2)); {
      argt[argtc] = SUMA_copy_string("drivesumacom"); ++argtc; 
      argt[argtc] = tp; tp = NULL; ++argtc;
   }
   /* get whatever else follows */
   while (com[0]) {
      SUMA_GET_BETWEEN_BLANKS(com, NULL, pos);
      tp=NULL;SUMA_COPY_TO_STRING(com, pos, tp); com = pos;
      SUMA_LHv("Adding >>>%s<<<\n", tp);
      argt = (char **)SUMA_realloc(argt, sizeof(char *)*(argtc+1)); argt[argtc] = tp; tp = NULL; ++argtc;
   }
   /* now parse these fake options */
   pst = SUMA_Parse_IO_Args(argtc, argt, "-i;-t;-spec;-sv;");
   if (LocalHead) SUMA_Show_IO_args(pst);
   
   if (pst->s_N_surfnames + pst->i_N_surfnames + pst->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. Only one surface allowed.");
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(pst, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
      exit(1);
   }

   /* read in one surface for now */
   SO = SUMA_Load_Spec_Surf(Spec, 0, pst->sv[0], 0);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   /* fix the trimmings */
   if (!SO->State) {SO->State = SUMA_copy_string("DC"); }
   if (!SO->Group) {SO->Group = SUMA_copy_string("DS"); }
   if (!SO->Label) {SO->Label = SUMA_copy_string("Benedictus"); }
   if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); }

   /* clean up */
   if (argt) {
      for (i=0; i<argtc; ++i) if (argt[i]) SUMA_free(argt[i]); 
      SUMA_free(argt); argt = NULL; argtc = 0;
   }
   if (pst) SUMA_FreeGenericArgParse(pst); pst = NULL;
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   
   SUMA_RETURN(SO);
}

SUMA_Boolean SUMA_ProcessCommand(char *com, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_ProcessCommand"};
   int i;
   char *act, *pos, *stp;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_SO_File_Type tp = SUMA_FT_NOT_SPECIFIED;
   SUMA_Boolean ans = NOPE;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   if (!com) { SUMA_S_Err("NULL command"); SUMA_RETURN(NOPE); }
   
   SUMA_GET_BETWEEN_BLANKS(com, NULL, pos);
   act = NULL;
   SUMA_COPY_TO_STRING(com, pos, act); com = pos;
   if (!act) { SUMA_S_Err("No action found"); SUMA_RETURN(NOPE); }
   
   ans = YUP;
   SUMA_TO_LOWER(act);
   if (strcmp((act), "show_surf") == 0) {
      SO = SUMA_ShowSurfComToSO(com);
      SUMA_LHv("Sending Surface %s\n", SO->Label); /* send the surface */
      SUMA_SendSumaNewSurface(SO, ps->cs);
   } else {
      fprintf(SUMA_STDERR, "Error %s: Action %s not supported.", FuncName, act);
      ans = NOPE;
   }
   
   if (act) SUMA_free(act);
   SUMA_RETURN(ans);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"DriveSuma"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   float ctr[3] = { 0.0, 0.0, 0.0};
   int cnt=0, i=0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-talk;");
   /* force talk option whether users specify it or not */
   ps->cs->talk_suma = 1;
   if (ps->cs->rps > 0) { ps->cs->nelps = (float)ps->cs->talk_suma * ps->cs->rps; }
   else { ps->cs->nelps = (float) ps->cs->talk_suma * -1.0; }

   if (argc < 0) {
      usage_DriveSuma(ps);
      exit (1);
   }
   
   Opt = SUMA_DriveSuma_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* open communication */
   SUMA_LH("Talking to suma");
   ps->cs->istream = SUMA_BRAINWRAP_LINE;
   ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2;
   ps->cs->kth = 1; /* make sure all surfaces get sent */
   if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
      SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
      ps->cs->Send = NOPE;
      ps->cs->afni_Send = NOPE;
      ps->cs->talk_suma = NOPE;
   } 

   
   #if 0 /* sample code for Ben Singer */
      /* create a surface of sorts, set up a few attributes */
      SO = SUMA_CreateIcosahedron (50.0, 12, ctr, "n", 1);
      if (!SO) { SUMA_S_Err("Failed to create Icosahedron"); exit(1); }
      if (!SO->State) {SO->State = SUMA_copy_string("DC"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("DS"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string("Benedictus"); }
      if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); }

      if (ps->cs->talk_suma) {   /* strcutre setup during program default options parsing */
         #if 0
         if (!SUMA_SendToAfni (ps->cs, NULL,  0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToAfni");
            ps->cs->afni_Send = NOPE;
            ps->cs->Send = NOPE;
         } 
         #endif
            SUMA_LH("Sending Ico"); /* send the surface */
            SUMA_SendSumaNewSurface(SO, ps->cs);

      }
   
      SUMA_LH("An example for modifying mesh and redisplaying");
      cnt = 0;
      while (cnt < 20) {
         /* Do some mesh action */
         for (i=0; i<SO->N_Node*SO->NodeDim; ++i) SO->NodeList[i] *= 0.8;
            /* recalculate surface normals */
            SUMA_RECOMPUTE_NORMALS(SO); 
            if (ps->cs->Send) {
               if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         ++cnt;
      }
   #else
   /* interpret command line commands */
   for (i=0; i<Opt->N_com; ++i) {
      if (LocalHead) {
         SUMA_LH("Have the following commands");
         fprintf(SUMA_STDERR,"Command %d: %s\n", i, Opt->com[i]);
      }
      if (!SUMA_ProcessCommand(Opt->com[i], ps)) {
         fprintf(SUMA_STDERR,"Warning %s: Failed in processing command\n%s\n", FuncName, Opt->com[i]); 
      }   
   }

   #endif
   
   SUMA_LH("Freedom");
   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
