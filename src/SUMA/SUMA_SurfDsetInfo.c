#include "SUMA_suma.h"

void usage_SurfDsetInfo (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfDsetInfo"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL, *sd = NULL;
      int i;

      SUMA_ENTRY;

      s = SUMA_help_basics();
      sd = SUMA_help_dset();
      printf ( "\n"
               "Usage: SurfDsetInfo [options] -input DSET1 -input DSET2 ...\n"
               "   or: SurfDsetInfo [options] DSET1 DSET2 ... \n"
               "   Optional Params:\n"
               "      -debug DBG: if DBG = 2, show dset->ngr in its entirety in NIML form.\n"
               "\n"
               "%s"
               "\n"
               "%s"
               "\n",  sd, s);
      if (s) SUMA_free(s); s = NULL; if (st) SUMA_free(st); st = NULL; if (sio) SUMA_free(sio); sio = NULL; if (sd) SUMA_free(sd);
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *
   SUMA_SurfDsetInfo_ParseInput(char *argv[], int argc,
                                SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfDsetInfo_ParseInput"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->ps = ps;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfDsetInfo(ps);
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

      if (!brk && !ps->arg_checked[kar]) {
			/* Assume the rest is input data */
			while (kar < argc) {
            if (Opt->ps->N_dsetname < SUMA_MAX_DSET_ON_COMMAND) {
               Opt->ps->dsetname[Opt->ps->N_dsetname] =
                                       SUMA_copy_string(argv[kar]);
               ++Opt->ps->N_dsetname; ++kar;
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

int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SurfDsetInfo"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-dset;");

   if (argc < 2) {
      usage_SurfDsetInfo(ps);
      exit (1);
   }

   Opt = SUMA_SurfDsetInfo_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;

   for (i=0; i<Opt->ps->N_dsetname; ++i) {
      fprintf(SUMA_STDOUT, "\n"
                           "Info for dset (%2d/%2d): %s\n"
                           "------------------------\n",
                            i+1, Opt->ps->N_dsetname, Opt->ps->dsetname[i]);

      if (!(dset = SUMA_LoadDset_s (Opt->ps->dsetname[i], &iform, 0))) {
         SUMA_S_Err("Failed reading dset");
         exit(1);
      }

      if (Opt->debug < 2) SUMA_ShowDset(dset, 0, SUMA_STDOUT);
      else {
         SUMA_ShowNel((void*)dset->ngr);
      }
      if (dset) SUMA_FreeDset(dset); dset = NULL;
   }
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);

}
