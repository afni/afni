#include "SUMA_suma.h"

void usage_3dCRUISEtoAFNI (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_3dCRUISEtoAFNI"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: 3dCRUISEtoAFNI -input CRUISE_HEADER.dx\n"
               " Converts a CRUISE dataset defined by a heder in OpenDX format\n"
               " The conversion is based on sample data and information\n"
               " provided by Aaron Carass from JHU's IACL iacl.ece.jhu.edu\n" 
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_3dCRUISEtoAFNI_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_3dCRUISEtoAFNI(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input\n");
				exit (1);
			}
         Opt->in_name = argv[kar];
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need integer argument after -debug\n");
				exit (1);
			}
         Opt->debug = atoi(argv[kar]);
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


int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"3dCRUISEtoAFNI"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_OPEN_DX_STRUCT **dx = NULL;
   THD_3dim_dataset *dset=NULL;
   char *sto3d = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "");
   
   if (argc < 2) {
      usage_3dCRUISEtoAFNI(ps);
      exit (1);
   }
   
   Opt = SUMA_3dCRUISEtoAFNI_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   dset = EDIT_empty_copy( NULL ) ;
   tross_Make_History( "3dCRUISEtoAFNI" , argc,argv , dset) ;
   if (!(sto3d = SUMA_OpenDX_Read_CruiseVolHead(Opt->in_name, dset, 1))) {
      if (Opt->debug) SUMA_SL_Err("Failed in SUMA_OpenDX_Read_CruiseVolHead");
      exit(1);   
   }
   if (dset) {
      SUMA_LH("Writing Dset");
      DSET_write(dset) ;
      if (LocalHead) { 
         fprintf(SUMA_STDERR,"%s: Can use the following command to create dset with to3d:\n%s\n", FuncName,sto3d); 
      } 
   } else {
      /* the olde way */
      if (system(sto3d)) {
         fprintf(SUMA_STDERR, "Error %s: Failed while executing shell command:\n%s\n"
                              "Check to3d's error messages, and disk writing permissions.\n", FuncName, sto3d);
      }
   }
   
   
      
   if (sto3d) SUMA_free(sto3d); sto3d = NULL;
   if (dset) { DSET_delete(dset); dset = NULL; }
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
