   
/* Header FILES */
#include "SUMA_suma.h"
   
#if 1
void usageParseName_Main ()
  {/*Usage*/
printf (
   "\n"
   "Usage:  ParseName [OPTIONS] <FName> \n"
   "Parses filename FName into components useful for AFNI\n"
   "The program makes no attempt to check that the file exists.\n"
   "OPTIONS:\n"
   "   -cwd: Specify the working directory, from which relative\n"
   "         path is constructed. Default is the program's CWD"
   "   -out OUT: Output only one component of the parsed file name\n"
   "             By default the whole parsed filename structure is\n"
   "             displayed.\n"
   "             OUT is one of the following:\n"
   "        FullName: ABSOLUTE_PATH/FName\n"
   "        RelName : RELATIVE_PATH/FName\n"
   "        AbsPath : ABSOLUTE_PATH/\n"
   "        RelPath : RELATIVE_PATH/\n"
   "\n"
   "Tests:\n"
   "    ParseName -cwd /hello/Joe /hello/Joe/afni.c\n"
   "    ParseName -cwd /hello/Joe/ /hello/Jane/afni.c\n"
   "\n"
   "     Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n"
   "\n");
   exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ParseName"}; 
	char *out=NULL,*FName=NULL, *cwd=NULL;
   int kar, brk;
   SUMA_PARSED_NAME *Test;
   
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		SUMA_S_Err("Failed in SUMA_Create_CommonFields\n");
		exit(1);
	}

   
   if (argc < 2) {
      usageParseName_Main ();
      exit (0);
   }
      
   kar = 1;
   cwd = NULL;
   out = NULL;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          usageParseName_Main ();
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-cwd") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a path after -cwd \n");
            exit (1);
         }
         
         cwd = argv[++kar];
         if (cwd[0] != '/') {
            SUMA_S_Errv("-cwd must specify an abosulte directory path\n"
                        "Have %s on command line.\n", cwd);
            exit(1);
         }
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-out") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -out \n");
            exit (1);
         }
         
         out = argv[++kar];
         brk = YUP;
      }

      
      if (!brk) {
			if (kar+1 == argc) {
            FName = argv[kar];
            ++kar;
         } else {
            SUMA_S_Errv("Option %s not understood.\n"
                        "FName must be the last option on command line .\n"
                        "Try -help for usage\n", 
                        argv[kar]);
			   exit (1);
		   }
      } else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (!FName) {
      SUMA_S_Err("No FName, nothing to do");
      exit(1);
   }
   if (!cwd) cwd = SUMAg_CF->cwd;
	
   if (!(Test = SUMA_ParseFname (FName, cwd))) {
      SUMA_S_Errv("Failed to parse %s, cwd %s\n", FName, cwd);
      exit(1);
   }
   
   if (out) {
      if (strcmp(out,"RelName") == 0) {
         fprintf(SUMA_STDOUT, "%s%s\n", 
                     Test->RelPath,Test->FileName);
      } else if (strcmp(out,"FullName") == 0) {
         fprintf(SUMA_STDOUT, "%s\n", 
                     Test->FullName);
      } else if (strcmp(out,"AbsPath") == 0) {
         fprintf(SUMA_STDOUT, "%s\n", 
                     Test->AbsPath);
      } else if (strcmp(out,"RelPath") == 0) {
         fprintf(SUMA_STDOUT, "%s\n", 
                     Test->RelPath);
      } else {
         SUMA_S_Errv("Bad -out option of %s\n", out);
         exit(1);
      }
   } else {
      SUMA_ShowParsedFname(Test, NULL);
   }
   
   if (Test) SUMA_Free_Parsed_Name (Test);
   
   exit (0);
}/* Main */
#endif

