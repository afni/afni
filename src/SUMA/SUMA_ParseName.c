   
/* Header FILES */
#include "SUMA_suma.h"

#if 1
void usageParseName_Main ()
  {/*Usage*/
printf (
   "\n"
   "Usage:  ParseName [OPTIONS] <FName> \n"
   "Parses filename FName into components useful for AFNI\n"
   "OPTIONS:\n"
   "   -cwd: Specify the working directory, from which relative\n"
   "         path is constructed. Default is the program's CWD\n"
   "   -pre PRE: Change the name so that you prepend PRE to the prefix\n"
   "   -app APP: Change the name so that you append APP to the prefix\n"
   "\n"
   "   -out OUT: Output only one component of the parsed file name\n"
   "             By default the whole parsed filename structure is\n"
   "             displayed.\n"
   "             OUT is one of the following:\n"
   "        FullName: ABSOLUTE_PATH/FName\n"
   "        RelName : RELATIVE_PATH/FName\n"
   "        AbsPath : ABSOLUTE_PATH/\n"
   "        RelPath : RELATIVE_PATH/\n"
   "        HeadName: RELATIVE_PATH/HEADNAME\n"
   "        Prefix  : PREFIX\n"
   "        uPrefix : USER_PATH/PREFIX\n"
   "        pPrefix : RELATIVE_PATH/PREFIX\n"
   "        PPrefix : ABSOLUTE_PATH/PREFIX\n"
   "        *PrefixView: Append view string (if any) to all prefix options\n"
   "                     listed above.\n"
   "        OnDisk  : 1 if file is on disk, 0 otherwise\n"
   "        FName   : Filename, no paths\n"
   "        FNameNoAfniExt : File name without any AFNI extensions\n"
   "                         e.g.: ParseName -out FNameNoAfniExt test.nii.gz\n"
   "        trim    : Trim the name to 20 characters.\n"
   "                  First the path goes, then extension, then view,\n"
   "                  then characters from the left. '~' indicates clipping.\n"
   "     If you want to output multiple parameters, list them all between \n"
   "     quotes with something like:\n"
   "        -out 'HeadName RelPath'\n"
   "\n"
   "  -outsep SEP: When outputing multiple components, use SEP as a separator\n"
   "               between them. Default is ' ', one space character\n"
   "\n"
   "Tests:\n"
   "    ParseName -cwd /hello/Joe /hello/Joe/afni.c\n"
   "    ParseName -cwd /hello/Joe/ /hello/Jane/afni.c\n"
   "    ParseName -out Prefix something.nii\n"
   "    ParseName -out uPrefixView something.nii\n"
   "    ParseName -out uPrefixView something+orig\n"
   "    ParseName -pre Need_ -out Prefix something.nii\n"
   "    ParseName -pre Need_  something.nii'[65-88]'\n"
   "    ParseName -pre Need_  something+orig.HEAD'{2-10}[4-6]'\n"
   "    ParseName -pre Need_ -out HeadName  something+orig.HEAD'{2-10}[4-6]'\n"
   "\n"
   "     Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n"
   "\n");
   exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ParseName"}; 
	char *out=NULL,*FName=NULL, *cwd=NULL, *what=NULL, *val=NULL, *sep=NULL;
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
   what = NULL;
   val = NULL;
   sep = " "; /* separator for multiple outs */
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

      if (!brk && (strcmp(argv[kar], "-outsep") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -outsep \n");
            exit (1);
         }
         
         sep = argv[++kar];
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-pre") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -pre \n");
            exit (1);
         }
         what = "prepend";
         val = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-app") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -app \n");
            exit (1);
         }
         what = "append";
         val = argv[++kar];
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
			   suggest_best_prog_option(argv[0], argv[kar]);
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
	
   
   if (what) {
      if (!(Test = SUMA_ParseModifyName(FName, what, val, cwd))) {
          SUMA_S_Errv("Failed to parse %s, cwd %s, what %s, val %s\n", 
               FName, cwd, what, val);
         exit(1);
      }
   } else {
      if (!(Test = SUMA_ParseFname (FName, cwd))) {
         SUMA_S_Errv("Failed to parse %s, cwd %s\n", FName, cwd);
         exit(1);
      }
   }
   
   if (out) {
      int kk=0;
      NI_str_array *nisa = NULL;
      nisa = SUMA_comp_str_2_NI_str_ar(out, " ");
      for (kk=0; kk<nisa->num; ++kk) {
         out = nisa->str[kk];
         if (kk > 0) fprintf(SUMA_STDOUT, "%s", sep);
         if (strcmp(out,"RelName") == 0) {
            fprintf(SUMA_STDOUT, "%s%s", 
                        Test->RelPath,Test->FileName);
         } else if (strcmp(out,"FullName") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->FullName);
         } else if (strcmp(out,"AbsPath") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->AbsPath);
         } else if (strcmp(out,"RelPath") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->RelPath);
         } else if (strcmp(out,"Prefix") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->Prefix);
         } else if (strcmp(out,"PrefixView") == 0) {
            fprintf(SUMA_STDOUT, "%s%s", 
                        Test->Prefix, Test->View);
         } else if (strcmp(out,"uPrefix") == 0) {
            fprintf(SUMA_STDOUT, "%s%s", 
                        Test->Path,Test->Prefix);
         } else if (strcmp(out,"uPrefixView") == 0) {
            fprintf(SUMA_STDOUT, "%s%s%s", 
                        Test->Path,Test->Prefix, Test->View);
         } else if (strcmp(out,"PPrefix") == 0) {
            fprintf(SUMA_STDOUT, "%s%s", 
                        Test->AbsPath,Test->Prefix);
         } else if (strcmp(out,"PPrefixView") == 0) {
            fprintf(SUMA_STDOUT, "%s%s%s", 
                        Test->AbsPath,Test->Prefix, Test->View);
         } else if (strcmp(out,"pPrefix") == 0) {
            fprintf(SUMA_STDOUT, "%s%s", 
                        Test->RelPath,Test->Prefix);
         } else if (strcmp(out,"pPrefixView") == 0) {
            fprintf(SUMA_STDOUT, "%s%s%s", 
                        Test->RelPath,Test->Prefix, Test->View);
         } else if (strcmp(out,"HeadName") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->HeadName);
         } else if (strcmp(out,"OnDisk") == 0) {
            fprintf(SUMA_STDOUT, "%d", 
                        Test->OnDisk);
         } else if (strcmp(out,"Size") == 0) {
            fprintf(SUMA_STDOUT, "%ld", 
                        Test->Size);
         } else if (strcmp(out,"FNameNoAfniExt") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        without_afni_filename_extension(Test->FileName));
         } else if (strcmp(out,"FName") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->FileName);
         } else if (strcmp(out,"ExistsAs") == 0) {
            fprintf(SUMA_STDOUT, "%s", 
                        Test->ExistsAs?Test->ExistsAs:"");
         } else if (strncmp(out,"trim",4) == 0) {
            int mxlen=20;
            if (strlen(out) == 4) mxlen = 20;
            else mxlen = (int)strtod(out+4,NULL);
            fprintf(SUMA_STDOUT, "%s", 
                        TrimString(Test->HeadName,mxlen));
         } else {
            SUMA_S_Errv("Bad -out option of %s", out);
            exit(1);
         }
      }
      fprintf(SUMA_STDOUT, "\n");
      if (nisa) SUMA_free_NI_str_array(nisa); nisa = NULL;
   } else {
      SUMA_ShowParsedFname(Test, NULL);
   }
   
   if (Test) SUMA_Free_Parsed_Name (Test);
   
   exit (0);
}/* Main */
#endif

