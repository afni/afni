   
/* Header FILES */
#include "SUMA_suma.h"
   
#undef STAND_ALONE

#if defined SUMA_StripPath_STAND_ALONE
#define STAND_ALONE 
#elif defined SUMA_ParseName_STAND_ALONE
#define STAND_ALONE
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF; 
#endif
   
   
#ifdef SUMA_StripPath_STAND_ALONE
void usageSUMA_StripPath ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_StripPath <Name> \n");
          printf ("\t  \n\n");
          printf ("To Compile:\ngcc -DSUMA_StripPath_STAND_ALONE -Wall -o $1 $1.c -SUMA_lib.a -I/usr/X11R6/include -I./ \n\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tThu Jan 24 10:55:18 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
	SUMA_FileName Test;
   
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_StripPath-Main-");
   
   
   if (argc < 2)
       {
          usageSUMA_StripPath ();
          exit (1);
       }

	Test = SUMA_StripPath (argv[1]);
	if (Test.Path)
		{
			fprintf(stdout,"%s %s", Test.Path, Test.FileName);
			SUMA_free(Test.Path);
			if (Test.FileName) SUMA_free(Test.FileName);
		}
	fprintf (stdout,"\n");
	
	return (0);
}/* Main */
#endif

#ifdef SUMA_ParseName_STAND_ALONE
void usageParseName_Main ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_ParseName <Name> \n");
          printf ("\t  breaks a file name into path, filename and extension components.\n");
          printf ("\t  The program outputs: Path FileName FileName_NoExtension Extension\n");
          printf ("\t  Empty attributes are represented by (null).\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Thu Jan  9 15:48:09 EST 2003\n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_ParseName_Main"}; 
	SUMA_PARSED_NAME *Test;
   
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}

   
   if (argc < 2)
       {
          usageParseName_Main ();
          exit (1);
       }

	Test = SUMA_ParseFname (argv[1]);
	if (!Test) {
      fprintf (SUMA_STDERR,"(null) (null) (null) (null)\n");
   }else {
      if (Test->Path[0] != '\0') fprintf (SUMA_STDERR,"%s ", Test->Path); 
      else fprintf (SUMA_STDERR,"(null) ");
      if (Test->FileName[0] != '\0') fprintf (SUMA_STDERR,"%s ",  Test->FileName);
      else fprintf (SUMA_STDERR,"(null) ");
      if (Test->FileName_NoExt[0]  != '\0') fprintf (SUMA_STDERR,"%s ",  Test->FileName_NoExt);
      else fprintf (SUMA_STDERR,"(null) ");
      if (Test->Ext[0]  != '\0') fprintf (SUMA_STDERR,"%s ",  Test->Ext);
      else fprintf (SUMA_STDERR,"(null) ");
      fprintf (SUMA_STDERR,"\n");
   }
   
   if (Test) SUMA_Free_Parsed_Name (Test);
   
   exit (0);
}/* Main */
#endif

