   
/* Header FILES */
#include "SUMA_suma.h"
   
/* CODE */
   
   
   
/*!**
File : SUMA_StripPath.c
\author Ziad Saad
Date : Thu Jan 24 10:55:18 EST 2002
   
Purpose : 
   
   splits a path/filename into its path and filename components
   
Usage : 
		Ans = SUMA_StripPath (Name)
   
   
Input paramters : 
\param   Name (char *) something like /hello/something
   
Returns : 
\return   ans (SUMA_FileName) .Path (char *) and .FileName (char *)
   
Support : 
\sa  SUMA_define.h 
   
To Compile as stand alone:
gcc -DSUMA_StripPath_STAND_ALONE -Wall -o $1 $1.c -SUMA_lib.a -I/usr/X11R6/include -I./
***/
SUMA_FileName SUMA_StripPath (char *FileName)
{/*SUMA_StripPath*/
   char FuncName[100],  PathDelimiter[1]; 
   int i, j, NotFound=1, N_FileName;
	SUMA_FileName NewName;
	
   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_StripPath");
   sprintf (PathDelimiter,"/");
	
	N_FileName = strlen(FileName);
	if (N_FileName ){
		i = N_FileName -1;
		while (i > -1 && NotFound) {
			if (FileName[i] == PathDelimiter[0]) NotFound = 0;
			--i;
		}
		if (!NotFound && i > -1) {
			NewName.Path = (char *)malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_alloc_problem (FuncName);
			}
			for (j=0; j<=i+1; ++j) {
				NewName.Path[j] = FileName[j];
			}
			/*fprintf(stdout,"jbegin=%d/%d\n", i+2, N_FileName);*/
			for (j=i+2; j < N_FileName; ++j) NewName.FileName[j-i-2] = FileName[j];
			/*fprintf(stdout,"All Path (%d chars)/%d: %s\n", (i+2),  strlen(NewName.Path), NewName.Path);
			fprintf(stdout,"All FileName (%d chars)/%d: %s\n", (N_FileName-i-2), strlen(NewName.FileName), NewName.FileName);*/
		}
		else {
			NewName.Path = (char *)malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_alloc_problem (FuncName);
			}
			sprintf(NewName.Path,"./");		
			sprintf(NewName.FileName,"%s", FileName);
		}
	}
	else {
		NewName.Path = NULL;
		NewName.FileName = NULL;
	}
	return (NewName);
}/*SUMA_StripPath*/
   
#ifdef SUMA_StripPath_STAND_ALONE
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_StripPath <Name> \n");
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
          usage ();
          exit (1);
       }

	Test = SUMA_StripPath (argv[1]);
	if (Test.Path)
		{
			fprintf(stdout,"%s %s", Test.Path, Test.FileName);
			free (Test.Path);
			if (Test.FileName) free(Test.FileName);
		}
	fprintf (stdout,"\n");
	
	return (0);
}/* Main */
#endif
