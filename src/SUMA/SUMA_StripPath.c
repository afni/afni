   
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

NOTE: SUMA_ParseFname() is better than this function	
   
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
			NewName.Path = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_alloc_problem (FuncName);
			}
			for (j=0; j<=i+1; ++j) {
				NewName.Path[j] = FileName[j];
			}
         NewName.Path[j] = '\0';
         
			/*fprintf(stdout,"jbegin=%d/%d\n", i+2, N_FileName);*/
			for (j=i+2; j < N_FileName; ++j) NewName.FileName[j-i-2] = FileName[j];
         NewName.FileName[j-i-2] = '\0';
         
			/* fprintf(stdout,"All Path (%d chars)/%d: %s\n", (i+2),  strlen(NewName.Path), NewName.Path);
			fprintf(stdout,"All FileName (%d chars)/%d: %s\n", (N_FileName-i-2), strlen(NewName.FileName), NewName.FileName); */
		}
		else {
			NewName.Path = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
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

/*!
   \brief ans = SUMA_ParseFname (FileName);
   parses a file name into its elements
   \param FileName (char *) obvious ...
   \return ans (SUMA_PARSED_NAME *) pointer to structure with following fields:
      .FileName (char *) containing filename without path. 
                        if empty .FileName = '\0'
      .Path (char *) containing path including last slash.
                     If no path exists, Path is "./" 
      .Ext (char *) containing extension including the dot.
                    If no extension exists, Ext = '\0'
      .FileName_NoExt (char *) filename without extension.
      
      \sa SUMA_Free_Parsed_Name
*/
SUMA_PARSED_NAME * SUMA_ParseFname (char *FileName)
{/*SUMA_ParseFname*/
   static char FuncName[]={"SUMA_ParseFname"};
   char PathDelimiter='/'; 
   int i, j, iExt , iFile, iPath, N_FileName;
	SUMA_PARSED_NAME *NewName = NULL;
   SUMA_Boolean FoundPath = NOPE, FoundExt, FoundFile;
	
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


	N_FileName = strlen(FileName);
   iExt = N_FileName;
   iPath = -1;
   iFile = 0;
   FoundPath = NOPE;
   FoundExt = NOPE;
	if (N_FileName ){
		NewName = (SUMA_PARSED_NAME *) SUMA_malloc(sizeof(SUMA_PARSED_NAME));
      
      i = N_FileName -1;
		while (i > -1 && !FoundPath) {
			if (FileName[i] == '.' && !FoundExt) {
            iExt = i;
            FoundExt = YUP;
         } else if (FileName[i] == PathDelimiter) {
            FoundPath = YUP;
            iPath = i;
            iFile = i+1;
         }
			--i;
		}
      
      if (iFile == iExt) {
         /* .file, not an extension */
         FoundExt = NOPE;
      }
      
      if (iFile ==  N_FileName) FoundFile = NOPE;
      else FoundFile = YUP;
      
      if (FoundPath) {
         NewName->Path = (char *)SUMA_malloc(sizeof(char)*(iPath+2));
         for (i=0; i<= iPath; ++i) NewName->Path[i] = FileName[i];
         NewName->Path[i] = '\0';
      }else {
         NewName->Path = (char *)SUMA_malloc(sizeof(char)*(3));
         sprintf(NewName->Path, "./");
      }
      
      if (FoundFile) {
         NewName->FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iFile + 2));
         for (i=iFile; i< N_FileName; ++i) NewName->FileName[i-iFile] = FileName[i];
         NewName->FileName[i-iFile] = '\0';
      }else {
         NewName->FileName = (char *)SUMA_malloc(sizeof(char));
         NewName->FileName[0] = '\0';
      }      
		
      if (FoundExt) {
		   NewName->FileName_NoExt = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iFile +2));
         NewName->Ext = (char *)SUMA_malloc(sizeof(char)*(N_FileName - iExt+2));
         for (i=iFile; i< iExt; ++i) NewName->FileName_NoExt[i-iFile] = FileName[i];
         NewName->FileName_NoExt[i-iFile] = '\0';
         for (i=iExt; i < N_FileName; ++i) NewName->Ext[i-iExt] = FileName[i];
         NewName->Ext[i-iExt] = '\0';
      } else {
         NewName->FileName_NoExt = (char *)SUMA_malloc(sizeof(char));
         NewName->Ext = (char *)SUMA_malloc(sizeof(char));
         NewName->FileName_NoExt[0] = '\0';
         NewName->Ext[0] = '\0';
      }
      
	}
   
	SUMA_RETURN (NewName);
}/*SUMA_ParseFname*/

/*!
   \brief ans = SUMA_isExtension(filename, ext);
      YUP if filename has the extension ext
*/
SUMA_Boolean SUMA_isExtension(char *filename, char *ext)
{
   static char FuncName[]={"SUMA_isExtension"}; 
   int cnt, N_ext, N_filename;
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!filename) SUMA_RETURN(NOPE);
   if (!ext) SUMA_RETURN(NOPE);
   N_ext = strlen(ext);
   N_filename = strlen(filename);
   if (N_ext > N_filename) SUMA_RETURN(NOPE);

   cnt = 1;
   while (cnt <= N_ext) {
      if (filename[N_filename-cnt] != ext[N_ext-cnt]) SUMA_RETURN(NOPE);
      ++cnt; 
   } 
   
   SUMA_RETURN(YUP);
}

/*!
   \brief ans = SUMA_Extension(filename, ext, Remove);
      removes or enforces an arbitrary extension from/to a filename
   
   \param filename(char *) input filename
   \param ext (char *) extension
   \param Remove (SUMA_Boolean) YUP = Remove extension if found
                                      Do nothing if it is not there already 
                                NOPE = Add extension if not there
                                       Do nothing if it is there already    
   \returns ans (char*) containing modified filename 
  
   - You must free ans on your own
   Examples:
      {
      char *ans=NULL;
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roi", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roxi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roxi", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", "", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension(".roi", "Junk.niml.roi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("", "", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      exit(1);
    }

*/
char *SUMA_Extension(char *filename, char *ext, SUMA_Boolean Remove)
{
   static char FuncName[]={"SUMA_Extension"}; 
   char *ans = NULL;
   int i, next, nfilename, ifile;
   SUMA_Boolean NoMatch = NOPE, LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!filename) SUMA_RETURN(NULL);
   nfilename = strlen(filename);
   
   if (!ext) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   next = strlen(ext);
   
   #if 0
   if (nfilename < next || next < 1 || nfilename < 1) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   #endif
   
   ifile = nfilename - next;
   NoMatch = NOPE;
   i = 0;
   do {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Comparing %c %c\n", FuncName, filename[ifile+i], ext[i]);
      if (filename[ifile+i] != ext[i]) NoMatch = YUP;
      ++i;
   }  while (ifile < nfilename && i < next && NoMatch);

   if (NoMatch) {
      if (Remove) { /* nothing to do */
         SUMA_LH("NoMatch, nothing to do");
         ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
         ans = strcpy(ans,filename);
         SUMA_RETURN(ans);
      } else { /* add extension */
         SUMA_LH("NoMatch, adding extensio");
         ans = (char *)SUMA_malloc((nfilename+next+1)*sizeof(char));
         sprintf(ans,"%s%s", filename, ext);
         SUMA_RETURN(ans);
      }
   }else {
      if (Remove) { /* remove it */
         SUMA_LH("Match, removing extension");
         ans = (char *)SUMA_malloc((nfilename - next+2)*sizeof(char));
         for (i=0; i< nfilename - next; ++i)  ans[i] = filename[i];
         ans[nfilename - next] = '\0'; /* for good measure */
      } else { /* nothing to do */
         SUMA_LH("Match, nothing to do");
         ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
         ans = strcpy(ans,filename);
         SUMA_RETURN(ans);
      }
   }
   
   SUMA_RETURN (ans);

}   
void *SUMA_Free_Parsed_Name(SUMA_PARSED_NAME *Test) 
{
   static char FuncName[]={"SUMA_Free_Parsed_Name"}; 

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!Test) SUMA_RETURN (NULL);
   if (Test->Path) SUMA_free(Test->Path);
   if (Test->FileName) SUMA_free(Test->FileName);
   if (Test->Ext) SUMA_free(Test->Ext);
   if (Test->FileName_NoExt) SUMA_free(Test->FileName_NoExt);
   SUMA_free(Test);
   
   SUMA_RETURN (NULL);
}

#ifdef SUMA_StripPath_STAND_ALONE
void usageSUMA_StripPath ()
   
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
          printf ("\n\33[1mUsage: \33[0m SUMA_ParseName <Name> \n");
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

