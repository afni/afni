
#define DEBUG_3
#ifdef DEBUG_1
   #define DEBUG_2
   #define DEBUG_3
#endif
   
/* Header FILES */
   
#include "SUMA_suma.h"

#ifdef STAND_ALONE
   SUMA_CommonFields *SUMAg_CF;
#else
   extern SUMA_CommonFields *SUMAg_CF; 
#endif

/* CODE */


/*!
   \brief Returns the code for the next command (at the tail of the list).
   CommandCode =  SUMA_GetListNextCommand (list);
   \param list (DList *) pointer to doubly linked list
   \return CommandCode (SUMA_ENGINE_CODE) code for next command. The next command is at the head of the list.
    
   Note that the list is not modified.
   
   This function replaces the obsolete SUMA_GetNextCommand
*/
SUMA_ENGINE_CODE SUMA_GetListNextCommand (DList *list)
{
   static char FuncName[]={"SUMA_GetListNextCommand"};
   DListElmt *next;
   SUMA_EngineData *ED = NULL;
   
   SUMA_ENTRY;

   if (!dlist_size(list)) {
      SUMA_RETURN (SE_Empty);
   }
   
   next = (DListElmt *)dlist_head(list);
   ED = (SUMA_EngineData *)(next->data);
   SUMA_RETURN (ED->CommandCode);
   
} 

   
/*!**
File : SUMA_ParseCommands.c
\author Ziad Saad
Date : Tue Feb 5 10:39:02 EST 2002
   
Purpose : 
   obtain the next command Scom from the string of commands S
   
   
Usage : 
      Ret = SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
   
   
Input paramters : 
\param  S (char *) : String containing commands like "Initialize|ShowSurf|LightsON~" 
\param  d (char)  : character delimiting multiple commands ('|' in this example)
\param  term (char) : character terminating entire command ('~' in this example)
\param  Scom (char *): Null terminated string that will contain latest command (LightsON) in this example
                     S will be returned "Initialize|ShowSurf~"

Returns : 
\return   code of the command as defined for SUMA_ENGINE_CODE
   
NOTE:  OBSOLETE, use SUMA_GetListNextCommand   
      
   
***/
int SUMA_GetNextCommand (char *S, char d, char term, char *Scom)
{/*SUMA_GetNextCommand*/
   static char FuncName[]={"SUMA_GetNextCommand"}; 
   int i=0, iBegin, iStop;
   
   SUMA_ENTRY;

   fprintf (SUMA_STDERR, "Error %s: This function is now obsolete. Must use SUMA_GetListNextCommand instead.\n", FuncName);
   SUMA_RETURN (NOPE);

   iStop = strlen(S)-1;
   
   /*fprintf(stdout,"%s %c %c\n", S, S[iStop], term);  */
   if (S[iStop] != term) {
      fprintf (stderr, "%s Error: Command poorly terminated!\n\a", FuncName);
      SUMA_RETURN (0);
   }
   /* Make sure character just before term is not d */
   if (S[iStop-1] == d) {
      S[iStop] = '\0';
      iStop -= 1;
      S[iStop] = term;
   }
   
   /* search for command delimiter */
   iBegin = iStop -1;
   while (iBegin > -1 && S[iBegin]!= d) --iBegin;
   ++iBegin;
   
   /* copy command to Scom*/
   for (i=0; i< iStop - iBegin; ++i)  {
      /*fprintf(stdout,"%d %c\n", iBegin+i, S[iBegin+i]);*/
      Scom[i] = S[iBegin+i]; 
      S[iBegin+i] = '\0'; /*for esthetics */}
   
   /* seal strings with terminators */
   Scom[iStop-iBegin] = '\0';
   if (iBegin > 0) {
      S[iBegin-1] = term;
      iStop = iBegin-1;
   }
   else {
      S[iBegin] = term;
      iStop = iBegin;

   }
   
   /*get the code of the command*/
   SUMA_RETURN (SUMA_CommandCode(Scom));

}/*SUMA_GetNextCommand*/

int SUMA_CommandCode(char *Scom)
{   
   static char FuncName[]={"SUMA_CommandCode"};
   
   SUMA_ENTRY;

   if (!strlen(Scom)) SUMA_RETURN (SE_Empty);
   if (strcmp(Scom,"~") == 0) SUMA_RETURN (SE_Empty);
   
   /*fprintf(stdout,"Looking for %s\n", Scom);*/
   if (!strcmp(Scom,"SetLookAt")) SUMA_RETURN(SE_SetLookAt);   
   if (!strcmp(Scom,"SetLookFrom")) SUMA_RETURN(SE_SetLookFrom);
   if (!strcmp(Scom,"Redisplay")) SUMA_RETURN(SE_Redisplay);
   if (!strcmp(Scom,"RedisplayNow")) SUMA_RETURN(SE_RedisplayNow);
   if (!strcmp(Scom,"Redisplay_AllVisible")) SUMA_RETURN(SE_Redisplay_AllVisible);
   if (!strcmp(Scom,"SetNodeColor")) SUMA_RETURN (SE_SetNodeColor);
   if (!strcmp(Scom,"FlipLight0Pos"))   SUMA_RETURN(SE_FlipLight0Pos);
   if (!strcmp(Scom,"GetNearestNode"))   SUMA_RETURN (SE_GetNearestNode);
   if (!strcmp(Scom,"SetLookAtNode"))   SUMA_RETURN (SE_SetLookAtNode);
   if (!strcmp(Scom,"SetRotMatrix"))   SUMA_RETURN (SE_SetRotMatrix);
   if (!strcmp(Scom,"SetCrossHair"))   SUMA_RETURN (SE_SetCrossHair);
   if (!strcmp(Scom,"ToggleCrossHair"))   SUMA_RETURN (SE_ToggleCrossHair);
   if (!strcmp(Scom,"HighlightNodes"))   SUMA_RETURN (SE_HighlightNodes);
   if (!strcmp(Scom,"ToggleShowSelectedNode"))   SUMA_RETURN (SE_ToggleShowSelectedNode);
   if (!strcmp(Scom,"SetSelectedNode"))   SUMA_RETURN (SE_SetSelectedNode);
   if (!strcmp(Scom,"SetSelectedFaceSet"))   SUMA_RETURN (SE_SetSelectedFaceSet);
   if (!strcmp(Scom,"ToggleShowSelectedFaceSet"))   SUMA_RETURN (SE_ToggleShowSelectedFaceSet);
   if (!strcmp(Scom,"ToggleConnected")) SUMA_RETURN (SE_ToggleConnected);
   if (!strcmp(Scom,"StartListening")) SUMA_RETURN(SE_StartListening);
   if (!strcmp(Scom,"SetAfniCrossHair")) SUMA_RETURN (SE_SetAfniCrossHair);
   if (!strcmp(Scom,"SetForceAfniSurf")) SUMA_RETURN (SE_SetForceAfniSurf);
   if (!strcmp(Scom,"CloseStream4All")) SUMA_RETURN (SE_CloseStream4All);
   if (!strcmp(Scom,"SetAfniSurf")) SUMA_RETURN (SE_SetAfniSurf);
   if (!strcmp(Scom,"SetAfniThisSurf")) SUMA_RETURN (SE_SetAfniThisSurf);
   if (!strcmp(Scom,"SetAfniSurfList")) SUMA_RETURN (SE_SetAfniSurfList);
   if (!strcmp(Scom,"BindCrossHair")) SUMA_RETURN(SE_BindCrossHair);
   if (!strcmp(Scom,"ToggleForeground")) SUMA_RETURN (SE_ToggleForeground);
   if (!strcmp(Scom,"ToggleBackground")) SUMA_RETURN (SE_ToggleBackground);
   if (!strcmp(Scom,"FOVreset")) SUMA_RETURN (SE_FOVreset);
   if (!strcmp(Scom,"ResetOpenGLState")) SUMA_RETURN (SE_ResetOpenGLState);
   if (!strcmp(Scom,"LockCrossHair")) SUMA_RETURN(SE_LockCrossHair);
   if (!strcmp(Scom,"Home")) SUMA_RETURN (SE_Home);
   if (!strcmp(Scom,"Home_AllVisible")) SUMA_RETURN (SE_Home_AllVisible);
   if (!strcmp(Scom,"ToggleLockAllCrossHair")) SUMA_RETURN(SE_ToggleLockAllCrossHair);
   if (!strcmp(Scom,"SetLockAllCrossHair")) SUMA_RETURN(SE_SetLockAllCrossHair);
   if (!strcmp(Scom,"ToggleLockView")) SUMA_RETURN(SE_ToggleLockView);
   if (!strcmp(Scom,"ToggleLockAllViews")) SUMA_RETURN(SE_ToggleLockAllViews);
   if (!strcmp(Scom,"Load_Group")) SUMA_RETURN(SE_Load_Group);
   if (!strcmp(Scom,"Help")) SUMA_RETURN(SE_Help);
   if (!strcmp(Scom,"Help_Cmap")) SUMA_RETURN(SE_Help_Cmap);
   if (!strcmp(Scom,"UpdateLog")) SUMA_RETURN(SE_UpdateLog);
   if (!strcmp(Scom,"Log")) SUMA_RETURN(SE_Log);
   if (!strcmp(Scom,"SetRenderMode")) SUMA_RETURN(SE_SetRenderMode);
   if (!strcmp(Scom,"OpenDrawROI")) SUMA_RETURN(SE_OpenDrawROI);
   if (!strcmp(Scom,"RedisplayNow_AllVisible")) SUMA_RETURN(SE_RedisplayNow_AllVisible);
   if (!strcmp(Scom,"RedisplayNow_AllOtherVisible")) SUMA_RETURN(SE_RedisplayNow_AllOtherVisible);
   if (!strcmp(Scom,"SetLight0Pos")) SUMA_RETURN(SE_SetLight0Pos);
   if (!strcmp(Scom,"OpenColFileSelection")) SUMA_RETURN(SE_OpenColFileSelection);
   if (!strcmp(Scom,"OpenDsetFileSelection")) SUMA_RETURN(SE_OpenDsetFileSelection);
   if (!strcmp(Scom,"OpenCmapFileSelection")) SUMA_RETURN(SE_OpenCmapFileSelection);
   if (!strcmp(Scom,"SaveDrawnROIFileSelection")) SUMA_RETURN(SE_SaveDrawnROIFileSelection);
   if (!strcmp(Scom,"OpenDrawnROIFileSelection")) SUMA_RETURN(SE_OpenDrawnROIFileSelection);
   if (!strcmp(Scom,"SendColorMapToAfni")) SUMA_RETURN(SE_SendColorMapToAfni);
   if (!strcmp(Scom,"SaveSOFileSelection")) SUMA_RETURN(SE_SaveSOFileSelection);
   if (!strcmp(Scom,"SetSOinFocus")) SUMA_RETURN(SE_SetSOinFocus);
   if (!strcmp(Scom,"LoadViewFileSelection")) SUMA_RETURN(SE_LoadViewFileSelection);
   if (!strcmp(Scom,"SaveViewFileSelection")) SUMA_RETURN(SE_SaveViewFileSelection);
   if (!strcmp(Scom,"LoadSegDO")) SUMA_RETURN(SE_LoadSegDO);
   /*if (!strcmp(Scom,"")) SUMA_RETURN(SE_);*/
   
   /* Last one is Bad Code */
   SUMA_RETURN (SE_BadCode);
     
}

const char *SUMA_ColMixModeString (SUMA_COL_MIX_MODE mode)
{
   static char FuncName[]={"SUMA_ColMixModeString"};
   
   SUMA_ENTRY;
   
   switch (mode) {
      case SUMA_BAD_MODE:
         SUMA_RETURN("BadMode");
      case SUMA_ORIG_MIX_MODE:
         SUMA_RETURN("ORIG");
      case SUMA_4AML:
         SUMA_RETURN("MOD1");
      default:
         SUMA_RETURN("VeryBadMode");
   }
   
}

const char *SUMA_DomainKinships_String (SUMA_DOMAIN_KINSHIPS code)
{
   static char FuncName[]={"SUMA_DomainKinships_String"};
   
   SUMA_ENTRY;
   
   switch (code) {
      case SUMA_DOMAINS_ERROR:
         SUMA_RETURN("Code Error");
      case SUMA_DOMAINS_NOT_RELATED:
         SUMA_RETURN("Surfaces domains not related");
      case SUMA_SO1_is_SO2:
         SUMA_RETURN("Surfaces are the same (identical idcodes)");
      case SUMA_SO1_is_LDPSO2:
         SUMA_RETURN("Surface 1 is the local domain parent of Surface 2");
      case SUMA_SO2_is_LDPSO1:
         SUMA_RETURN("Surface 2 is the local domain parent of Surface 1");
      case SUMA_NUCELAR_FAMILY:
         SUMA_RETURN("Flag for nuclear family flag limit");
      case SUMA_LDPSO1_is_LDPSO2:
         SUMA_RETURN("Surfaces have same domain grandparent");
      case SUMA_SO1_is_GPSO2:
         SUMA_RETURN("Surface 1 is the domain grandparent of Surface 2");
      case SUMA_SO2_is_GPSO1:
         SUMA_RETURN("Surface 2 is the domain grandparent of Surface 1");
      default:
         SUMA_RETURN("Should not see this"); 
   }
     
   SUMA_RETURN("Should not see this either");
}
/*!
   \brief Transforms a command code into a string for human consumption
   const char *SUMA_CommandString (SUMA_ENGINE_CODE code);

*/ 
const char *SUMA_CommandString (SUMA_ENGINE_CODE code)
{
   static char FuncName[]={"SUMA_CommandString"};
   
   SUMA_ENTRY;
   
   switch (code) {
      case SE_SetLookAt:
         SUMA_RETURN("SetLookAt");
      case SE_SetLookFrom:
         SUMA_RETURN("SetLookFrom");
      case SE_Redisplay:
         SUMA_RETURN("Redisplay");
      case SE_RedisplayNow:
         SUMA_RETURN("RedisplayNow");      
      case SE_Redisplay_AllVisible:
         SUMA_RETURN("Redisplay_AllVisible");      
      case SE_SetNodeColor:
         SUMA_RETURN("SetNodeColor");      
      case SE_FlipLight0Pos:
         SUMA_RETURN("FlipLight0Pos");      
      case SE_GetNearestNode: 
         SUMA_RETURN("GetNearestNode");      
      case SE_SetLookAtNode:
         SUMA_RETURN("SetLookAtNode");      
      case SE_SetRotMatrix:
         SUMA_RETURN("SetRotMatrix");      
      case SE_SetCrossHair:
         SUMA_RETURN("SetCrossHair");      
      case SE_ToggleCrossHair:
         SUMA_RETURN("ToggleCrossHair");      
      case SE_HighlightNodes:
         SUMA_RETURN("HighlightNodes");      
      case SE_ToggleShowSelectedNode:
         SUMA_RETURN("ToggleShowSelectedNode");      
      case SE_SetSelectedNode:
         SUMA_RETURN("SetSelectedNode");      
      case SE_SetSelectedFaceSet:
         SUMA_RETURN("SetSelectedFaceSet");      
      case SE_ToggleShowSelectedFaceSet:
         SUMA_RETURN("ToggleShowSelectedFaceSet");      
      case SE_ToggleConnected:
         SUMA_RETURN("ToggleConnected");      
      case SE_StartListening:
         SUMA_RETURN("StartListening");
      case SE_SetAfniCrossHair:
         SUMA_RETURN("SetAfniCrossHair");      
      case SE_SetForceAfniSurf:
         SUMA_RETURN("SetForceAfniSurf");      
      case SE_CloseStream4All:
         SUMA_RETURN("CloseStream4All");      
      case SE_SetAfniThisSurf: 
         SUMA_RETURN("SetAfniThisSurf");      
      case SE_SetAfniSurf: 
         SUMA_RETURN("SetAfniSurf");      
      case SE_SetAfniSurfList: 
         SUMA_RETURN("SetAfniSurfList");      
      case SE_BindCrossHair:
         SUMA_RETURN("BindCrossHair");      
      case SE_ToggleForeground:
         SUMA_RETURN("ToggleForeground");      
      case SE_ToggleBackground:
         SUMA_RETURN("ToggleBackground");      
      case SE_FOVreset:
         SUMA_RETURN("FOVreset");      
      case SE_ResetOpenGLState: 
         SUMA_RETURN("ResetOpenGLState");      
      case SE_LockCrossHair:
         SUMA_RETURN("LockCrossHair");      
      case SE_Home:
         SUMA_RETURN("Home"); 
      case SE_Home_AllVisible:
         SUMA_RETURN("Home_AllVisible");     
      case SE_Empty:
         SUMA_RETURN("Empty");
      case SE_ToggleLockAllCrossHair:
         SUMA_RETURN("ToggleLockAllCrossHair");      
      case SE_SetLockAllCrossHair:
         SUMA_RETURN("SetLockAllCrossHair"); 
      case SE_ToggleLockView:
         SUMA_RETURN("ToggleLockView");
      case SE_ToggleLockAllViews:
         SUMA_RETURN("ToggleLockAllViews");   
      case SE_Load_Group:
         SUMA_RETURN("Load_Group"); 
      case SE_Help:
         SUMA_RETURN("Help");
      case SE_Help_Cmap:
         SUMA_RETURN("Help_Cmap");
      case SE_UpdateLog:
         SUMA_RETURN("UpdateLog"); 
      case SE_Log:
         SUMA_RETURN("Log");
      case SE_SetRenderMode:
         SUMA_RETURN("SetRenderMode");
      case SE_OpenDrawROI:
         SUMA_RETURN("OpenDrawROI"); 
      case SE_RedisplayNow_AllVisible:
         SUMA_RETURN("RedisplayNow_AllVisible");
      case SE_RedisplayNow_AllOtherVisible:
         SUMA_RETURN("RedisplayNow_AllOtherVisible");
      case SE_SetLight0Pos:
         SUMA_RETURN("SetLight0Pos");      
      case SE_OpenColFileSelection:
         SUMA_RETURN("OpenColFileSelection");      
      case SE_OpenDsetFileSelection:
         SUMA_RETURN("OpenDsetFileSelection");      
      case SE_OpenCmapFileSelection:
         SUMA_RETURN("OpenCmapFileSelection");      
      case SE_SaveDrawnROIFileSelection:
         SUMA_RETURN("SaveDrawnROIFileSelection");      
      case SE_OpenDrawnROIFileSelection:
         SUMA_RETURN("OpenDrawnROIFileSelection");      
      case SE_SendColorMapToAfni:
         SUMA_RETURN("SendColorMapToAfni");      
      case SE_SaveSOFileSelection:
         SUMA_RETURN("SaveSOFileSelection");      
      case SE_SetSOinFocus:
         SUMA_RETURN("SetSOinFocus");
      case SE_LoadViewFileSelection:
         SUMA_RETURN("LoadViewFileSelection"); 
      case SE_SaveViewFileSelection:
         SUMA_RETURN("SaveViewFileSelection"); 
      case SE_LoadSegDO:
         SUMA_RETURN("LoadSegDO");    
      /*case SE_:
         SUMA_RETURN("");      */
      default:        
         SUMA_RETURN ("BadCode");
   }
}

/*!
   \brief Returns the name (string) of a surface type
*/
const char * SUMA_SurfaceTypeString (SUMA_SO_File_Type tp)
{
   static char FuncName[]={"SUMA_SurfaceTypeString"};
   
   SUMA_ENTRY; 
   switch (tp) {
      case SUMA_FT_NOT_SPECIFIED:
         SUMA_RETURN("NotSpecified");
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         SUMA_RETURN("FreeSurfer");
         break;
      case SUMA_SUREFIT:
         SUMA_RETURN("SureFit");
         break;
      case SUMA_INVENTOR_GENERIC:
         SUMA_RETURN("GenericInventor"); 
         break;
      case SUMA_PLY:
         SUMA_RETURN("Ply");
         break;
      case SUMA_BRAIN_VOYAGER:
         SUMA_RETURN("BrainVoyager");
         break;
      case SUMA_VEC:
         SUMA_RETURN("1D");
         break;
      case SUMA_OPENDX_MESH:
         SUMA_RETURN("OpenDX");
         break;
      case SUMA_FT_ERROR:
         SUMA_RETURN("Error");     
      default:        
         SUMA_RETURN ("Error");
   }
}

/*!
   \brief Returns the code for a surface's file type
*/
SUMA_SO_File_Type SUMA_SurfaceTypeCode (char *cd)
{
   static char FuncName[]={"SUMA_SurfaceTypeCode"};
   
   SUMA_ENTRY;
   
   if (!cd) { SUMA_RETURN(SUMA_FT_ERROR); }
   
   if (!strcmp(cd, "NotSpecified")) { SUMA_RETURN(SUMA_FT_NOT_SPECIFIED ); }
   if (!strcmp(cd, "FreeSurfer") || !strcmp(cd, "FS")) { SUMA_RETURN( SUMA_FREE_SURFER); }
   if (!strcmp(cd, "SureFit") || !strcmp(cd, "SF")) { SUMA_RETURN( SUMA_SUREFIT); }
   if (!strcmp(cd, "GenericInventor") || !strcmp(cd, "INV")) { SUMA_RETURN(SUMA_INVENTOR_GENERIC ); }
   if (!strcmp(cd, "Ply") || !strcmp(cd, "PLY")) { SUMA_RETURN( SUMA_PLY); }
   if (!strcmp(cd, "DX") || !strcmp(cd, "dx") || !strcmp(cd, "OpenDX") || !strcmp(cd, "opendx")) { SUMA_RETURN( SUMA_OPENDX_MESH); }
   if (!strcmp(cd, "BrainVoyager") || !strcmp(cd, "BV") || !strcmp(cd, "bv")) { SUMA_RETURN( SUMA_BRAIN_VOYAGER); }
   if (!strcmp(cd, "1D") || !strcmp(cd, "VEC") || !strcmp(cd, "1d")) { SUMA_RETURN(SUMA_VEC ); }
   if (!strcmp(cd, "Error")) { SUMA_RETURN(SUMA_FT_ERROR ); }
   /* if (!strcmp(cd, "")) { SUMA_RETURN( ); } */
   SUMA_RETURN(SUMA_FT_ERROR); 
   
}
/*!**
   
Purpose : 
   Append or prepend command Scom to S
   
   
Usage : 
      Ret =  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
   
   
Input paramters : 
\param  S (char *) : String containing commands like "Initialize|ShowSurf~" MUST BE NULL TERMINATED
\param  d (char)  : character delimiting multiple commands ('|' in this example)
\param  term (char) : character terminating entire command ('~' in this example)
\param  Scom (char *): Null terminated string that will contain latest command (LightsON) in this example
                     S will be returned "Initialize|ShowSurf/LightsON~" if Prepend is YUP
                     S will be returned "LightsON|Initialize|ShowSurf" is Prepend if NOPE
\param Prepend (SUMA_Boolean): append or prepend command

Returns : 
\return   NOPE for failure, YUP success
   
NOTE: OBSOLETE, use  SUMA_RegisterEngineListCommand  
      
   
***/
SUMA_Boolean SUMA_RegisterCommand (char *S, char d, char term, char *Scom, SUMA_Boolean Prepend)
{   int i, iStop, iorig, iStopNew, nCom;
   static char FuncName[]={"SUMA_RegisterCommand"};
   
   SUMA_ENTRY;
   
   fprintf (SUMA_STDERR, "Error %s: This function is now obsolete. Must use SUMA_RegisterCommand instead.\n", FuncName);
   SUMA_RETURN (NOPE);

   iStop = strlen(S)-1;
      
   nCom = strlen(Scom);
   /*fprintf (stdout,"Scom->%s<-, length %d\n", Scom, nCom);*/
   if (strlen(Scom) + iStop + 2 > SUMA_MAX_COMMAND_LENGTH ) {
      fprintf (stderr, "%s Error: Resultant command longer than SUMA_MAX_COMMAND_LENGTH!\n\a", FuncName);
      SUMA_RETURN (NOPE);
   }
   if (S[iStop] != term) {
      fprintf (stderr, "%s Error: S improperly terminated!\n\a", FuncName);
      SUMA_RETURN (NOPE);
   }
   if (!Prepend) {
      /* add a delimiter */
      if (S[iStop-1] != d) { 
         S[iStop] = d;
         iStop += 1;
      }
      /*append the command */
      for (i=0; i <nCom; ++i) {
         S[iStop+i] = Scom[i];
      } 
      iStop += nCom;
      S[iStop] = term;
      S[iStop+1] = '\0';
      SUMA_RETURN (YUP);
   } else {
      /* move old string forward*/
      iStopNew = iStop+nCom+1;
      S[iStopNew+1] = '\0';
      iorig = 0;
      while (iorig <= iStop) {
         S[iStopNew-iorig] = S[iStop-iorig];
         ++iorig;
      }
      S[iStopNew-iorig] = d;
      
      /*add new one */
      for (i=0; i < nCom; ++i) {
         S[i] = Scom[i];
      }
      iStop = iStopNew;
      SUMA_RETURN (YUP);
   }
}

/*!
   \brief translates SUMA_ENGINE_FIELD_CODE to string
*/
const char* SUMA_EngineFieldString (SUMA_ENGINE_FIELD_CODE i)
{
   static char FuncName[]={"SUMA_EngineFieldString"};
   
   SUMA_ENTRY;

   switch (i) {
      case (SEF_fm):
         SUMA_RETURN ("fm");
         break;
      case (SEF_im):
         SUMA_RETURN ("im");
         break;
      case (SEF_ivec):
         SUMA_RETURN ("ivec");
         break;
      case (SEF_fvec):
         SUMA_RETURN ("fvec");
         break;
      case (SEF_fv3):
         SUMA_RETURN ("fv3");
         break;
      case (SEF_iv3):
         SUMA_RETURN ("iv3");
         break;
      case (SEF_fv15):
         SUMA_RETURN ("fv15");
         break;
      case (SEF_iv15):
         SUMA_RETURN ("iv15");
         break;
      case (SEF_iv200):
         SUMA_RETURN ("iv200");
         break;
      case (SEF_fv200):
         SUMA_RETURN ("fv200");
         break;
      case (SEF_i):
         SUMA_RETURN ("i");
         break;
      case (SEF_f):
         SUMA_RETURN ("f");
         break;
      case (SEF_s):
         SUMA_RETURN ("s");
         break;
      case (SEF_vp):
         SUMA_RETURN ("vp");
         break;
      case (SEF_cp):
         SUMA_RETURN ("cp");
         break;
      case (SEF_fp):
         SUMA_RETURN ("fp");
         break;
      case (SEF_ip):
         SUMA_RETURN ("ip");
         break;
      default:
         SUMA_RETURN ("Unknown");
         break;
      
   }
   
}

SUMA_ENGINE_FIELD_CODE SUMA_EngineFieldCode(char *Scom)
{   
   static char FuncName[]={"SUMA_EngineFieldCode"};
   
   SUMA_ENTRY;

   if (!strlen(Scom)) SUMA_RETURN (SEF_Empty);
   
   /*fprintf(stdout,"Looking for %s\n", Scom);*/
   if (!strcmp(Scom,"fm")) SUMA_RETURN(SEF_fm);   
   if (!strcmp(Scom,"im")) SUMA_RETURN(SEF_im);   
   if (!strcmp(Scom,"fvec")) SUMA_RETURN(SEF_fvec);   
   if (!strcmp(Scom,"ivec")) SUMA_RETURN(SEF_ivec);   
   if (!strcmp(Scom,"fv3")) SUMA_RETURN(SEF_fv3);
   if (!strcmp(Scom,"iv3")) SUMA_RETURN(SEF_iv3);
   if (!strcmp(Scom,"fv15")) SUMA_RETURN(SEF_fv15);
   if (!strcmp(Scom,"iv15")) SUMA_RETURN(SEF_iv15);
   if (!strcmp(Scom,"fv200")) SUMA_RETURN(SEF_fv200);
   if (!strcmp(Scom,"iv200")) SUMA_RETURN(SEF_iv200);
   if (!strcmp(Scom,"i")) SUMA_RETURN(SEF_i);
   if (!strcmp(Scom,"f")) SUMA_RETURN (SEF_f);
   if (!strcmp(Scom,"s")) SUMA_RETURN (SEF_s);
   if (!strcmp(Scom,"vp")) SUMA_RETURN (SEF_vp); /* void pointer */
   if (!strcmp(Scom,"fp")) SUMA_RETURN(SEF_fp);
   if (!strcmp(Scom,"cp")) SUMA_RETURN(SEF_cp);
   if (!strcmp(Scom,"ip")) SUMA_RETURN(SEF_ip);
   /*if (!strcmp(Scom,"")) SUMA_RETURN(SEF_);*/
   
   /* Last one is Bad Code */
   SUMA_RETURN (SEF_BadCode);
     
}

int SUMA_EngineSourceCode (char *Scom)
{
   static char FuncName[]={"SUMA_EngineSourceCode"};
   
   SUMA_ENTRY;

   if (!strlen(Scom)) SUMA_RETURN (SES_Empty);
   if (!strcmp(Scom,"suma")) SUMA_RETURN(SES_Suma);
   if (!strcmp(Scom,"afni")) SUMA_RETURN(SES_Afni);
   if (!strcmp(Scom,"suma_widget")) SUMA_RETURN(SES_SumaWidget);
   if (!strcmp(Scom,"suma_from_afni")) SUMA_RETURN(SES_SumaFromAfni);
   if (!strcmp(Scom,"suma_from_any")) SUMA_RETURN(SES_SumaFromAny);
   if (!strcmp(Scom,"unknown")) SUMA_RETURN(SES_Unknown);
   
   /* got here? Unknown */
   SUMA_RETURN (SES_Unknown);
}  

void SUMA_EngineSourceString (char *Scom, int i)
{
   static char FuncName[]={"SUMA_EngineSourceString"};

   SUMA_ENTRY;

   switch (i) {
      case SES_Empty:
         Scom[0]='\0';
         break;
      case SES_Suma:
         sprintf(Scom,"suma");
         break;
      case SES_Afni:
         sprintf(Scom, "afni");
         break;
      case SES_SumaWidget:
         sprintf(Scom, "suma_widget");
         break;
      case SES_SumaFromAfni:
         sprintf(Scom, "suma_from_afni");
         break;
      case SES_SumaFromAny:
         sprintf(Scom, "suma_from_any");
         break;
      case SES_Unknown:
         sprintf(Scom, "unknown");
         break;
      default:
         sprintf(Scom, "Undetermined flag");
         break;
   }
   SUMA_RETURNe;
}

/*! 
\brief Appends a new message to the list of SUMA messages, making sure
the number of messages does not exceed SUMA_MAX_MESSAGES
Ans = SUMA_RegisterMessage (  list, Message, Source, Type, Action );

\param list (DList *) pointer to doubly linked list of messages
\param Message (char *) null terminated message
\param Source (char *) null terminated source of message
\param Type (SUMA_MESSAGE_TYPES) Type of message to spit out
\param Action (SUMA_MESSAGE_ACTION) Action to perform with message
\return  YUP/NOPE, success/failure

*/
SUMA_Boolean SUMA_RegisterMessage ( DList *list, char *Message, 
                                    char *Source, SUMA_MESSAGE_TYPES Type, 
                                    SUMA_MESSAGE_ACTION Action)
{
   static char FuncName[]={"SUMA_RegisterMessage"};
   SUMA_MessageData *MD = NULL;
   SUMA_Boolean TryLogWindow = NOPE;
   int i=0, TrimTheFat=0;
   
   SUMA_ENTRY;
   
   if (!list) {
      fprintf (SUMA_STDERR, "Warning %s: list has not been initialized.\n"
                            "Nothing done.\n", FuncName);
      SUMA_RETURN (YUP);
   }
   
   /* allocate and initialize element */
   MD = (SUMA_MessageData *) SUMA_malloc(sizeof(SUMA_MessageData));
   if (!MD) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   MD->Message = SUMA_copy_string(Message);
   MD->Source = SUMA_copy_string(Source);
   MD->Type = Type;
   MD->Action = Action;
   
   /* add element at end */
   if (dlist_ins_next (list, dlist_tail(list), (void *)MD) < 0) {
       fprintf (SUMA_STDERR, "Error %s: Failed to insert message:\n%s from %s in list.\n", 
         FuncName, MD->Message, MD->Source);
       SUMA_RETURN(NOPE);
   }
   
   /* make sure size of list is < SUMA_MAX_MESSAGES */
   TrimTheFat = list->size - SUMA_MAX_MESSAGES;
   if ( TrimTheFat > 0) {
      for (i=0; i < TrimTheFat; ++i) {
         /* remove the head */
         if (!SUMA_ReleaseMessageListElement (list, dlist_head(list))) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_ReleaseMessageListElement.\n", FuncName);
            SUMA_RETURN (NOPE);
         } 
      }
   }
   
   /* Decide on what to do with new element */
   switch (MD->Action) {
      case SMA_Nothing:
         break;
      case SMA_Log:
         TryLogWindow = YUP;
         break;
      case SMA_LogAndPopup:
         TryLogWindow = YUP;
         SUMA_PopUpMessage (MD);
         break;
      default:
         break;
   
   }
   
   if (TryLogWindow) {
      DList *Elist=NULL;
      SUMA_EngineData *ED=NULL;

      Elist = SUMA_CreateList();
      SUMA_REGISTER_HEAD_COMMAND_NO_DATA(Elist, SE_UpdateLog, SES_Suma, NULL);

      if (!SUMA_Engine (&Elist)) {
         fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }

   
   SUMA_RETURN (YUP);
}

/*!
   \brief forms a string out of all the messages in the Message list
   
*/
char *SUMA_BuildMessageLog (DList *ML)
{
   static char FuncName[]={"SUMA_BuildMessageLog"};
   char *s=NULL;
   SUMA_STRING *SS = NULL;
   SUMA_MessageData *MD=NULL;
   DListElmt *CurElmt=NULL;
   
   SUMA_ENTRY;
   
  
   if (!ML->size) { /* Nothing */
      SUMA_RETURN (NULL);
   }
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!(CurElmt = dlist_head(ML))) {
      SUMA_RETURN (NULL);
   }
   do {
      MD = (SUMA_MessageData *)CurElmt->data;
      SS = SUMA_StringAppend (SS, SUMA_FormatMessage (MD)); 
      if (MD->Type != SMT_Text)SS = SUMA_StringAppend (SS, "---------------------\n");
   } while ((CurElmt = dlist_next(CurElmt)));
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);
   
}
/*!
\brief Adds a new element to the list of commands for SUMA_Engine.
NewElement = SUMA_RegisterEngineListCommand (   list,  EngineData,  
                                                FldCode, FldValp, 
                                                SourceCode, SourcePointer, PassByPointer, 
                                                InsertAt, Element);
                                                
\param list (DList *) pointer to doubly linked list of engine commands.
\param EngineData (SUMA_EngineData *) a properly initialized pointer to EngineData structure.
\param FldCode (SUMA_ENGINE_FIELD_CODE) code of field in EngineData structure to be filled.
\param FldValp (void *) pointer to value that is to be placed in FldCode field of EngineData.
\param SourceCode (SUMA_ENGINE_SOURCE) code of source issuing command.
\param SourcePointer (void *) pointer to data structure of source issuing command. 
   I use this as a pointer to the calling surface viewer structure, but you can use it for anyting you please as long
   as SUMA_Engine knows what to do with it. Send NULL for none.
\param PassByPointer (SUMA_Boolean) flag (YUP/NOPE), if YUP then assignment is done at the pointer level (EngineData->Fld = FldValp)
       if NOPE then space is allocated for Fld and values are copied from FldValp[i][j] to EngineData->Fld[i][j]
\param InsertAt (SUMA_ENGINE_INSERT_LOCATION) Determines where to insert the next element in the list.
       SEI_Head : Insert at head of list (prepend)
       SEI_Tail : Insert at tail of list (append)
       SEI_Before : Insert before Element
       SEI_After : Insert after Element
       SEI_In : Inset in Element
\param Element (DListElmt *) Element relative to which the insertion is made. NULL should be used with SEI_Head and SEI_Tail

\return NewElement (DListElmt *) The new element inserted into the list. 
                                    NewElement = Element if SEI_In is used for InsertAt.
                                    NULL is returned if the function fails.


\sa SUMA_InitializeEngineListData

-PassByPointer option is only useful when dealing with fields that are/can be dynamically allocated like fm and fi. 
For fields like fv3 or iv15 then assignments are done by value and not pointers.
-You cannot set the value of a field unless the destination for the pre-existing data in that field has been reached                        
-When passing by value for fields requiring allocation, like fm or fi, you must be sure that EngineData->N_cols and 
EngineData->N_rows are set correctly before you call the function.

-NOTE: If a Command requires that mutliple fields be filled, you can call this function repeatedly with the same 
fields except FldCode, FldValp, SourcePointer and InsertAt should be SEI_In and Element should be the pointer 
returned in the previous call for  SUMA_RegisterEngineListCommand.

*/
DListElmt * SUMA_RegisterEngineListCommand (DList *list, SUMA_EngineData * EngineData,  
                                             SUMA_ENGINE_FIELD_CODE Fld, void *FldValp, 
                                             SUMA_ENGINE_SOURCE Src, void *Srcp, SUMA_Boolean PassByPointer, 
                                             SUMA_ENGINE_INSERT_LOCATION InsertAt, DListElmt *Element)
{ 
   SUMA_ENGINE_CODE Dest=SES_Empty;
   static char FuncName[]={"SUMA_RegisterEngineListCommand"};
   SUMA_Boolean  Refill = NOPE;
   DListElmt *tail=NULL, *head=NULL, *NewElement=NULL;
   SUMA_EngineData * Old_ED=NULL;
   SUMA_IVEC *ivec=NULL;
   SUMA_FVEC *fvec=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (PassByPointer) {
      if (Fld != SEF_im && Fld != SEF_fm && Fld != SEF_ivec && Fld != SEF_fvec) {
         SUMA_SL_Err("Cannot use PassByPointer except with SEF_im || SEF_fm || SEF_ivec || SEF_fvec");
         SUMA_RETURN (NULL);
      }
   } 
   
   if (!list) {
      fprintf (SUMA_STDERR, "Error %s: list has not been initialized.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   if (InsertAt == SEI_In) {
      /* adding fields to EngineData, check for errors */
      Refill = YUP;
      /* Src and Srcp should be the same as before */
      if (!Element) {
         SUMA_SL_Err("NULL element with SEI_In");
         SUMA_RETURN(NULL);
      }
      Old_ED = (SUMA_EngineData *)Element->data;
      if (Old_ED != EngineData) {
         fprintf (SUMA_STDERR, "Error %s: EngineData is different from initializing call for Element.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      if (Old_ED->Src != Src) {
         fprintf (SUMA_STDERR, "Error %s: Src is different from initializing call for Element.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      if (Old_ED->Srcp != Srcp) {
         fprintf (SUMA_STDERR, "Error %s: Srcp is different from initializing call for Element.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      if (Old_ED->CommandCode != EngineData->CommandCode) {
         fprintf (SUMA_STDERR, "Error %s: CommandCode is different in EngineData from the one initializing call for Element.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      
   } else Refill = NOPE;
   
   Dest = EngineData->CommandCode;
   
   if (!Refill) {
      /* make sure Destination is good and wholesome*/
      switch (Dest) {
         case SE_BadCode:
            fprintf (SUMA_STDERR, "Error in %s: Bad code string.\n", FuncName);
            SUMA_RETURN (NULL);
            break;
         case SE_Empty:
            fprintf (SUMA_STDERR, "Error in %s: Empty code string.\n", FuncName);
            SUMA_RETURN (NULL);
            break;
         default:
            break;
      }

      /* make sure that Srcp is empty or the same as in EngineData */
      if (EngineData->Srcp != NULL) {
         if (EngineData->Srcp != Srcp) {
            fprintf (SUMA_STDERR, "Error %s: Attempting to assign a Srcp to a structure that has a different Srcp.\n", FuncName);
            SUMA_RETURN (NULL); 
         }
      }

      EngineData->Srcp = Srcp;
      EngineData->Src = Src;
   }
   
   if (LocalHead) fprintf(SUMA_STDOUT, "%s: Registering %s for %s\n", FuncName, SUMA_EngineFieldString(Fld), SUMA_CommandString (Dest));
   
   switch (Fld) { /* switch Fld */
      case SEF_Empty:
         break;
      case SEF_fm:
         if (EngineData->fm_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->fm_Dest);
            SUMA_RETURN (NULL);
         }
         /* space available*/
         if (PassByPointer) {
            /* pass the pointer */
            EngineData->fm = (float **)FldValp;
            EngineData->fm_LocalAlloc = NOPE; /* allocation not done by Engine functions */
         }
         else { /* pass by value */
            if (EngineData->fm != NULL) {
               fprintf(SUMA_STDERR, "Error %s: Passing by value and EngineData->fm is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NULL);
            } 
            if (!EngineData->N_rows || !EngineData->N_cols) {
               fprintf(SUMA_STDERR, "Error %s: EngineData->N_rows or EngineData->N_cols is 0.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            EngineData->fm = (float **)SUMA_allocate2D(EngineData->N_rows, EngineData->N_cols, sizeof(float));
            if (EngineData->fm == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed to allocate fm.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            EngineData->fm_LocalAlloc = YUP; /* allocation done by Engine functions, this can be freed*/
            {/* copy the data */
               float **fm; 
               int i, j;
               fm = (float **)FldValp;
               for (i=0; i < EngineData->N_rows; ++i) {
                  for (j=0; j < EngineData->N_cols; ++j) 
                     EngineData->fm[i][j] = fm[i][j];
               }
            }/* copy the data */
         }/* pass by value */
         /* set the new destination*/
         EngineData->fm_Dest = Dest;
         EngineData->fm_Source = Src;
         break;
      case SEF_im:
         if (EngineData->im_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->im_Dest);
            SUMA_RETURN (NULL);
         }
         /* space available*/
         if (PassByPointer) {
            /* pass the pointer */
            EngineData->im = (int **)FldValp;
            EngineData->im_LocalAlloc = NOPE; /* allocation not done by Engine functions */
         }   else { /* pass by value */
            if (EngineData->im != NULL) {
               fprintf(SUMA_STDERR, "Error %s: Passing by value and EngineData->im is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NULL);
            } 
            if (!EngineData->N_rows || !EngineData->N_cols) {
               fprintf(SUMA_STDERR, "Error %s: EngineData->N_rows or EngineData->N_cols is 0.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            EngineData->im = (int **)SUMA_allocate2D(EngineData->N_rows, EngineData->N_cols, sizeof(int));
            if (EngineData->im == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed to allocate im.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            EngineData->im_LocalAlloc = YUP; /* allocation done by Engine functions, this can be freed*/
            {/* copy the data */
               int **im; 
               int i, j;
               im = (int **)FldValp;
               for (i=0; i < EngineData->N_rows; ++i) {
                  for (j=0; j < EngineData->N_cols; ++j) 
                     EngineData->im[i][j] = im[i][j];
               }
            }/* copy the data */
         }/* pass by value */
         /* set the new destination*/
         EngineData->im_Dest = Dest;
         EngineData->im_Source = Src;
         break;

      case SEF_ivec:
         if (EngineData->ivec_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->ivec_Dest);
            SUMA_RETURN (NULL);
         }
         ivec = (SUMA_IVEC *)FldValp;
         /* space available*/
         if (PassByPointer) {
            EngineData->ivec = ivec;
            EngineData->ivec_LocalAlloc = NOPE;
         } else {
            if (EngineData->fvec != NULL) {
               fprintf(SUMA_STDERR, "Error %s: Passing by value and EngineData->fvec is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            if (ivec->n <= 0 && ivec->v) {
               fprintf(SUMA_STDERR, "Error %s: ivec->n <= 0 while ivec->v is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            EngineData->ivec_LocalAlloc = YUP;
            EngineData->ivec = (SUMA_IVEC*)SUMA_malloc(sizeof(SUMA_IVEC));
            EngineData->ivec->v = (int *)SUMA_malloc(ivec->n * sizeof(int));
            if (!EngineData->ivec->v)  {
               SUMA_SL_Crit("Failed to allocate");
               SUMA_RETURN(NULL);
            }
            SUMA_COPY_VEC(ivec->v, EngineData->ivec->v, ivec->n, int, int);
            EngineData->ivec->n = ivec->n;
         }
         /* set the new destination*/
         EngineData->ivec_Dest = Dest;
         EngineData->ivec_Source = Src;
         break;
         
     case SEF_fvec:
         if (EngineData->fvec_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->fvec_Dest);
            SUMA_RETURN (NULL);
         }
         fvec = (SUMA_FVEC *)FldValp;
         /* space available*/
         if (PassByPointer) {
            EngineData->fvec = fvec;
            EngineData->fvec_LocalAlloc = NOPE;
         } else {
            if (EngineData->fvec != NULL) {
               fprintf(SUMA_STDERR, "Error %s: Passing by value and EngineData->fvec is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            if (fvec->n <= 0 && fvec->v) {
               fprintf(SUMA_STDERR, "Error %s: fvec->n <= 0 while fvec->v is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NULL);
            }
            EngineData->fvec_LocalAlloc = YUP;
            EngineData->fvec = (SUMA_FVEC*)SUMA_malloc(sizeof(SUMA_FVEC));
            EngineData->fvec->v = (float *)SUMA_malloc(fvec->n * sizeof(float));
            if (!EngineData->fvec->v)  {
               SUMA_SL_Crit("Failed to allocate");
               SUMA_RETURN(NULL);
            }
            SUMA_COPY_VEC(fvec->v, EngineData->fvec->v, fvec->n, float, float);
            EngineData->fvec->n = fvec->n;
         }
         /* set the new destination*/
         EngineData->fvec_Dest = Dest;
         EngineData->fvec_Source = Src;
         break;

      case SEF_i:
         if (EngineData->i_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->i_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            int *it;
            it = (int*)FldValp;
            EngineData->i = *it;
         }
         EngineData->i_Dest = Dest;
         EngineData->i_Source = Src;
         break;
         
      case SEF_f:
         if (EngineData->f_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->f_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            float *ft;
            ft = (float*)FldValp;
            EngineData->f = *ft;
         }
         EngineData->f_Dest = Dest;
         EngineData->f_Source = Src;
         break;

      case SEF_fv3:
         if (EngineData->fv3_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->fv3_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            float *fvt;
            int kt;
            fvt = (float*)FldValp;
            for (kt=0; kt < 3; ++kt) EngineData->fv3[kt] = fvt[kt];
         }
         EngineData->fv3_Dest = Dest;
         EngineData->fv3_Source = Src;
         break;

      case SEF_fv15:
         if (EngineData->fv15_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->fv15_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            float *fvt;
            int kt;
            fvt = (float*)FldValp;
            for (kt=0; kt < 15; ++kt) EngineData->fv15[kt] = fvt[kt];
         }
         EngineData->fv15_Dest = Dest;
         EngineData->fv15_Source = Src;
         break;
         
      case SEF_fv200:
         if (EngineData->fv200_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->fv200_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            float *fvt;
            int kt;
            fvt = (float*)FldValp;
            for (kt=0; kt < 200; ++kt) EngineData->fv200[kt] = fvt[kt];
         }
         EngineData->fv200_Dest = Dest;
         EngineData->fv200_Source = Src;
         break;
         
      case SEF_iv3:
         if (EngineData->iv3_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->iv3_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            int *ivt;
            int kt;
            ivt = (int*)FldValp;
            for (kt=0; kt < 3; ++kt) EngineData->iv3[kt] = ivt[kt];
         }
         EngineData->iv3_Dest = Dest;
         EngineData->iv3_Source = Src;
         break;
         
      case SEF_iv15:
         if (EngineData->iv15_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->iv15_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            int *ivt;
            int kt;
            ivt = (int*)FldValp;
            for (kt=0; kt < 15; ++kt) EngineData->iv15[kt] = ivt[kt];
         }
         EngineData->iv15_Dest = Dest;
         EngineData->iv15_Source = Src;
         break;

      case SEF_iv200:
         if (EngineData->iv200_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->iv200_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            int *ivt;
            int kt;
            ivt = (int*)FldValp;
            for (kt=0; kt < 200; ++kt) EngineData->iv200[kt] = ivt[kt];
         }
         EngineData->iv200_Dest = Dest;
         EngineData->iv200_Source = Src;
         break;
     
     case SEF_s:
         if (EngineData->s_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->s_Dest);
            SUMA_RETURN (NULL);
         }
         { /* assign by value */
            char *st;
            st = (char*)FldValp;
            if (strlen(st) < SUMA_MAX_STRING_LENGTH) {
               sprintf(EngineData->s,"%s", st);
            } else {
               fprintf(SUMA_STDERR, "Error %s: string in FldValp is longer than SUMA_MAX_STRING_LENGTH.\n", FuncName);
               SUMA_RETURN (NULL);
            }
         }
         EngineData->s_Dest = Dest;
         EngineData->s_Source = Src;
         break;
      
      case SEF_vp:
         if (EngineData->vp_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->vp_Dest);
            SUMA_RETURN (NULL);
         }
         EngineData->vp = (void *)FldValp;
         EngineData->vp_Dest = Dest;
         EngineData->vp_Source = Src;
         break;
            
      case SEF_ip:
         if (EngineData->ip_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->ip_Dest);
            SUMA_RETURN (NULL);
         }
         EngineData->ip = (int *)FldValp;
         EngineData->ip_Dest = Dest;
         break;
            
      case SEF_fp:
         if (EngineData->fp_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->fp_Dest);
            SUMA_RETURN (NULL);
         }
         EngineData->fp = (float *)FldValp;
         EngineData->fp_Dest = Dest;
         break;
             
      case SEF_cp:
         if (EngineData->cp_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %d has a preset destination (%d).\n", FuncName, Fld, EngineData->cp_Dest);
            SUMA_RETURN (NULL);
         }
         EngineData->cp = (char *)FldValp;
         EngineData->cp_Dest = Dest;
         break;
      
      default:
         fprintf(SUMA_STDERR, "Error %s: Not setup for field %d yet.\n", FuncName, Fld);
         SUMA_RETURN (NULL);
         break;
   }/* switch Fld */
    
   /* Now EngineData is filled up, add an element (if not present already) to the list with EngineData */
   switch (InsertAt) {
      case SEI_In:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Element already in list.\n", FuncName);
         NewElement = Element;
         break;
      case SEI_Tail:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Inserting new element at end of list \n", FuncName); 
         if (dlist_ins_next (list, dlist_tail(list), (void *)EngineData) < 0) {
            fprintf (SUMA_STDERR, "Error %s: Failed to insert element in list.\n", FuncName);
            SUMA_RETURN(NULL);
         }
         NewElement = dlist_tail(list);
         break;
      case SEI_Head:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Inserting new element at beginning of list \n", FuncName); 
         if (dlist_ins_prev (list, dlist_head(list), (void *)EngineData) < 0) {
            fprintf (SUMA_STDERR, "Error %s: Failed to insert element in list.\n", FuncName);
            SUMA_RETURN(NULL);
         }
         NewElement = dlist_head(list);
         break;
      case SEI_Before:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Inserting new element before specified element.\n", FuncName);
         if (!Element) fprintf (SUMA_STDERR, "Error %s: NULL Element!\n", FuncName);
         if (dlist_ins_prev (list, Element, (void *)EngineData) < 0) {
            fprintf (SUMA_STDERR, "Error %s: Failed to insert element in list.\n", FuncName);
            SUMA_RETURN(NULL);
         }
         NewElement = Element->prev;
         if (!NewElement) {
            fprintf (SUMA_STDERR, "Error %s: No previous element. List size %d", FuncName, dlist_size(list));
            SUMA_RETURN(NULL);
         }
         break;
      case SEI_After:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Inserting new element after specified element.\n", FuncName);
         if (!Element) fprintf (SUMA_STDERR, "Error %s: NULL Element!\n", FuncName);
         if (dlist_ins_next (list, Element, (void *)EngineData) < 0) {
            fprintf (SUMA_STDERR, "Error %s: Failed to insert element in list.\n", FuncName);
            SUMA_RETURN(NULL);
         }
         NewElement = Element->next;
         if (!NewElement) {
            fprintf (SUMA_STDERR, "Error %s: No next element. List size %d", FuncName, dlist_size(list));
            SUMA_RETURN(NULL);
         }
         break;
      case SEI_WTSDS:
      case SEI_BadLoc:
      default:
         fprintf (SUMA_STDERR, "Error %s: Bad insertion location!\n", FuncName);
         SUMA_RETURN(NULL);
         break;
         
   }

   SUMA_RETURN (NewElement);   
}

/*!
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *ED, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer)
\param ED (SUMA_EngineData *) pointer to EngineData structure
\param Fldname (char *) Field name
\param FldValp (void *) Pointer to the value that is to be placed in Fldname
\param DestName (char *) Name of EngineCommand that the data in Fldname is destined to
\param PassByPointer (SUMA_Boolean) flag (YUP/NOPE), if YUP then assignment is done at the pointer level (ED->Fld = FldValp)
                        if NOPE then space is allocated for Fldname and values are copied from FldValp[i][j] to ED->Fld[i][j]

\ret YUP/NOPE

+PassByPointer option is only useful when dealing with fields that are/can be dynamically allocated like fm and fi. For fields like fv3 or iv15 then assignments are done by value and not pointers.
+You cannot set the value of a field unless the destination for the pre-existing data in that field has been reached                        
+When passing by value for fields requiring allocation, like fm or fi, you must be sure that ED->N_cols and ED->N_rows are
set correctly before you call the function.

\sa SUMA_EngineDataFieldCode
\sa SUMA_ReleaseEngineData
\sa SUMA_InitializeEngineData
\sa SUMA_FreeEngineData
\sa EngineData
\sa SUMA_define.h

NOTE: OBSOLETE, use SUMA_RegisterEngineListCommand
*/

SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *ED, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer)
{ /* SUMA_RegisterEngineData*/
   int Dest, Fld, Src;
   static char FuncName[]={"SUMA_RegisterEngineData"};
   
   SUMA_ENTRY;

   fprintf (SUMA_STDERR, "Error %s: This function is now obsolete. Must use SUMA_RegisterEngineListCommand instead.\n", FuncName);
   SUMA_RETURN (NOPE);
   
   Dest = SUMA_CommandCode((char *)DestName);
   Fld = SUMA_EngineFieldCode((char *)Fldname);
   Src = SUMA_EngineSourceCode ((char *)SourceName);
   
   /* make sure Destination is good and wholesome*/
   switch (Dest) {
      case SE_BadCode:
         fprintf (SUMA_STDERR, "Error in %s: Bad code string.\n", FuncName);
         SUMA_RETURN (NOPE);
         break;
      case SE_Empty:
         fprintf (SUMA_STDERR, "Error in %s: Empty code string.\n", FuncName);
         SUMA_RETURN (NOPE);
         break;
      default:
         break;
   }
   
   /*fprintf(SUMA_STDOUT, "%s: Registering %s for %s\n", FuncName, Fldname, DestName);*/
   
   switch (Fld) { /* switch Fld */
      case SEF_fm:
         if (ED->fm_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->fm_Dest);
            SUMA_RETURN (NOPE);
         }
         /* space available*/
         if (PassByPointer) {
            /* pass the pointer */
            ED->fm = (float **)FldValp;
            ED->fm_LocalAlloc = NOPE; /* allocation not done by Engine functions */
         }
         else { /* pass by value */
            if (ED->fm != NULL) {
               fprintf(SUMA_STDERR, "Error %s: Passing by value and ED->fm is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NOPE);
            } 
            if (!ED->N_rows || !ED->N_cols) {
               fprintf(SUMA_STDERR, "Error %s: ED->N_rows or ED->N_cols is 0.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            ED->fm = (float **)SUMA_allocate2D(ED->N_rows, ED->N_cols, sizeof(float));
            if (ED->fm == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed to allocate fm.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            ED->fm_LocalAlloc = YUP; /* allocation done by Engine functions, this can be freed*/
            {/* copy the data */
               float **fm; 
               int i, j;
               fm = (float **)FldValp;
               for (i=0; i < ED->N_rows; ++i) {
                  for (j=0; j < ED->N_cols; ++j) 
                     ED->fm[i][j] = fm[i][j];
               }
            }/* copy the data */
         }/* pass by value */
         /* set the new destination*/
         ED->fm_Dest = Dest;
         ED->fm_Source = Src;
         SUMA_RETURN (YUP);   
         break;
      case SEF_im:
         if (ED->im_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->im_Dest);
            SUMA_RETURN (NOPE);
         }
         /* space available*/
         if (PassByPointer) {
            /* pass the pointer */
            ED->im = (int **)FldValp;
            ED->im_LocalAlloc = NOPE; /* allocation not done by Engine functions */
         }   else { /* pass by value */
            if (ED->im != NULL) {
               fprintf(SUMA_STDERR, "Error %s: Passing by value and ED->im is not NULL. Clean up your act.\n", FuncName);
               SUMA_RETURN(NOPE);
            } 
            if (!ED->N_rows || !ED->N_cols) {
               fprintf(SUMA_STDERR, "Error %s: ED->N_rows or ED->N_cols is 0.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            ED->im = (int **)SUMA_allocate2D(ED->N_rows, ED->N_cols, sizeof(int));
            if (ED->im == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed to allocate im.\n", FuncName);
               SUMA_RETURN(NOPE);
            }
            ED->im_LocalAlloc = YUP; /* allocation done by Engine functions, this can be freed*/
            {/* copy the data */
               int **im; 
               int i, j;
               im = (int **)FldValp;
               for (i=0; i < ED->N_rows; ++i) {
                  for (j=0; j < ED->N_cols; ++j) 
                     ED->im[i][j] = im[i][j];
               }
            }/* copy the data */
         }/* pass by value */
         /* set the new destination*/
         ED->im_Dest = Dest;
         ED->im_Source = Src;
         SUMA_RETURN (YUP);   
         break;

      case SEF_i:
         if (ED->i_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->i_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            int *it;
            it = (int*)FldValp;
            ED->i = *it;
         }
         ED->i_Dest = Dest;
         ED->i_Source = Src;
         SUMA_RETURN (YUP);   
         break;
         
      case SEF_f:
         if (ED->f_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->f_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            float *ft;
            ft = (float*)FldValp;
            ED->f = *ft;
         }
         ED->f_Dest = Dest;
         ED->f_Source = Src;
         SUMA_RETURN (YUP);   
         break;

      case SEF_fv3:
         if (ED->fv3_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->fv3_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            float *fvt;
            int kt;
            fvt = (float*)FldValp;
            for (kt=0; kt < 3; ++kt) ED->fv3[kt] = fvt[kt];
         }
         ED->fv3_Dest = Dest;
         ED->fv3_Source = Src;
         SUMA_RETURN (YUP);   
         break;

      case SEF_fv15:
         if (ED->fv15_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->fv15_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            float *fvt;
            int kt;
            fvt = (float*)FldValp;
            for (kt=0; kt < 15; ++kt) ED->fv15[kt] = fvt[kt];
         }
         ED->fv15_Dest = Dest;
         ED->fv15_Source = Src;
         SUMA_RETURN (YUP);   
         break;
         
      case SEF_fv200:
         if (ED->fv200_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->fv200_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            float *fvt;
            int kt;
            fvt = (float*)FldValp;
            for (kt=0; kt < 200; ++kt) ED->fv200[kt] = fvt[kt];
         }
         ED->fv200_Dest = Dest;
         ED->fv200_Source = Src;
         SUMA_RETURN (YUP);   
         break;
         
      case SEF_iv3:
         if (ED->iv3_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->iv3_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            int *ivt;
            int kt;
            ivt = (int*)FldValp;
            for (kt=0; kt < 3; ++kt) ED->iv3[kt] = ivt[kt];
         }
         ED->iv3_Dest = Dest;
         ED->iv3_Source = Src;
         SUMA_RETURN (YUP);   
         break;
         
      case SEF_iv15:
         if (ED->iv15_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->iv15_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            int *ivt;
            int kt;
            ivt = (int*)FldValp;
            for (kt=0; kt < 15; ++kt) ED->iv15[kt] = ivt[kt];
         }
         ED->iv15_Dest = Dest;
         ED->iv15_Source = Src;
         SUMA_RETURN (YUP);   
         break;

      case SEF_iv200:
         if (ED->iv200_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->iv200_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            int *ivt;
            int kt;
            ivt = (int*)FldValp;
            for (kt=0; kt < 200; ++kt) ED->iv200[kt] = ivt[kt];
         }
         ED->iv200_Dest = Dest;
         ED->iv200_Source = Src;
         SUMA_RETURN (YUP);   
         break;

      case SEF_s:
         if (ED->s_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->s_Dest);
            SUMA_RETURN (NOPE);
         }
         { /* assign by value */
            char *st;
            st = (char*)FldValp;
            if (strlen(st) < SUMA_MAX_STRING_LENGTH) {
               sprintf(ED->s,"%s", st);
            } else {
               fprintf(SUMA_STDERR, "Error %s: string in FldValp is longer than SUMA_MAX_STRING_LENGTH.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
         }
         ED->s_Dest = Dest;
         ED->s_Source = Src;
         SUMA_RETURN (YUP);   
         break;
      
      case SEF_vp:
         if (ED->vp_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->vp_Dest);
            SUMA_RETURN (NOPE);
         }
         ED->vp = (void *)FldValp;
         ED->vp_Dest = Dest;
         ED->vp_Source = Src;
         SUMA_RETURN (YUP);   
         break;
            
      case SEF_ip:
         if (ED->ip_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->ip_Dest);
            SUMA_RETURN (NOPE);
         }
         ED->ip = (int *)FldValp;
         ED->ip_Dest = Dest;
         SUMA_RETURN (YUP);   
         break;
      
      case SEF_fp:
         if (ED->fp_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->fp_Dest);
            SUMA_RETURN (NOPE);
         }
         ED->fp = (float *)FldValp;
         ED->fp_Dest = Dest;
         SUMA_RETURN (YUP);   
         break;
      
      case SEF_cp:
         if (ED->cp_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
            fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, ED->cp_Dest);
            SUMA_RETURN (NOPE);
         }
         ED->cp = (char *)FldValp;
         ED->cp_Dest = Dest;
         SUMA_RETURN (YUP);   
         break;
      
      default:
         fprintf(SUMA_STDERR, "Error %s: Not setup for field %s yet.\n", FuncName, Fldname);
         SUMA_RETURN (NOPE);
         break;
   }/* switch Fld */
    

}/* SUMA_RegisterEngineData*/

/*!
   \brief allocate and initialize the data structure for EngineData
   SUMA_EngineData *SUMA_InitializeEngineListData (SUMA_ENGINE_CODE CommandCode);
   \param CommandCode (SUMA_ENGINE_CODE) command code to store in Engine data structure
   \return ED (SUMA_EngineData *) Pointer to empty engine data structure with the field ED->CommandCode set to CommandCode
      NULL if function failed.
     
*/
SUMA_EngineData *SUMA_InitializeEngineListData (SUMA_ENGINE_CODE CommandCode)
{
   static char FuncName[]={"SUMA_InitializeEngineListData"};
   SUMA_EngineData *ED=NULL;
   int i;
   
   SUMA_ENTRY;

   if (CommandCode <= SE_Empty || CommandCode >= SE_BadCode) {
      fprintf(SUMA_STDERR,"Error %s: Bad command code.\n", FuncName);
      SUMA_RETURN (NULL); 
   }
   
   ED = SUMA_malloc(sizeof(SUMA_EngineData));
   if (!ED) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for ED.\n", FuncName);
      SUMA_RETURN (NULL);   
   }
   
   ED->CommandCode = CommandCode;
   ED->Srcp = NULL;
   ED->fm = NULL;
   ED->fm_LocalAlloc = NOPE;
   ED->im = NULL;
   ED->im_LocalAlloc = NOPE;
   ED->N_rows = 0;
   ED->N_cols = 0;
   ED->ivec = NULL;
   ED->fvec = NULL;
   ED->ivec_LocalAlloc = NOPE;
   ED->fvec_LocalAlloc = NOPE;
   ED->i = 0;
   ED->f = 0.0;
   ED->iv3[0] = ED->iv3[1] = ED->iv3[2] = 0;
   ED->fv3[0] = ED->fv3[1] = ED->fv3[2] = 0.0;
   for (i=0; i < 15; ++i) {
      ED->fv15[i] = 0.0;
      ED->iv15[i] = 0;
   }
   for (i=0; i < 200; ++i) {
      ED->fv200[i] = 0.0;
      ED->iv200[i] = 0;
   }
   sprintf(ED->s,"NOTHING");
   
   ED->vp = NULL;
   
   ED->fm_Dest = ED->im_Dest = ED->i_Dest = ED->f_Dest = ED->iv3_Dest = ED->fv3_Dest = \
   ED->fv15_Dest = ED->iv15_Dest = ED->fv200_Dest = ED->iv200_Dest = ED->s_Dest = ED->vp_Dest = ED->ip_Dest = ED->fp_Dest = \
   ED->cp_Dest = ED->ivec_Dest = ED->fvec_Dest = SE_Empty;
   
   ED->fm_Source = ED->im_Source = ED->i_Source = ED->f_Source = ED->iv3_Source = ED->fv3_Source = \
   ED->fv15_Source = ED->iv15_Source = ED->fv200_Source = ED->iv200_Source = ED->s_Source = ED->vp_Source = \
   ED->ivec_Source = ED->fvec_Source = SES_Empty;
   
   SUMA_RETURN (ED);

}


/*!
   Free an action stack data structure 
   -frees AS_data->ActionData by calling destructor function 
   -frees AS_data
*/
void SUMA_FreeActionStackData(void *asdata)
{
   static char FuncName[]={"SUMA_FreeActionStackData"};
   SUMA_ACTION_STACK_DATA *AS_data=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   AS_data=(SUMA_ACTION_STACK_DATA *)asdata;
   if (AS_data) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Destroying Action Stack Data \n", FuncName);
      /* first you want to free the Action Data */
      AS_data->ActionDataDestructor(AS_data->ActionData);
      
      /* Now you can free the Action Stucture Data */
      SUMA_free(AS_data);
   }
   
   SUMA_RETURNe;
}

/*!
   Releases an action stack data structure 
   -frees AS_data->ActionData WITHOUT calling destructor function 
   -frees AS_data
*/
void SUMA_ReleaseActionStackData (void *asdata)
{
   static char FuncName[]={"SUMA_ReleaseActionStackData"};
   SUMA_ACTION_STACK_DATA *AS_data=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   AS_data=(SUMA_ACTION_STACK_DATA *)asdata;
   if (AS_data) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Releasing Action Stack Data structure\n", FuncName);
      /* first you want to free the Action Data */
      if (AS_data->ActionData) SUMA_free(AS_data->ActionData);

      /* Now you can free the Action Stucture Data */
      SUMA_free(AS_data);
   }
   
   SUMA_RETURNe;
}
/*!
   \brief free a message structure and any allocated space in its fields
   
   \param Hv (void *) pointer to Message structure. 
            It is type cast to void* to suit the standard list manipulation routines.
*/
void SUMA_FreeMessageListData(void *Hv)
{
   static char FuncName[]={"SUMA_FreeMessageListData"};
   SUMA_MessageData *H = NULL;
   
   SUMA_ENTRY;
   
   H = (SUMA_MessageData *)Hv;
   
   if (!H) {
      fprintf(SUMA_STDERR,"Warning %s: H is null, nothing to do!\n", FuncName);
      SUMA_RETURNe;
   }
   
#if 1   /* Message and Source are never allocated and should not be freed. */
        /*                                            2003 June 19 [rickr] */
        /* Message and Source are now allocated */
        /*                 2003 June 23 [zsaad] */
   if (H->Message) SUMA_free(H->Message);
   if (H->Source) SUMA_free(H->Source);
#endif

   if (H) SUMA_free(H);
   
   SUMA_RETURNe;
}

/*!
   \brief free an engine data structure and any allocated space in its fields
   SUMA_FreeEngineListData (EDv);
   
   \param EDv (void *) pointer to EngineData structure. 
            It is type cast to void* to suit the standard list manipulation routines.

if space for im or fm has been dynamically allocated in SUMA_RegisterEngineListCommand()
it is released. 

*/
void SUMA_FreeEngineListData(void *EDv)
{
   static char FuncName[]={"SUMA_FreeEngineListData"};
   SUMA_EngineData *ED = NULL;
   
   SUMA_ENTRY;

   ED = (SUMA_EngineData *)EDv;
   
   if (ED == NULL) {
      fprintf(SUMA_STDERR,"Warning %s: ED is null, nothing to do!\n", FuncName);
      SUMA_RETURNe;
   }
   
   /* check on Dynamic Memory Allocations needs */
   if (ED->fm_LocalAlloc) {
      if (!ED->N_rows || !ED->N_cols) {
         fprintf(SUMA_STDERR,"Error %s: N_rows or N_cols are 0.\n\a", FuncName);
         SUMA_RETURNe;
      }
      if (ED->fm == NULL) {
         fprintf(SUMA_STDERR,"Error %s: ED->fm is NULL, not good here.\n\a", FuncName);
         SUMA_RETURNe;
      }
      /* OK, free ED->fm */
      SUMA_free2D((char **)ED->fm, ED->N_rows);
   } 
   
   if (ED->im_LocalAlloc) {
      if (!ED->N_rows || !ED->N_cols) {
         fprintf(SUMA_STDERR,"Error %s: N_rows or N_cols are 0.\n\a", FuncName);
         SUMA_RETURNe;
      }
      if (ED->im == NULL) {
         fprintf(SUMA_STDERR,"Error %s: ED->im is NULL, not good here.\n\a", FuncName);
         SUMA_RETURNe;
      }
      /* OK, free ED->im */
      SUMA_free2D((char **)ED->im, ED->N_rows);
   } 
   
   /* good deal, flush ED */
   SUMA_free(ED);
   SUMA_RETURNe;
}
   
/*!
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *ED)

free memory allocated for ED in SUMA_InitializeEngineData

\param ED (SUMA_EngineData *) pointer to SUMA_EngineData

\ret YUP/NOPE

if space for im or fm has been dynamically allocated in SUMA_RegisterEngineData
it is released. ED itself is not freed

OBSOLETE: Use SUMA_FreeEngineListData
   
*/
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *ED)
{
   static char FuncName[]={"SUMA_FreeEngineData"};
   
   SUMA_ENTRY;

   fprintf (SUMA_STDERR, "Error %s: This function is now obsolete. Must use SUMA_FreeEngineListData instead.\n", FuncName);
   SUMA_RETURN (NOPE);

   if (ED == NULL) {
      fprintf(SUMA_STDERR,"Error %s: ED is null, nothing to do!\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* check on Dynamic Memory Allocations needs */
   if (ED->fm_LocalAlloc) {
      if (!ED->N_rows || !ED->N_cols) {
         fprintf(SUMA_STDERR,"Error %s: N_rows or N_cols are 0.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      if (ED->fm == NULL) {
         fprintf(SUMA_STDERR,"Error %s: ED->fm is NULL, not good here.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      /* OK, free ED->fm */
      SUMA_free2D((char **)ED->fm, ED->N_rows);
   } 
   
   if (ED->im_LocalAlloc) {
      if (!ED->N_rows || !ED->N_cols) {
         fprintf(SUMA_STDERR,"Error %s: N_rows or N_cols are 0.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      if (ED->im == NULL) {
         fprintf(SUMA_STDERR,"Error %s: ED->im is NULL, not good here.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      /* OK, free ED->im */
      SUMA_free2D((char **)ED->im, ED->N_rows);
   } 
   
   /* good deal, DO NOT flush ED in case it was not dynamically allocated*/
   SUMA_RETURN (YUP);
}

/*!
   \brief removes an element from the list, frees the data structure associated with the removed element
   ans = SUMA_ReleaseEngineListElement (list, element) 
   \param list (DList *)
   \param element (DListElmt *)
   \return (YUP/NOPE), success, failure
   
   - The list is not destroyed if no elements remain in it, 
   you should check for that after this function returns.
*/
SUMA_Boolean SUMA_ReleaseEngineListElement (DList *list, DListElmt *element) 
{
   static char FuncName[]={"SUMA_ReleaseEngineListElement"};
   void *ED=NULL;
   
   SUMA_ENTRY;
   if (!element) {
      SUMA_SL_Err("Null element");
      SUMA_RETURN (NOPE);
   }
   if (dlist_remove (list, element, &ED) < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed to remove element from list.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   if (ED) { /* had an if (!ED) here..... */
      SUMA_FreeEngineListData((SUMA_EngineData *)ED);
   }
   
   SUMA_RETURN (YUP);
}

/*!
   \brief removes an element from the list, frees the data structure associated with the removed element
   ans = SUMA_ReleaseMessageListElement (list, element) 
   \param list (DList *)
   \param element (DListElmt *)
   \return (YUP/NOPE), success, failure
   
   - The list is not destroyed if no elements remain in it, 
   you should check for that after this function returns.
*/
SUMA_Boolean SUMA_ReleaseMessageListElement (DList *list, DListElmt *element) 
{
   static char FuncName[]={"SUMA_ReleaseMessageListElement"};
   void *H=NULL;
   
   SUMA_ENTRY;
   
   if (dlist_remove (list, element, &H) < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed to remove element from list.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   if (H) {/* had an if (!H) here..... */
      SUMA_FreeMessageListData((SUMA_MessageData *)H);
   }
   
   SUMA_RETURN (YUP);
}


/*!
   \brief destroys a list IF IT IS EMPTY !
   list = SUMA_DestroyList (list);
   
   \param list (DList *)
   \return ans (DList *) NULL if function succeeds, list if function fails
   
   ++ list is also freed (Jan. 29 04)
   \sa SUMA_EmptyDestroyList
*/
DList * SUMA_DestroyList (DList *list) 
{
   static char FuncName[]={"SUMA_DestroyList"};
   
   SUMA_ENTRY;
   
   if (list->size) {
      fprintf (SUMA_STDERR, "Error %s: list still contains elements.\n", FuncName);
      SUMA_RETURN (list);
   }
   
   dlist_destroy(list);
   if (list) SUMA_free(list);
   SUMA_RETURN (NULL);   
} 

/*!
   \brief destroys a list even if it is not empty
   list = SUMA_DestroyList (list);
   
   \param list (DList *)
   \return ans (DList *) NULL always
   ++ list is also freed (Jan. 29 04)
   \sa SUMA_DestroyList
*/
DList * SUMA_EmptyDestroyList (DList *list) 
{
   static char FuncName[]={"SUMA_EmptyDestroyList"};
   
   SUMA_ENTRY;
   
   dlist_destroy(list);
   if (list) SUMA_free(list);
   SUMA_RETURN (NULL);   
} 

/*!
   \brief creates a list for the Action Stack
   list = SUMA_CreateActionStack ();
   \return list (DList *) pointer to doubly linked list
            NULL if function fails
            
*/
DList *SUMA_CreateActionStack (void)
{
   static char FuncName[]={"SUMA_CreateActionStack"};
   DList *list=NULL;
   
   SUMA_ENTRY;

   list = (DList *)malloc (sizeof(DList));
   if (!list) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for list.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   /* 
      Do not use: dlist_init(list, SUMA_FreeActionStackData);
      You do not want to destroy data stored inside the ActionStack because
      the data is used elsewhere. Destruction of ActionStackData should
      only be done when a stack element is above the new Do element and will
      therefore never be used again. */
   
   dlist_init(list, SUMA_ReleaseActionStackData);
   
   SUMA_RETURN (list);
}

/*!
   \brief destroys the Action Stack
   ans = SUMA_DestroyActionStack (DList *AS);
   
   \returns NULL 
            
*/
DList *SUMA_EmptyDestroyActionStack (DList *AS)
{
   static char FuncName[]={"SUMA_DestroyActionStack"};

   SUMA_ENTRY;

   dlist_destroy(AS);
   
   SUMA_RETURN (NULL);
}

/*!
   \brief creates a list for the message list
   list = SUMA_CreateMessageList ();
   \return list (DList *) pointer to doubly linked list
            NULL if function fails
            
            DO not use common field variables here.
*/
DList *SUMA_CreateMessageList (void)
{
   static char FuncName[]={"SUMA_CreateMessageList"};
   DList *list=NULL;
   
   
   list = (DList *)malloc (sizeof(DList));
   if (!list) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for list.\n", FuncName);
      return (NULL);
   }
   
   dlist_init(list, SUMA_FreeMessageListData);
   
   return (list);
}

/*!
   \brief creates a list for SUMA_Engine
   list = SUMA_CreateList ();
   \return list (DList *) pointer to doubly linked list
            NULL if function fails
*/
DList *SUMA_CreateList (void)
{
   static char FuncName[]={"SUMA_CreateList"};
   DList *list=NULL;
   
   SUMA_ENTRY;
   
   list = (DList *)SUMA_malloc (sizeof(DList));
   if (!list) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for list.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   dlist_init(list, SUMA_FreeEngineListData);
   
   SUMA_RETURN (list);
}

/*!
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *ED, char *Location)

This function releases data fields that were destined to Location

\param ED (SUMA_EngineData *) pointer to Engine data structure
\param Location (char *) location in SUMA_Engine, from which the function was called (one of the commands in SUMA_Engine)

\ret YUP/NOPE

Memory is freed for fm and im only if their assignment in SUMA_RegisterEngineData was done by value 

\sa SUMA_EngineDataFieldCode
\sa SUMA_ReleaseEngineData
\sa SUMA_InitializeEngineData
\sa SUMA_FreeEngineData
\sa EngineData
\sa SUMA_define.h
\sa SUMA_RegisterEngineData

OBSOLETE: Use SUMA_ReleaseEngineListElement
*/
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *ED, char *Location)
{/* SUMA_ReleaseEngineData*/
   static char FuncName[]={"SUMA_ReleaseEngineData"};
   int Loc;

   SUMA_ENTRY;

   fprintf (SUMA_STDERR, "Error %s: This function is now obsolete. Must use SUMA_ReleaseEngineListElement instead.\n", FuncName);
   SUMA_RETURN (NOPE);

   /* search through all fields and clear (or release) all those who should be */
   Loc = SUMA_CommandCode((char *)Location);
   
   /* make sure Destination is good and wholesome*/
   switch (Loc) {
      case SE_BadCode:
         fprintf (SUMA_STDERR, "Error in %s: Bad code string.\n", FuncName);
         SUMA_RETURN (NOPE);
         break;
      case SE_Empty:
         fprintf (SUMA_STDERR, "Error in %s: Empty code string.\n", FuncName);
         SUMA_RETURN (NOPE);
         break;
      default:
         break;
   }

   /*fprintf(SUMA_STDOUT,"%s : Releasing location %s\n", FuncName, Location);*/
   /* go through all the fields*/
   /* fm */
   if (ED->fm_Dest == Loc) {
      /* needs to be released */
      if (ED->fm_LocalAlloc) { /* locally allocated */
         /* must be freed */
         if (!ED->N_rows || !ED->N_cols) {
            fprintf (SUMA_STDERR, "Error in %s: ED->N_rows or ED->N_cols is 0 .\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->fm == NULL) {
            fprintf (SUMA_STDERR, "Error in %s: fm is null already. This should not be .\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         SUMA_free2D((char **)ED->fm, ED->N_rows);
         ED->N_rows = ED->N_cols = 0; 
         ED->fm = NULL;
         ED->fm_Dest = SE_Empty;
         ED->fm_Source = SES_Empty;
      } /* locally allocated */ else { /* passed by pointer */
         ED->fm = NULL; 
         ED->N_rows = ED->N_cols = 0; 
         ED->fm_Dest = SE_Empty;
         ED->fm_Source = SES_Empty;
      }/* passed by pointer */
   }
   
   /*im*/
   if (ED->im_Dest == Loc) {
      /* needs to be released */
      if (ED->im_LocalAlloc) { /* locally allocated */
         /* must be freed */
         if (!ED->N_rows || !ED->N_cols) {
            fprintf (SUMA_STDERR, "Error in %s: ED->N_rows or ED->N_cols is 0 .\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->im == NULL) {
            fprintf (SUMA_STDERR, "Error in %s: im is null already. This should not be .\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         SUMA_free2D((char **)ED->im, ED->N_rows);
         ED->N_rows = ED->N_cols = 0; 
         ED->im = NULL;
         ED->im_Dest = SE_Empty;
         ED->im_Source = SES_Empty;
      } /* locally allocated */ else { /* passed by pointer */
         ED->im = NULL; 
         ED->N_rows = ED->N_cols = 0; 
         ED->im_Dest = SE_Empty;
         ED->im_Source = SES_Empty;
      }/* passed by pointer */
   }

   /* ivec */
   if (ED->ivec_Dest == Loc) {
      /* needs to be released */
      if (ED->ivec_LocalAlloc) { /* locally allocated */
         /* must be freed */
         if (!ED->ivec) {
            fprintf (SUMA_STDERR, "Error %s: NULL ivec, why?\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->ivec->n > 0 && !ED->ivec->v) {
            fprintf (SUMA_STDERR, "Error %s: ED->ivec->n >= 0 && ED->ivec->v = NULL.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->ivec->v == NULL) {
            fprintf (SUMA_STDERR, "Error in %s: v is null already. This should not be .\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->ivec->v) SUMA_free(ED->ivec->v); 
         if (ED->ivec) SUMA_free(ED->ivec); 
         ED->ivec = NULL;
         ED->ivec_Dest = SE_Empty;
         ED->ivec_Source = SES_Empty;
      } /* locally allocated */ else { /* passed by pointer */
         ED->ivec = NULL; 
         ED->ivec_Dest = SE_Empty;
         ED->ivec_Source = SES_Empty;
      }/* passed by pointer */
   }
   
   /* fvec */
   if (ED->fvec_Dest == Loc) {
      /* needs to be released */
      if (ED->fvec_LocalAlloc) { /* locally allocated */
         /* must be freed */
         if (!ED->fvec) {
            fprintf (SUMA_STDERR, "Error %s: NULL fvec, why?\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->fvec->n > 0 && !ED->fvec->v) {
            fprintf (SUMA_STDERR, "Error %s: ED->fvec->n >= 0 && ED->fvec->v = NULL.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->fvec->v == NULL) {
            fprintf (SUMA_STDERR, "Error in %s: v is null already. This should not be .\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (ED->fvec->v) SUMA_free(ED->fvec->v); 
         if (ED->fvec) SUMA_free(ED->fvec); 
         ED->fvec = NULL;
         ED->fvec_Dest = SE_Empty;
         ED->fvec_Source = SES_Empty;
      } /* locally allocated */ else { /* passed by pointer */
         ED->fvec = NULL; 
         ED->fvec_Dest = SE_Empty;
         ED->fvec_Source = SES_Empty;
      }/* passed by pointer */
   }

  /* i */
   if (ED->i_Dest == Loc) {
      ED->i_Dest = SE_Empty;
      ED->i_Source = SES_Empty;
   }

   /* iv3 */
   if (ED->iv3_Dest == Loc) {
      ED->iv3_Dest = SE_Empty;
      ED->iv3_Source = SES_Empty;
   }
   
   /* iv15 */
   if (ED->iv15_Dest == Loc) {
      ED->iv15_Dest = SE_Empty;
      ED->iv15_Source = SES_Empty;
   }

   /* iv200 */
   if (ED->iv200_Dest == Loc) {
      ED->iv200_Dest = SE_Empty;
      ED->iv200_Source = SES_Empty;
   }
   
   /* f */
   if (ED->f_Dest == Loc) {
      ED->f_Dest = SE_Empty;
      ED->f_Source = SES_Empty;
   }

   /* fv3 */
   if (ED->fv3_Dest == Loc) {
      ED->fv3_Dest = SE_Empty;
      ED->fv3_Source = SES_Empty;
   }
   
   /* fv15 */
   if (ED->fv15_Dest == Loc) {
      ED->fv15_Dest = SE_Empty;
      ED->fv15_Source = SES_Empty;
   }
   
   /* fv200 */
   if (ED->fv200_Dest == Loc) {
      ED->fv200_Dest = SE_Empty;
      ED->fv200_Source = SES_Empty;
   }

   /*s*/
   if (ED->s_Dest == Loc) {
      ED->s_Dest = SE_Empty;
      ED->s_Source = SES_Empty;
   }

   /* vp */
   if (ED->vp_Dest == Loc) {
      ED->vp_Dest = SE_Empty;
      ED->vp_Source = SES_Empty;
   }
   
   /* cp */
   if (ED->cp_Dest == Loc) {
      ED->cp_Dest = SE_Empty;
   }
   
   /* ip */
   if (ED->ip_Dest == Loc) {
      ED->ip_Dest = SE_Empty;
   }
   
   /* fp */
   if (ED->fp_Dest == Loc) {
      ED->fp_Dest = SE_Empty;
   }
   /* SUMA_RETURN, tout va bien */
   SUMA_RETURN (YUP);
}

/*!
   \brief Writes the commands to be executed in list 
          SUMA_ShowList (list, Out);
   \param list (DList *) Pointer to list
   \param Out (FILE *) pointer to output stream, if NULL then Out is stdout
   \return (void)
   
*/
void SUMA_ShowList (DList *list, FILE *Out)
{
   static char FuncName[]={"SUMA_ShowList"};
   DListElmt *NE;
   SUMA_EngineData *ED;
   
   SUMA_ENTRY;

   if (!Out) Out = stdout;
   
   if (!list) {
      fprintf (Out,"%s: NULL List.\n", FuncName);
      SUMA_RETURNe;
   }
   
   if (!list->size) {
      fprintf (Out,"%s: Empty List.\n", FuncName);
      SUMA_RETURNe;
   }
   
   fprintf (Out,"%s: List of %d elements.\n\t", FuncName, list->size);
   do{
      NE = dlist_head(list);
      ED = (SUMA_EngineData *) NE->data;
      if (!ED) {
         fprintf (Out, "NULL-This should not be | ");
      } else {
         fprintf (Out, "%s | ", SUMA_CommandString (ED->CommandCode));
      }
   } while (!dlist_is_tail(NE));
   
   fprintf (Out,"\n");
   
   SUMA_RETURNe;
}

/*!
   \brief looks for words in str to guess what surface type
   is being mentioned
*/
SUMA_SO_File_Type SUMA_guess_surftype_argv(char *str)
{
   static char FuncName[]={"SUMA_guess_surftype_argv"};
   SUMA_SO_File_Type tp=SUMA_FT_NOT_SPECIFIED;

   SUMA_ENTRY;

   if (SUMA_iswordin_ci(str, "FreeSurfer") == 1 || SUMA_iswordin_ci(str, "fs") == 1 ) {
      SUMA_RETURN( SUMA_FREE_SURFER );
   }
   if (SUMA_iswordin_ci(str, "SureFit")  == 1 || SUMA_iswordin_ci(str, "sf") == 1  || SUMA_iswordin_ci(str, "caret") == 1 ) SUMA_RETURN( SUMA_SUREFIT );
   if (SUMA_iswordin_ci(str, "Inventor")  == 1 || SUMA_iswordin_ci(str, "iv") == 1  || SUMA_iswordin_ci(str, "inv") == 1 ) SUMA_RETURN( SUMA_INVENTOR_GENERIC );
   if (SUMA_iswordin_ci(str, "dx")  == 1 ) SUMA_RETURN( SUMA_OPENDX_MESH );
   if (SUMA_iswordin_ci(str, "ply")  == 1 ) SUMA_RETURN( SUMA_PLY );
   if (SUMA_iswordin_ci(str, "vec")  == 1 || SUMA_iswordin_ci(str, "1d") == 1   ) SUMA_RETURN( SUMA_VEC );
   if (SUMA_iswordin_ci(str, "BrainVoyager") == 1  || SUMA_iswordin_ci(str, "bv") == 1 ) SUMA_RETURN( SUMA_BRAIN_VOYAGER );
   if (SUMA_iswordin_ci(str, "cmap")  == 1 ) SUMA_RETURN( SUMA_CMAP_SO );

   SUMA_RETURN(tp);
}

/*!
   \brief Reads in a sequence of numbers of an undetermined length
   Not for reading in large numbers of numbers!
   \param op (char *) pointing to the beginning of a 
                     blank delimited series of numbers
   \param opend (char **) if not NULL, *opend will contain the value
                           of op at the end of successful reads
   \param tp (SUMA_VARTYPE) SUMA_int, SUMA_float, SUMA_double supported 
                           at the moment
   \return ans (void*) if  tp == SUMA_int then ans is (SUMA_IVEC *)
                           tp == SUMA_float then ans is (SUMA_FVEC *)
                           tp == SUMA_double then ans is (SUMA_DVEC *)
   \sa SUMA_strtol_vec  
   \sa SUMA_SringToNum
*/
void *SUMA_AdvancePastNumbers(char *op, char **opend, SUMA_VARTYPE tp)
{
   static char FuncName[]={"SUMA_AdvancePastNumbers"};
   double *d=NULL, db;
   int nrealloc = 0, Chunk = 100, nalloc = 0;
   int Found = 0, i, nread;
   void *ans;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   nread = 0;
   Found = 1;
   while (Found) {
      SUMA_ADVANCE_PAST_NUM(op, db, Found);
      if (Found) {
         if (nread == nalloc) {
            nalloc += Chunk; ++nrealloc;
            d = (double*)SUMA_realloc(d, nalloc*sizeof(double));
            if (!d) { SUMA_SL_Crit("Failed to allocate"); if (d) SUMA_free(d); d = NULL; SUMA_RETURN(NULL); }
            if (!(nrealloc % 10)) { SUMA_SL_Warn("Too much reallocation, improper use of function?"); }
         }
         d[nread] = db;
         ++(nread);
      }
   } 
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR,"%s: Found %d numbers:\n", FuncName, nread);
      for (i=0; i<nread; ++i) fprintf(SUMA_STDERR,"%f\t", d[i]);
      fprintf(SUMA_STDERR,"\n");
   }
   
   if (opend) *opend = op;
   
   ans = NULL;
   switch (tp) {
      case SUMA_int:
         {
            SUMA_IVEC *ivec= (SUMA_IVEC *)SUMA_malloc(sizeof(SUMA_IVEC));
            ivec->v = (int *)SUMA_calloc(nread,sizeof(int));
            ivec->n = nread;
            for (i=0; i<nread; ++i) ivec->v[i] = (int)d[i];
            ans = (void *)ivec;
         }
         break;
      case SUMA_float:
         {
            SUMA_FVEC *fvec= (SUMA_FVEC *)SUMA_malloc(sizeof(SUMA_FVEC));
            fvec->v = (float *)SUMA_calloc(nread,sizeof(float));
            fvec->n = nread;
            for (i=0; i<nread; ++i) fvec->v[i] = (float)d[i];
            ans = (void *)fvec;
         }
         break;
      case SUMA_double:
         {
            SUMA_DVEC *dvec= (SUMA_DVEC *)SUMA_malloc(sizeof(SUMA_DVEC));
            dvec->v = (double *)SUMA_calloc(nread,sizeof(double));
            dvec->n = nread;
            for (i=0; i<nread; ++i) dvec->v[i] = (double)d[i];
            ans = (void *)dvec;
         }
         break;
      case SUMA_notypeset:
         SUMA_SL_Err("Type not set");
         ans = NULL;
         break;   
      default:
         SUMA_SL_Err("Type not supported by this function");
         ans = NULL;
         break;   
         
   }
   if (d) SUMA_free(d); d = NULL;
   
   SUMA_RETURN(ans);
   
}   

/*!
   \brief change a character string of numbers to a vector of values.
   op must be NULL terminated!
   
   \sa SUMA_AdvancePastNumbers
   \sa SUMA_StringToNum
*/
void *SUMA_strtol_vec(char *op, int nvals, int *nread, SUMA_VARTYPE vtp)
{
   static char FuncName[]={"SUMA_strtol_vec"};
   void *ans = NULL;
   long lv;
   double dv;
   char *endptr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   *nread = 0;
   if (!SUMA_OK_OPENDX_DATA_TYPE(vtp)) {
      SUMA_SL_Err("Bad type");
      SUMA_RETURN(ans);
   }
   
   ans = NULL;
   switch (vtp) {
      case SUMA_byte:
         {
            byte *bvec=NULL;
            bvec = (byte *)SUMA_calloc(nvals,sizeof(byte));
            lv = strtol(op, &endptr, 10);
            while (endptr && endptr != op && *nread < nvals) {
               bvec[*nread] = (byte)lv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%d<<<\t", bvec[*nread]);  */
               ++(*nread);
               op = endptr;
               lv = strtol(op, &endptr, 10);
            }
            ans = (void *)bvec;
         }
         break;
      case SUMA_int:
         {
            int *ivec=NULL;
            ivec = (int *)SUMA_calloc(nvals,sizeof(int));
            lv = strtol(op, &endptr, 10);
            while (endptr && endptr != op && *nread < nvals) {
               ivec[*nread] = (int)lv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%d<<<\t", ivec[*nread]);  */
               ++(*nread);
               op = endptr;
               lv = strtol(op, &endptr, 10);
            }
            ans = (void *)ivec;
         }
         break;
      case SUMA_float:
         {
            float *fvec=NULL;
            fvec = (float *)SUMA_calloc(nvals,sizeof(float));
            dv = strtod(op, &endptr);
            while (endptr && endptr != op && *nread < nvals) {
               fvec[*nread] = (float)dv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%f<<<\t", fvec[*nread]); */
               ++(*nread);
               op = endptr;
               dv = strtod(op, &endptr);
            }
            ans = (void *)fvec;
         }
         break;
      case SUMA_double:
         {
            double *dvec=NULL;
            dvec = (double *)SUMA_calloc(nvals,sizeof(double));
            dv = strtod(op, &endptr);
            while (endptr && endptr != op && *nread < nvals) {
               dvec[*nread] = (double)dv;
               /* if (LocalHead) fprintf(SUMA_STDERR,">>>%f<<<\t", dvec[*nread]); */
               ++(*nread);
               op = endptr;
               dv = strtod(op, &endptr);
            }
            ans = (void *)dvec;
         }
         break;
      case SUMA_notypeset:
         SUMA_SL_Err("Type not set");
         ans = NULL;
         break;   
      default:
         SUMA_SL_Err("Type not supported by this function");
         ans = NULL;
         break;   
         
   }
   
   SUMA_RETURN(ans);
}

/*!
   \brief a function to allocate and initialize the option structure 
   that is to be used by most programs.
   
   This is probably the least elegant function. Do not change initialization values 
   because they might affect different programs in various ways. 
   Some options are very specific to an application, others are common to many

   \sa SUMA_Free_Generic_Prog_Options_Struct
   \sa SUMA_GENERIC_PROG_OPTIONS_STRUCT
*/
SUMA_GENERIC_PROG_OPTIONS_STRUCT * SUMA_Alloc_Generic_Prog_Options_Struct(void)
{
   static char FuncName[]={"SUMA_Alloc_Generic_Prog_Options_Struct"};
   int i;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt = NULL;
   
   Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)SUMA_malloc(sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));
   Opt->SpatNormDxyz = 0.0;
   Opt->spec_file = NULL;
   Opt->out_vol_prefix = NULL;
   Opt->out_vol_view[0]='\0';
   Opt->out_vol_exists = -1;
   Opt->out_grid_prefix = NULL;
   Opt->out_grid_view[0]='\0';
   Opt->out_grid_exists = -1;
   Opt->in_vol_prefix = NULL;
   Opt->in_vol_view[0]='\0';
   Opt->in_vol_exists = -1;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = SUMA_ISO_UNDEFINED;
   for (i=0; i<SUMA_GENERIC_PROG_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcdatav = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = 0.0;
   Opt->dvec = NULL;
   Opt->SurfFileType = SUMA_PLY;
   Opt->SurfFileFormat = SUMA_ASCII;
   Opt->xform = SUMA_ISO_XFORM_UNDEFINED;
   Opt->obj_type = -1;
   Opt->obj_type_res = -1;
   Opt->XYZ = NULL;
   Opt->in_1D = NULL;
   Opt->N_XYZ = 0;
   Opt->Zt = 0.0;
   Opt->ExpFrac = 0.0;
   Opt->N_it = 0;
   Opt->Icold = 0;
   Opt->NodeDbg = -1;
   Opt->t2 = Opt->t98 = Opt->t = Opt->tm = -1;
   Opt->r = 0;
   Opt->d1 = 0;
   Opt->su1 = 0;
   Opt->UseNew = 0.0;
   Opt->d4 = 0;
   Opt->ztv = NULL;
   Opt->Kill98 = 0;
   Opt->NoEyes = 0;
   Opt->NNsmooth = 0;
   Opt->smootheach = 0;
   Opt->avoid_vent = 0;
   Opt->smooth_end = 0;
   Opt->fix_winding = 0;
   Opt->k98mask = NULL;
   Opt->k98maskcnt = 0;
   Opt->dbg_eyenodes = NULL;
   Opt->travstp = 0.0;
   Opt->Stop = NULL;
   Opt->MaxIntIter = 0;
   Opt->UseExpansion = 0;
   Opt->PercInt = 0;
   Opt->UseSkull = 0;
   Opt->send_hull = 0;
   Opt->bot_lztclip = 0.0; /* 0.5 is OK but causes too much leakage below cerebellum in most dsets, 0.65 seems better. 0 if you do not want to use it*/
	Opt->var_lzt = 0.0; /* a flag at the moment, set it to 1 to cause shirnk fac to vary during iterations. Helps escape certain large 
                           chunks of CSF just below the brain */
   Opt->DemoPause = 0;
   Opt->DoSpatNorm = 0;
   Opt->WriteSpatNorm = 0;
   Opt->fillhole = -1;
   Opt->iset = NULL;
   Opt->SpatShift[0] = Opt->SpatShift[1] = Opt->SpatShift[2] = 0.0;
   Opt->OrigSpatNormedSet = NULL;
   Opt->in_edvol = NULL;
   Opt->blur_fwhm = 0.0;
   Opt->iset_hand = 0;
   
   Opt->NearestNode = 0;
   Opt->NearestTriangle = 0;
   Opt->DistanceToMesh = 0;
   Opt->ProjectionOnMesh = 0;
   Opt->Data = 0;
   
   Opt->in_nodeindices = NULL;
   
   SUMA_RETURN(Opt);
}   
SUMA_GENERIC_PROG_OPTIONS_STRUCT * SUMA_Free_Generic_Prog_Options_Struct(SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt)
{
   static char FuncName[]={"SUMA_Free_Generic_Prog_Options_Struct"};
   
   SUMA_ENTRY;
   
   if (!Opt) SUMA_RETURN(NULL);
   
   if (Opt->OrigSpatNormedSet && Opt->OrigSpatNormedSet != Opt->in_vol) { DSET_delete(Opt->OrigSpatNormedSet); Opt->OrigSpatNormedSet = NULL; }
   else Opt->OrigSpatNormedSet = NULL;

   if (Opt->dbg_eyenodes) fclose(Opt->dbg_eyenodes); Opt->dbg_eyenodes = NULL;
   if (Opt->k98mask) SUMA_free(Opt->k98mask); Opt->k98mask = NULL;
   if (Opt->Stop) SUMA_free(Opt->Stop); Opt->Stop = NULL;
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcdatav) {SUMA_free(Opt->mcdatav); Opt->mcdatav = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt->out_vol_prefix) SUMA_free(Opt->out_vol_prefix); Opt->out_vol_prefix = NULL;
   if (Opt->in_vol_prefix) SUMA_free(Opt->in_vol_prefix); Opt->in_vol_prefix = NULL;
   if (Opt->out_grid_prefix) SUMA_free(Opt->out_grid_prefix); Opt->out_grid_prefix = NULL;
   if (Opt->XYZ) SUMA_free(Opt->XYZ); Opt->XYZ = NULL;
   if (Opt->ztv) SUMA_free(Opt->ztv); Opt->ztv = NULL;
   if (Opt) SUMA_free(Opt);

   SUMA_RETURN(NULL);
   
}
SUMA_GENERIC_ARGV_PARSE *SUMA_CreateGenericArgParse(char *optflags)
{
   static char FuncName[]={"SUMA_CreateGenericArgParse"};
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   int i;
   SUMA_ENTRY;
   
   ps = (SUMA_GENERIC_ARGV_PARSE*)SUMA_malloc(sizeof(SUMA_GENERIC_ARGV_PARSE));
   
   ps->cs = NULL;
   
   for (i=0;i<SUMA_MAX_SURF_ON_COMMAND; ++i) {
      ps->t_surfnames[i] =  ps->t_surftopo[i] = ps->t_surfpath[i] = ps->t_surfprefix[i] = ps->t_state[i] = NULL;
      ps->t_N_surfnames = 0; ps->t_FF[i] = SUMA_FF_NOT_SPECIFIED; ps->t_FT[i] = SUMA_FT_NOT_SPECIFIED; ps->t_anatomical[i] = NOPE;
      ps->i_surfnames[i] =  ps->i_surftopo[i] = ps->i_surfpath[i] = ps->i_surfprefix[i] = ps->i_state[i] = ps->i_group[i] = NULL;
      ps->i_N_surfnames = 0; ps->i_FF[i] = SUMA_FF_NOT_SPECIFIED; ps->i_FT[i] = SUMA_FT_NOT_SPECIFIED;  ps->i_anatomical[i] = NOPE;
      ps->ipar_surfnames[i] =  ps->ipar_surftopo[i] = ps->ipar_surfpath[i] = ps->ipar_surfprefix[i] = ps->ipar_state[i] = ps->ipar_group[i] = NULL; 
      ps->ipar_N_surfnames = 0; ps->ipar_FF[i] = SUMA_FF_NOT_SPECIFIED; ps->ipar_FT[i] = SUMA_FT_NOT_SPECIFIED;  ps->ipar_anatomical[i] = NOPE;
      ps->o_surfnames[i] =  ps->o_surftopo[i] = ps->o_surfpath[i] = ps->o_surfprefix[i] = ps->o_state[i] = ps->o_group[i] = NULL;
      ps->o_N_surfnames = 0; ps->o_FF[i] = SUMA_FF_NOT_SPECIFIED; ps->o_FT[i] = SUMA_FT_NOT_SPECIFIED; ps->o_anatomical[i] = NOPE;
      ps->s_surfnames[i] = ps->s_surfprefix[i] = ps->s_surfpath[i] = NULL;  
      ps->s_N_surfnames = 0;
      ps->spec_names[i] = NULL; ps->N_spec_names = 0;
      ps->sv[i] = NULL; ps->N_sv = 0;
      ps->vp[i] = NULL; ps->N_vp = 0;
   }
   
   for (i=0; i< SUMA_N_ARGS_MAX; ++i) {
      ps->arg_checked[i] = 0;
   }
   ps->N_args = 0;
   
   if (SUMA_iswordin(optflags,"-t;")) ps->accept_t = 1; else ps->accept_t = 0;
   if (SUMA_iswordin(optflags,"-i;")) ps->accept_i = 1; else ps->accept_i = 0;
   if (SUMA_iswordin(optflags,"-ipar;")) ps->accept_ipar = 1; else ps->accept_ipar = 0;
   if (SUMA_iswordin(optflags,"-s;")) ps->accept_s = 1; else ps->accept_s = 0;
   if (SUMA_iswordin(optflags,"-o;")) ps->accept_o = 1; else ps->accept_o = 0;
   if (SUMA_iswordin(optflags,"-spec;")) ps->accept_spec = 1; else ps->accept_spec = 0;
   if (SUMA_iswordin(optflags,"-sv;")) ps->accept_sv = 1; else ps->accept_sv = 0;
   if (SUMA_iswordin(optflags,"-talk;")) ps->accept_talk_suma = 1; else ps->accept_talk_suma = 0;
   ps->check_input_surf = 1;
   
   SUMA_RETURN(ps);
}

SUMA_GENERIC_ARGV_PARSE *SUMA_FreeGenericArgParse(SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_FreeGenericArgParse"};
   int i;
   SUMA_ENTRY;
   
   if (ps) {
      for (i=0; i<SUMA_MAX_SURF_ON_COMMAND; ++i) {
         if (ps->t_surfnames[i]) SUMA_free(ps->t_surfnames[i]); ps->t_surfnames[i]= NULL;
         if (ps->t_surftopo[i]) SUMA_free(ps->t_surftopo[i]); ps->t_surftopo[i]= NULL;
         if (ps->t_surfpath[i]) SUMA_free(ps->t_surfpath[i]); ps->t_surfpath[i]= NULL;
         if (ps->t_surfprefix[i]) SUMA_free(ps->t_surfprefix[i]);ps->t_surfprefix[i] = NULL;
         if (ps->t_state[i]) SUMA_free(ps->t_state[i]);ps->t_state[i] = NULL;
         if (ps->i_surfnames[i]) SUMA_free(ps->i_surfnames[i]); ps->i_surfnames[i]= NULL;
         if (ps->i_surftopo[i]) SUMA_free(ps->i_surftopo[i]); ps->i_surftopo[i]= NULL;
         if (ps->i_surfpath[i]) SUMA_free(ps->i_surfpath[i]); ps->i_surfpath[i]= NULL;
         if (ps->i_surfprefix[i]) SUMA_free(ps->i_surfprefix[i]);ps->i_surfprefix[i] = NULL;
         if (ps->i_state[i]) SUMA_free(ps->i_state[i]);ps->i_state[i] = NULL;
         if (ps->ipar_surfnames[i]) SUMA_free(ps->ipar_surfnames[i]); ps->ipar_surfnames[i]= NULL;
         if (ps->ipar_surftopo[i]) SUMA_free(ps->ipar_surftopo[i]); ps->ipar_surftopo[i]= NULL;
         if (ps->ipar_surfpath[i]) SUMA_free(ps->ipar_surfpath[i]); ps->ipar_surfpath[i]= NULL;
         if (ps->ipar_surfprefix[i]) SUMA_free(ps->ipar_surfprefix[i]);ps->ipar_surfprefix[i] = NULL;
         if (ps->ipar_state[i]) SUMA_free(ps->ipar_state[i]);ps->ipar_state[i] = NULL;
         if (ps->o_surfnames[i]) SUMA_free(ps->o_surfnames[i]); ps->o_surfnames[i]= NULL;
         if (ps->o_surftopo[i]) SUMA_free(ps->o_surftopo[i]); ps->o_surftopo[i]= NULL;
         if (ps->o_surfpath[i]) SUMA_free(ps->o_surfpath[i]); ps->o_surfpath[i]= NULL;
         if (ps->o_surfprefix[i]) SUMA_free(ps->o_surfprefix[i]);ps->o_surfprefix[i] = NULL;
         if (ps->o_state[i]) SUMA_free(ps->o_state[i]);ps->o_state[i] = NULL;
         if (ps->s_surfnames[i]) SUMA_free(ps->s_surfnames[i]); ps->s_surfnames[i]= NULL;
         if (ps->s_surfprefix[i]) SUMA_free(ps->s_surfprefix[i]);ps->s_surfprefix[i] = NULL;
         if (ps->s_surfpath[i]) SUMA_free(ps->s_surfpath[i]); ps->s_surfpath[i]= NULL;
         if (ps->spec_names[i]) SUMA_free(ps->spec_names[i]); ps->spec_names[i] = NULL;
         if (ps->sv[i]) SUMA_free(ps->sv[i]); ps->sv[i] = NULL;
         if (ps->vp[i]) SUMA_free(ps->vp[i]); ps->vp[i] = NULL;
      }
      if (ps->cs) SUMA_Free_CommSrtuct(ps->cs); ps->cs = NULL;
      SUMA_free(ps); ps = NULL;  
   } 
   SUMA_RETURN(NULL);
}  

char *SUMA_help_IO_Args(SUMA_GENERIC_ARGV_PARSE *opt)
{
   static char FuncName[]={"SUMA_help_IO_Args"};
   char *s=NULL, *st = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (opt->accept_i) {
      SS = SUMA_StringAppend (SS, 
                  " Specifying input surfaces using -i_TYPE options: \n"
                  "    -i_TYPE inSurf specifies the input surface,\n"
                  "            TYPE is one of the following:\n"
                  "       fs: FreeSurfer surface. \n"
                  "           If surface name has .asc it is assumed to be\n"
                  "           in ASCII format. Otherwise it is assumed to be\n"
                  "           in BINARY_BE (Big Endian) format.\n"
                  "           Patches in Binary format cannot be read at the moment.\n"
                  "       sf: SureFit surface. \n"
                  "           You must specify the .coord followed by the .topo file.\n"
                  "       vec (or 1D): Simple ascii matrix format. \n"
                  "            You must specify the coord (NodeList) file followed by \n"
                  "            the topo (FaceSetList) file.\n"
                  "            coord contains 3 floats per line, representing \n"
                  "            X Y Z vertex coordinates.\n"
                  "            topo contains 3 ints per line, representing \n"
                  "            v1 v2 v3 triangle vertices.\n"
                  "       ply: PLY format, ascii or binary.\n"
                  "            Only vertex and triangulation info is preserved.\n"
                  "       bv: BrainVoyager format. \n"
                  "           Only vertex and triangulation info is preserved.\n"
                  "       dx: OpenDX ascii mesh format.\n"
                  "           Only vertex and triangulation info is preserved.\n"
                  "           Requires presence of 3 objects, the one of class \n"
                  "           'field' should contain 2 components 'positions'\n"
                  "           and 'connections' that point to the two objects\n"
                  "           containing node coordinates and topology, respectively.\n"
      );
   }
   if (opt->accept_ipar) {
      SS = SUMA_StringAppend (SS, 
                  "    -ipar_TYPE ParentSurf specifies the parent surface. Only used\n"
                  "            when -o_fsp is used, see -o_TYPE options.\n"
      );
   }
   
   if (opt->accept_t) {
      SS = SUMA_StringAppend (SS, 
                  " Specifying surfaces using -t* options: \n"
                  "   -tn TYPE NAME: specify surface type and name.\n"
                  "                  See below for help on the parameters.\n"
                  "   -tsn TYPE STATE NAME: specify surface type state and name.\n"
	               "        TYPE: Choose from the following (case sensitive):\n"
                  "           1D: 1D format\n"
                  "           FS: FreeSurfer ascii format\n"
                  "           PLY: ply format\n"
                  "           SF: Caret/SureFit format\n"
                  "           BV: BrainVoyager format\n"
                  "        NAME: Name of surface file. \n"
                  "           For SF and 1D formats, NAME is composed of two names\n"
                  "           the coord file followed by the topo file\n"
                  "        STATE: State of the surface.\n"
                  "           Default is S1, S2.... for each surface.\n"
      );
   }
   

   if (opt->accept_sv) {
      SS = SUMA_StringAppend (SS, 
                  " Specifying a Surface Volume:\n"
                  "    -sv SurfaceVolume [VolParam for sf surfaces]\n"
                  "       If you supply a surface volume, the coordinates of the input surface.\n"
                  "        are modified to SUMA's convention and aligned with SurfaceVolume.\n"
                  "        You must also specify a VolParam file for SureFit surfaces.\n"
      );
   }
   
   if (opt->accept_spec) {
      SS = SUMA_StringAppend (SS, 
                  " Specifying a surface specification (spec) file:\n"
                  "    -spec SPEC: specify the name of the SPEC file.\n"
      );
   }
   
   if (opt->accept_s) {
      SS = SUMA_StringAppend (SS, 
                  " Specifying a surface using -surf_? method:\n"
                  "    -surf_A SURFACE: specify the name of the first\n"
                  "            surface to load. If the program requires\n"
                  "            or allows multiple surfaces, use -surf_B\n"
                  "            ... -surf_Z .\n"
                  "            You need not use _A if only one surface is\n"
                  "            expected.\n"
                  "            SURFACE is the name of the surface as specified\n"
                  "            in the SPEC file. The use of -surf_ option \n"
                  "            requires the use of -spec option.\n"

      );
   }

   if (opt->accept_o) {
      SS = SUMA_StringAppend (SS, 
                  " Specifying output surfaces using -o_TYPE options: \n"
                  "    -o_TYPE outSurf specifies the output surface, \n"
                  "            TYPE is one of the following:\n"
                  "       fs: FreeSurfer ascii surface. \n"
                  "       fsp: FeeSurfer ascii patch surface. \n"
                  "            In addition to outSurf, you need to specify\n"
                  "            the name of the parent surface for the patch.\n"
                  "            using the -ipar_TYPE option.\n"
                  "            This option is only for ConvertSurface \n"
                  "       sf: SureFit surface. \n"
                  "           For most programs, you are expected to specify prefix:\n"
                  "           i.e. -o_sf brain. In some programs, you are allowed to \n"
                  "           specify both .coord and .topo file names: \n"
                  "           i.e. -o_sf XYZ.coord TRI.topo\n"
                  "           The program will determine your choice by examining \n"
                  "           the first character of the second parameter following\n"
                  "           -o_sf. If that character is a '-' then you have supplied\n"
                  "           a prefix and the program will generate the coord and topo names.\n"
                  "       vec (or 1D): Simple ascii matrix format. \n"
                  "            For most programs, you are expected to specify prefix:\n"
                  "            i.e. -o_1D brain. In some programs, you are allowed to \n"
                  "            specify both coord and topo file names: \n"
                  "            i.e. -o_1D brain.1D.coord brain.1D.topo\n"
                  "            coord contains 3 floats per line, representing \n"
                  "            X Y Z vertex coordinates.\n"
                  "            topo contains 3 ints per line, representing \n"
                  "            v1 v2 v3 triangle vertices.\n"
                  "       ply: PLY format, ascii or binary.\n"
      );
   }
   
   if (opt->accept_talk_suma) {
      st = SUMA_help_talk();
      SS = SUMA_StringAppend_va (SS,
                  "\n"
                  "%s"
                  "\n", 
                  st
      );
      SUMA_free(st); st = NULL;
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
   
}
/*!
   A function to parse command line arguments and return a convenient
   structure that can be used by various programs for surface specifications

   I hope and pray that this will not be a can of worms. 
*/
SUMA_GENERIC_ARGV_PARSE *SUMA_Parse_IO_Args (int argc, char *argv[], char *optflags)
{
   static char FuncName[]={"SUMA_Parse_IO_Args"};
   int i, kar, ind, N_name;
   SUMA_Boolean brk = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   
   SUMA_ENTRY;
   
   if (argc >= SUMA_N_ARGS_MAX) {
      SUMA_S_Err("Number of arguments exceeds SUMA's parsing limit.");
      SUMA_RETURN(NULL);
   }
   
   ps = SUMA_CreateGenericArgParse(optflags);
   if (!ps) {
      SUMA_S_Err("Failed to create parse structure.");
      SUMA_RETURN(NULL);
   }
   ps->N_args = argc;
   ps->cs = SUMA_Create_CommSrtuct();
   kar = 1;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
      if (ps->accept_talk_suma) {
         if (!brk && (strcmp(argv[kar], "-talk_suma") == 0)) {
            ps->arg_checked[kar]=1;
            ps->cs->talk_suma = 1;
            brk = YUP;
         }
         
         if (!brk && (strcmp(argv[kar], "-feed_afni") == 0)) {
            ps->arg_checked[kar]=1;
            ps->cs->Feed2Afni = 1;
            brk = YUP;
         }
         
         if (!brk && strcmp(argv[kar], "-send_kth") == 0)
		   {
            ps->arg_checked[kar]=1;
			   kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err("need argument after -send_kth \n");
				   exit (1);
			   }
			   ps->cs->kth = atoi(argv[kar]);
            if (ps->cs->kth <= 0) {
               fprintf (SUMA_STDERR, "Bad value (%d) for send_kth\n", ps->cs->kth);
				   exit (1);
            }

			   brk = YUP;
		   }
         
         if (!brk && strcmp(argv[kar], "-ni_text") == 0)
		   {
            ps->arg_checked[kar]=1;
            ps->cs->comm_NI_mode = NI_TEXT_MODE;
            brk = YUP;
         }

         if (!brk && strcmp(argv[kar], "-ni_binary") == 0)
		   {
            ps->arg_checked[kar]=1;
            ps->cs->comm_NI_mode = NI_BINARY_MODE;
            brk = YUP;
         }

		   if (!brk && strcmp(argv[kar], "-sh") == 0)
		   {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err("need argument after -sh \n");
				   exit (1);
			   }
			   if (strcmp(argv[kar],"localhost") != 0) {
               ps->cs->suma_host_name = SUMA_copy_string(argv[kar]);
            }else {
              fprintf (SUMA_STDERR, "localhost is the default for -sh\nNo need to specify it.\n");
            }

			   brk = YUP;
		   }
         	
         if (!brk && strcmp(argv[kar], "-ah") == 0)
		   {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err("need argument after -ah \n");
				   exit (1);
			   }
			   if (strcmp(argv[kar],"localhost") != 0) {
               ps->cs->afni_host_name = SUMA_copy_string(argv[kar]);
            }else {
              fprintf (SUMA_STDERR, "localhost is the default for -ah\nNo need to specify it.\n");
            }

			   brk = YUP;
		   }
         
         if (!brk && strcmp(argv[kar], "-refresh_rate") == 0)
		   {
            ps->arg_checked[kar]=1;
			   kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err("need argument after -refresh_rate \n");
				   exit (1);
			   }
			   ps->cs->rps = atof(argv[kar]);
            if (ps->cs->rps <= 0) {
               fprintf (SUMA_STDERR, "Bad value (%f) for refresh_rate\n", ps->cs->rps);
				   exit (1);
            }

			   brk = YUP;
		   }
           
      }
      
      if (ps->accept_sv) {
         if (!brk && (strcmp(argv[kar], "-sv") == 0)) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err("need 1 or 2 arguments after -sv ");
				   exit (1);
			   }
            if (ps->N_sv >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed sv files...");
               exit(1);   
            }
			   ps->sv[ps->N_sv] = SUMA_copy_string(argv[kar]);
            /* is there a volparam option ?*/
            if (kar+1 < argc) {
               /* maybe a volparam file */
               if (argv[kar+1][0] == '-') { /* NOPE*/
               } else {
                  kar ++; ps->arg_checked[kar]=1;
                  ps->vp[ps->N_vp] = SUMA_copy_string(argv[kar]);
                  ++ps->N_vp;
               }
            }
			   ++ps->N_sv;
            brk = YUP;
		   }   
      }
      if (ps->accept_spec || ps->accept_s) {
         if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "need argument after -spec ");
				   exit (1);
			   }
            if (ps->N_spec_names >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed spec files...");
               exit(1);   
            }
			   ps->spec_names[ps->N_spec_names] = SUMA_copy_string(argv[kar]);
			   ++ps->N_spec_names;
            brk = YUP;
		   }   
      }
      if (ps->accept_s) {
         if (!brk && (strncmp(argv[kar], "-surf_", 6) == 0)) {
            ps->arg_checked[kar]=1;
		      if (kar + 1>= argc)  {
		  	      SUMA_S_Err( "need argument after -surf_X SURF_NAME \n");
			      exit (1);
		      }
		      ind = argv[kar][6] - 'A';
            if (ind < 0 || ind >= 'Z') {
               fprintf (SUMA_STDERR,   "Error %s:\n -surf_X SURF_NAME option is out of range.\n"
                                       "Only %d surfaces are allowed. \n"
                                       "Must start with surf_A for first surface.\n", FuncName, 'Z');
			      exit (1);
            }
            kar ++;
            ps->arg_checked[kar]=1;
            if (ps->s_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->s_surfnames[ps->s_N_surfnames+ind] = SUMA_copy_string(argv[kar]);
            ++ps->s_N_surfnames;
            brk = YUP;
	      } 
         if (!brk && (strncmp(argv[kar], "-surf", 5) == 0)) {
            ps->arg_checked[kar]=1;
		      if (kar + 1>= argc)  {
		  	      SUMA_S_Err( "need argument after -surf SURF_NAME \n");
			      exit (1);
		      }
		      ind = 0;
            
            kar ++;
            ps->arg_checked[kar]=1;
            if (ps->s_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            if (ps->s_surfnames[ps->s_N_surfnames+ind]) {
               SUMA_SL_Err("surface (A) already specified.");
               exit(1);   
            }
            ps->s_surfnames[ps->s_N_surfnames+ind] = SUMA_copy_string(argv[kar]);
            ++ps->s_N_surfnames;
            brk = YUP;
	      }  
      }
      if (ps->accept_i) {
         if (!brk && ( (strcmp(argv[kar], "-i_bv") == 0) || (strcmp(argv[kar], "-i_BV") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -i_bv ");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_BRAIN_VOYAGER;
            ps->i_FF[ps->i_N_surfnames] = SUMA_BINARY;
            ++ps->i_N_surfnames;
            brk = YUP;
         }

         if (!brk && ( (strcmp(argv[kar], "-i_fs") == 0) || (strcmp(argv[kar], "-i_FS") == 0)) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -i_fs ");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_FREE_SURFER;
            if (SUMA_isExtension(ps->i_surfnames[ps->i_N_surfnames], ".asc")) 
               ps->i_FF[ps->i_N_surfnames] = SUMA_ASCII;
            else
               ps->i_FF[ps->i_N_surfnames] = SUMA_BINARY_BE;
            ++ps->i_N_surfnames;
            brk = YUP;
         }
         
         if (!brk && ( (strcmp(argv[kar], "-i_sf") == 0) || (strcmp(argv[kar], "-i_SF") == 0)) ){
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar+1 >= argc)  {
	            SUMA_S_Err( "need 2 arguments after -i_sf");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            kar ++; ps->arg_checked[kar]=1;
            ps->i_surftopo[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_SUREFIT;
            ps->i_FF[ps->i_N_surfnames] = SUMA_ASCII;
            ++ps->i_N_surfnames;            
            brk = YUP;
         }

         if (!brk && ( (strcmp(argv[kar], "-i_vec") == 0) || (strcmp(argv[kar], "-i_1d") == 0) ||
                       (strcmp(argv[kar], "-i_VEC") == 0) || (strcmp(argv[kar], "-i_1D") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar+1 >= argc)  {
	            SUMA_S_Err( "need 2 argument after -i_vec or -i_1d");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            kar ++; ps->arg_checked[kar]=1;
            ps->i_surftopo[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_VEC;
            ps->i_FF[ps->i_N_surfnames] = SUMA_ASCII; 
            ++ps->i_N_surfnames;            
            brk = YUP;
         }

         if (!brk && ( (strcmp(argv[kar], "-i_ply") == 0) || (strcmp(argv[kar], "-i_Ply") == 0) || (strcmp(argv[kar], "-i_PLY") == 0))) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -i_ply ");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_PLY;
            ps->i_FF[ps->i_N_surfnames] = SUMA_FF_NOT_SPECIFIED;
            ++ps->i_N_surfnames;
            brk = YUP;
         }
         if (!brk && ( (strcmp(argv[kar], "-i_DX") == 0) || (strcmp(argv[kar], "-i_dx") == 0) || (strcmp(argv[kar], "-i_Dx") == 0))) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -i_dx ");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_OPENDX_MESH;
            ps->i_FF[ps->i_N_surfnames] = SUMA_ASCII;
            ++ps->i_N_surfnames;
            brk = YUP;
         }
         if (!brk && ( (strcmp(argv[kar], "-i_iv") == 0) || (strcmp(argv[kar], "-i_IV") == 0) )) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -i_iv ");
	            exit (1);
            }
            if (ps->i_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->i_surfnames[ps->i_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->i_FT[ps->i_N_surfnames] = SUMA_INVENTOR_GENERIC;
            ps->i_FF[ps->i_N_surfnames] = SUMA_ASCII;
            ++ps->i_N_surfnames;
            brk = YUP;
         }
                  
      }
      
      if (ps->accept_ipar) {
         if (!brk && ( (strcmp(argv[kar], "-ipar_bv") == 0) || (strcmp(argv[kar], "-ipar_BV") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -ipar_bv ");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_BRAIN_VOYAGER;
            ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_BINARY;
            ++ps->ipar_N_surfnames;
            brk = YUP;
         }

         if (!brk && ( (strcmp(argv[kar], "-ipar_fs") == 0) || (strcmp(argv[kar], "-ipar_FS") == 0)) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -ipar_fs ");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_FREE_SURFER;
            if (SUMA_isExtension(ps->ipar_surfnames[ps->ipar_N_surfnames], ".asc")) 
               ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_ASCII;
            else
               ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_BINARY_BE;
            ++ps->ipar_N_surfnames;
            brk = YUP;
         }
         
         if (!brk && ( (strcmp(argv[kar], "-ipar_sf") == 0) || (strcmp(argv[kar], "-ipar_SF") == 0)) ){
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar+1 >= argc)  {
	            SUMA_S_Err( "need 2 arguments after -ipar_sf");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            kar ++; ps->arg_checked[kar]=1;
            ps->ipar_surftopo[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_SUREFIT;
            ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_ASCII;
            ++ps->ipar_N_surfnames;            
            brk = YUP;
         }

         if (!brk && ( (strcmp(argv[kar], "-ipar_vec") == 0) || (strcmp(argv[kar], "-ipar_1d") == 0) ||
                       (strcmp(argv[kar], "-ipar_VEC") == 0) || (strcmp(argv[kar], "-ipar_1D") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar+1 >= argc)  {
	            SUMA_S_Err( "need 2 argument after -ipar_vec or -ipar_1d");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            kar ++; ps->arg_checked[kar]=1;
            ps->ipar_surftopo[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_VEC;
            ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_ASCII; 
            ++ps->ipar_N_surfnames;            
            brk = YUP;
         }

         if (!brk && ( (strcmp(argv[kar], "-ipar_ply") == 0) || (strcmp(argv[kar], "-ipar_Ply") == 0) || (strcmp(argv[kar], "-ipar_PLY") == 0))) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -ipar_ply ");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_PLY;
            ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_FF_NOT_SPECIFIED;
            ++ps->ipar_N_surfnames;
            brk = YUP;
         }
         if (!brk && ( (strcmp(argv[kar], "-ipar_DX") == 0) || (strcmp(argv[kar], "-ipar_dx") == 0) || (strcmp(argv[kar], "-ipar_Dx") == 0))) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -ipar_dx ");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_OPENDX_MESH;
            ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_ASCII;
            ++ps->ipar_N_surfnames;
            brk = YUP;
         }
         if (!brk && ( (strcmp(argv[kar], "-ipar_iv") == 0) || (strcmp(argv[kar], "-ipar_IV") == 0) )) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
	            SUMA_S_Err( "need argument after -ipar_iv ");
	            exit (1);
            }
            if (ps->ipar_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->ipar_surfnames[ps->ipar_N_surfnames] = SUMA_copy_string(argv[kar]);
            ps->ipar_FT[ps->ipar_N_surfnames] = SUMA_INVENTOR_GENERIC;
            ps->ipar_FF[ps->ipar_N_surfnames] = SUMA_ASCII;
            ++ps->ipar_N_surfnames;
            brk = YUP;
         }         
      }      
      
      if (ps->accept_t) {
         if (!brk && (strcmp(argv[kar], "-tn") == 0)) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "Type argument must follow -tn ");
				   exit (1);
			   }
            if (ps->t_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            /* get the type */
            ps->t_FT[ps->t_N_surfnames] = SUMA_SurfaceTypeCode(argv[kar]);
            if (ps->t_FT[ps->t_N_surfnames] == SUMA_FT_ERROR || ps->t_FT[ps->t_N_surfnames] == SUMA_FT_NOT_SPECIFIED) {
               fprintf (SUMA_STDERR, "%s is a bad file type.\n", argv[kar]);
               exit(1);
            }
            /* get the name */
            if (ps->t_FT[ps->t_N_surfnames] == SUMA_SUREFIT || ps->t_FT[ps->t_N_surfnames] == SUMA_VEC) N_name = 2;
            else N_name = 1;
            if (kar+N_name >= argc)  {
		  		   fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
				   exit (1);
			   }
            kar ++; ps->arg_checked[kar]=1;
            ps->t_surfnames[ps->t_N_surfnames] = SUMA_copy_string(argv[kar]);
            if (N_name == 2) {
               kar ++; ps->arg_checked[kar]=1;
               ps->t_surftopo[ps->t_N_surfnames] = SUMA_copy_string(argv[kar]);
            } 

            ++ps->t_N_surfnames; 
			   brk = YUP;
		   } 
         
         if (!brk && (strcmp(argv[kar], "-tsn") == 0)) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   fprintf (SUMA_STDERR, "Type argument must follow -tn \n");
				   exit (1);
			   }
            if (ps->t_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            /* get the type */
            ps->t_FT[ps->t_N_surfnames] = SUMA_SurfaceTypeCode(argv[kar]);
            if (ps->t_FT[ps->t_N_surfnames] == SUMA_FT_ERROR || ps->t_FT[ps->t_N_surfnames] == SUMA_FT_NOT_SPECIFIED) {
               fprintf (SUMA_STDERR, "%s is a bad file type.\n", argv[kar]);
               exit(1);
            }
            /* get the state */
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "STATE argument must follow TYPE with -tsn ");
				   exit (1);
			   }
            ps->t_state[ps->t_N_surfnames] = SUMA_copy_string(argv[kar]);

            /* get the name */
            if (ps->t_FT[ps->t_N_surfnames] == SUMA_SUREFIT || ps->t_FT[ps->t_N_surfnames] == SUMA_VEC) N_name = 2;
            else N_name = 1;
            if (kar+N_name >= argc)  {
		  		   fprintf (SUMA_STDERR, "Error %s:\nneed %d elements for NAME \n", FuncName, N_name);
				   exit (1);
			   }
            kar ++; ps->arg_checked[kar]=1;
            ps->t_surfnames[ps->t_N_surfnames] = SUMA_copy_string(argv[kar]);
            if (N_name == 2) {
               kar ++; ps->arg_checked[kar]=1;
               ps->t_surftopo[ps->t_N_surfnames] = SUMA_copy_string(argv[kar]);
            } 
            /* get the format */
            if (  ps->t_FT[ps->t_N_surfnames] == SUMA_SUREFIT || ps->t_FT[ps->t_N_surfnames] == SUMA_VEC ||
                  ps->t_FT[ps->t_N_surfnames] == SUMA_INVENTOR_GENERIC || ps->t_FT[ps->t_N_surfnames] == SUMA_OPENDX_MESH) 
                  ps->t_FF[ps->t_N_surfnames] = SUMA_ASCII;
            else if (ps->t_FT[ps->t_N_surfnames] == SUMA_PLY)  ps->t_FF[ps->t_N_surfnames] = SUMA_FF_NOT_SPECIFIED;
            else if (ps->t_FT[ps->t_N_surfnames] == SUMA_BRAIN_VOYAGER) ps->t_FF[ps->t_N_surfnames] = SUMA_BINARY;
            else if (ps->t_FT[ps->t_N_surfnames] == SUMA_FREE_SURFER) {
               if (SUMA_isExtension(ps->t_surfnames[ps->t_N_surfnames], ".asc")) 
               ps->t_FF[ps->t_N_surfnames] = SUMA_ASCII;
            else
               ps->t_FF[ps->t_N_surfnames] = SUMA_BINARY_BE;
            }
            
            ++ps->t_N_surfnames;  
			   brk = YUP;
		   }
          
      }
      if (ps->accept_o) {
         if (!brk && ( (strcmp(argv[kar], "-o_fs") == 0) || (strcmp(argv[kar], "-o_FS") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "need argument after -o_fs ");
				   exit (1);
			   }
			   if (ps->o_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->o_surfnames[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_FREE_SURFER);
            ps->o_FT[ps->o_N_surfnames] = SUMA_FREE_SURFER;
            ps->o_FF[ps->o_N_surfnames] = SUMA_ASCII;
            ++ps->o_N_surfnames;    
			   brk = YUP;
		   }
      
         if (!brk && ( (strcmp(argv[kar], "-o_fsp") == 0) || (strcmp(argv[kar], "-o_FSP") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "need argument after -o_fsp ");
				   exit (1);
			   }
			   if (ps->o_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
			   
			   ps->o_surfnames[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_FREE_SURFER_PATCH); 
            ps->o_FT[ps->o_N_surfnames] = SUMA_FREE_SURFER_PATCH;
            ps->o_FF[ps->o_N_surfnames] = SUMA_BINARY;
			   ++ps->o_N_surfnames;   
            brk = YUP;
		   }

         if (!brk && ( (strcmp(argv[kar], "-o_sf") == 0) || (strcmp(argv[kar], "-o_SF") == 0)) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "need 1 or 2 argument after -o_sf ");
				   exit (1);
			   }
            if (ps->o_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->o_surfnames[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_SUREFIT);
            ps->o_FT[ps->o_N_surfnames] = SUMA_SUREFIT;
            ps->o_FF[ps->o_N_surfnames] = SUMA_ASCII;
            /* is there another argument ?*/
            if (kar+1 < argc)  {
		  		   if (argv[kar+1][0] == '-') {
                  /* that is an option flag */ 
                  ps->o_surftopo[ps->o_N_surfnames] = SUMA_copy_string(ps->o_surfnames[ps->o_N_surfnames]);
               } else {
                  kar ++; ps->arg_checked[kar]=1;
                  ps->o_surftopo[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_SUREFIT);
               }
			   }
            ++ps->o_N_surfnames;   
			   brk = YUP;
		   }
      
      
         if (!brk && (  (strcmp(argv[kar], "-o_vec") == 0) || (strcmp(argv[kar], "-o_1d") == 0)  ||
                        (strcmp(argv[kar], "-o_VEC") == 0) || (strcmp(argv[kar], "-o_1D") == 0) ) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
			   if (kar >= argc)  {
		  		   SUMA_S_Err( "need 1 or 2 argument after -o_sf ");
				   exit (1);
			   }
            if (ps->o_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
            ps->o_surfnames[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_VEC);
            ps->o_FT[ps->o_N_surfnames] = SUMA_VEC;
            ps->o_FF[ps->o_N_surfnames] = SUMA_ASCII;
			   /* is there another argument ?*/
            if (kar+1 < argc)  {
		  		   if (argv[kar+1][0] == '-') {
                  /* that is an option flag */ 
                  ps->o_surftopo[ps->o_N_surfnames] = SUMA_copy_string(ps->o_surfnames[ps->o_N_surfnames]);
               } else {
                  kar ++; ps->arg_checked[kar]=1;
                  ps->o_surftopo[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_VEC);
               }
			   }
            ++ps->o_N_surfnames;   
			   brk = YUP;
		   }
      
         if (!brk && ( (strcmp(argv[kar], "-o_dx") == 0) || (strcmp(argv[kar], "-o_DX") == 0) || (strcmp(argv[kar], "-o_Dx") == 0)) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
		  		   SUMA_S_Err( "need argument after -o_dx \n");
				   exit (1);
			   }
			   if (ps->o_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
			   ps->o_surfnames[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_OPENDX_MESH); 
            ps->o_FT[ps->o_N_surfnames] = SUMA_OPENDX_MESH;
            ps->o_FF[ps->o_N_surfnames] = SUMA_ASCII;
			   ++ps->o_N_surfnames;  
			   brk = YUP;
		   }         
         if (!brk && ( (strcmp(argv[kar], "-o_ply") == 0) || (strcmp(argv[kar], "-o_PLY") == 0) || (strcmp(argv[kar], "-o_Ply") == 0)) ) {
            ps->arg_checked[kar]=1;
            kar ++; ps->arg_checked[kar]=1;
            if (kar >= argc)  {
		  		   SUMA_S_Err( "need argument after -o_ply \n");
				   exit (1);
			   }
			   if (ps->o_N_surfnames >= SUMA_MAX_SURF_ON_COMMAND) {
               SUMA_S_Err("Exceeding maximum number of allowed surfaces...");
               exit(1);   
            }
			   ps->o_surfnames[ps->o_N_surfnames] = SUMA_RemoveSurfNameExtension(argv[kar], SUMA_PLY); 
            ps->o_FT[ps->o_N_surfnames] = SUMA_PLY;
            ps->o_FF[ps->o_N_surfnames] = SUMA_ASCII;
			   ++ps->o_N_surfnames;  
			   brk = YUP;
		   }
      }

		brk = NOPE;
		kar ++;
   }   
   
   if (ps->cs->rps > 0) { ps->cs->nelps = (float)ps->cs->talk_suma * ps->cs->rps; }
   else { ps->cs->nelps = (float) ps->cs->talk_suma * -1.0; }

    
   SUMA_RETURN(ps);
}

 
#ifdef STAND_ALONE
void usage ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_GetNextCommand ..... \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSTAND_ALONE -Wall -o $1 $1.c -I/usr/X11R6/include -I./\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tTue Feb 5 10:39:02 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   char Scom[500], S[500], term, d;

   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_GetNextCommand-Main-");
   
   term = '~';
   d = '|';
   
   if (argc < 2)
       {
          usage ();
          exit (1);
       }
   
   /* copy argc into string */
   sprintf (S,"%s", argv[1]);
   fprintf(stderr,"INITIAL: %s, \n", S);

   if(!SUMA_RegisterCommand (S,  d, term, "Newly Registered Append", NOPE)) {
      fprintf (stderr, "%s Error: Failed to register new command\n", FuncName);
   }
   if(!SUMA_RegisterCommand (S,  d, term, "Newly Registered Prepend", YUP)) {
      fprintf (stderr, "%s Error: Failed to register new command\n", FuncName);
   }

   /*repeat until nothing left */
   fprintf(stderr,"%s\n", S);
   while (SUMA_GetNextCommand (S, d, term, Scom) != SE_Empty)  {
       fprintf (stdout, "Command->%s<-\n", Scom);   
       fprintf(stderr,"%s\n", S);
   }
return(0);
}/* Main */
#endif
