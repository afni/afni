#ifndef SUMA_PARSECOMMANDS_INCLUDED
#define SUMA_PARSECOMMANDS_INCLUDED

int  SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
SUMA_Boolean  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
int SUMA_CommandCode(char *Scom);
const char *SUMA_CommandString (SUMA_ENGINE_CODE code);
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer);
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI);
SUMA_ENGINE_FIELD_CODE SUMA_EngineFieldCode(char *Scom);
const char *SUMA_EngineFieldString (SUMA_ENGINE_FIELD_CODE i);
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location);
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI);
int SUMA_EngineSourceCode (char *Scom);
void SUMA_EngineSourceString (char *Scom, int ses_code);

DList *SUMA_CreateList (void);
SUMA_EngineData *SUMA_InitializeEngineListData (SUMA_ENGINE_CODE CommandCode);
DListElmt * SUMA_RegisterEngineListCommand (DList *list, SUMA_EngineData * EngineData,  
                                             SUMA_ENGINE_FIELD_CODE Fld, void *FldValp, 
                                             SUMA_ENGINE_SOURCE Src, void *Srcp, SUMA_Boolean PassByPointer, 
                                             SUMA_ENGINE_INSERT_LOCATION InsertAt, DListElmt *Element);
SUMA_Boolean SUMA_ReleaseEngineListElement (DList *list, DListElmt *element);
DList * SUMA_DestroyList (DList *list);
DList * SUMA_EmptyDestroyList (DList *list);
void SUMA_FreeEngineListData(void *MTI);
SUMA_ENGINE_CODE SUMA_GetListNextCommand (DList *list);
void SUMA_ShowList (DList *list, FILE *Out);
void SUMA_FreeMessageListData(void *Hv);
SUMA_Boolean SUMA_ReleaseMessageListElement (DList *list, DListElmt *element) ;
DList *SUMA_CreateMessageList (void);
SUMA_Boolean SUMA_RegisterMessage ( DList *list, char *Message, char *Source, SUMA_MESSAGE_TYPES Type, SUMA_MESSAGE_ACTION Action);
char *SUMA_BuildMessageLog (DList *ML);
void SUMA_FreeActionStackData(void *asdata);
DList *SUMA_CreateActionStack (void);
void SUMA_ReleaseActionStackData (void *asdata);
DList *SUMA_EmptyDestroyActionStack (DList *AS);



/*!
   \brief Macro that adds a command to the head of command list.
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, Command, Src, Srcp)

   \param list (DList *) pointer to list 
   \param Command (SUMA_ENGINE_CODE) command code
   \param Src (SUMA_ENGINE_SOURCE) source of command
   \param Srcp (void *) pointer to source pointer. (No need to type cast it yourself, macro will)

   - Expects the variable FuncName (char *) to be defined already (that's the case in all of SUMA's functions)
   - No Engine Data can be passed with this macro

*/
#define SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, Command, Src, Srcp) {\
   SUMA_EngineData *ED_macro; \
   ED_macro = SUMA_InitializeEngineListData (Command);   \
   if (!SUMA_RegisterEngineListCommand (  list, ED_macro, \
                                          SEF_Empty, NULL,  \
                                          Src, (void *)Srcp, NOPE,   \
                                          SEI_Head, NULL)) {   \
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);   \
   }  \
}

/*!
   \brief Macro that adds a command to the tail of command list.
   
   \sa SUMA_REGISTER_HEAD_COMMAND_NO_DATA
*/
#define SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, Command, Src, Srcp) {\
   SUMA_EngineData *ED_macro; \
   ED_macro = SUMA_InitializeEngineListData (Command);   \
   if (!SUMA_RegisterEngineListCommand (  list, ED_macro, \
                                          SEF_Empty, NULL,  \
                                          Src, (void *)Srcp, NOPE,   \
                                          SEI_Tail, NULL)) {   \
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);   \
   }  \
}

/*!
   \brief Macro that reports an error to the log 

*/
#define SUMA_L_Err(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Error, SMA_Log); \
}
/*!
   \brief Macro that reports an error to stderr 

*/
#define SUMA_S_Err(msg) {\
   fprintf (SUMA_STDERR, "Error %s: %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports an error to stderr and log 

*/
#define SUMA_SL_Err(msg) {\
   fprintf (SUMA_STDERR, "Error %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Error, SMA_Log); \
}
/*!
   \brief Macro that reports an error to stderr and log and popup

*/
#define SUMA_SLP_Err(msg) {\
   fprintf (SUMA_STDERR, "Error %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Error, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a notice to the log 

*/
#define SUMA_L_Note(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Notice, SMA_Log); \
}
/*!
   \brief Macro that reports a notice to stderr 

*/
#define SUMA_S_Note(msg) {\
   fprintf (SUMA_STDERR, "Notice %s: %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports a notice to stderr and log 

*/
#define SUMA_SL_Note(msg) {\
   fprintf (SUMA_STDERR, "Notice %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Notice, SMA_Log); \
}
/*!
   \brief Macro that reports a notice to stderr and log and popup

*/
#define SUMA_SLP_Note(msg) {\
   fprintf (SUMA_STDERR, "Notice %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Notice, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a warning to the log 

*/
#define SUMA_L_Warn(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Warning, SMA_Log); \
}
/*!
   \brief Macro that reports a warning to stderr 

*/
#define SUMA_S_Warn(msg) {\
   fprintf (SUMA_STDERR, "Warning %s: %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports a warning to stderr and log 

*/
#define SUMA_SL_Warn(msg) {\
   fprintf (SUMA_STDERR, "Warning %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Warning, SMA_Log); \
}
/*!
   \brief Macro that reports a warning to stderr and log and popup

*/
#define SUMA_SLP_Warn(msg) {\
   fprintf (SUMA_STDERR, "Warning %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Warning, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a critical error to the log 

*/
#define SUMA_L_Crit(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Critical, SMA_Log); \
}
/*!
   \brief Macro that reports a critical error to stderr 

*/
#define SUMA_S_Crit(msg) {\
   fprintf (SUMA_STDERR, "Critical %s: %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports a critical error to stderr and log 

*/
#define SUMA_SL_Crit(msg) {\
   fprintf (SUMA_STDERR, "Critical %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Critical, SMA_Log); \
}
/*!
   \brief Macro that reports a critical error to stderr and log and popup

*/
#define SUMA_SLP_Crit(msg) {\
   fprintf (SUMA_STDERR, "Critical %s: %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Critical, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a message to SUMA_STDERR if LocalHead is set to YUP
*/
#define SUMA_LH(msg) {\
   if (LocalHead) fprintf (SUMA_STDERR, "%s: %s\n", FuncName, msg);  \
}

#endif
