#ifndef SUMA_PARSECOMMANDS_INCLUDED
#define SUMA_PARSECOMMANDS_INCLUDED

int  SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
SUMA_Boolean  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
int SUMA_CommandCode(char *Scom);
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer);
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI);
int SUMA_EngineFieldCode(char *Scom);
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location);
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI);
int SUMA_EngineSourceCode (char *Scom);
void SUMA_EngineSourceString (char *Scom, int ses_code);

#endif
