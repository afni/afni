#ifndef SUMA_ENGINE_INCLUDED
#define SUMA_ENGINE_INCLUDED

SUMA_Boolean SUMA_Engine (char *Command, SUMA_EngineData *EngineData, SUMA_SurfaceViewer *sv);
int SUMA_EngineFieldCode(char *Scom);
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI);
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location);
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI);
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer);
int SUMA_EngineSourceCode (char *Scom);
void SUMA_EngineSourceString (char *Scom, int ses_code);
SUMA_Boolean SUMA_process_NIML_data( void *nini , SUMA_SurfaceViewer *sv );


#endif
