#ifndef SUMA_PROTOTYPE_INCLUDED
#define SUMA_PROTOTYPE_INCLUDED

/* functions that have yet to be prototyped in a particular spot */
void SUMA_mapStateChanged(Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
void SUMA_input(Widget w, XtPointer clientData, XtPointer callData);
void SUMA_help(void);
void SUMA_help_message(FILE *Out);
void SUMA_VolSurf_help (FILE *Out);
void SUMA_Version (FILE *Out);
void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
void SUMA_unSetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
SUMA_Boolean  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
int  SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
int SUMA_CommandCode(char *Scom);
SUMA_Boolean SUMA_SureFit_Read_Coord (char * f_name, SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_SureFit_Read_Topo (char * f_name, SUMA_SureFit_struct *SF);
void SUMA_Show_SureFit (SUMA_SureFit_struct *SF, FILE *Out);
SUMA_Boolean SUMA_Free_SureFit (SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS);
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS);
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out);














#endif



