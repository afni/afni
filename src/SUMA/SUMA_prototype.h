#ifndef SUMA_PROTOTYPE_INCLUDED
#define SUMA_PROTOTYPE_INCLUDED

/* functions that have yet to be prototyped in a particular spot */
void SUMA_mapStateChanged(Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
void SUMA_help(void);
void SUMA_help_message(FILE *Out);
char * SUMA_help_message_Info(void);
void SUMA_Help_open (void *p);
void SUMA_Help_destroyed (void *p);
void SUMA_VolSurf_help (FILE *Out);
void SUMA_Version (FILE *Out);
void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
void SUMA_unSetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);




#endif



