#ifndef SUMA_PROTOTYPE_INCLUDED
#define SUMA_PROTOTYPE_INCLUDED

/* functions that have yet to be prototyped in a particular spot */
void SUMA_mapStateChanged(Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
char * SUMA_gsf(char *wname, TFORM target, char **hintout, char **helpout);
char * SUMA_hkcf(char *keyi, TFORM target);
char * SUMA_hkf(char *keyi, TFORM target);
char * SUMA_hkf_eng(char *keyi, TFORM target, char *cm);
void SUMA_help(void);
void SUMA_help_message(FILE *Out, TFORM targ);
void SUMA_cmap_help_message(FILE *Out, TFORM targ);
char * SUMA_help_message_Info(TFORM targ);
char * SUMA_help_xform_dot_message_Info(void);
char * SUMA_help_Cmap_message_Info(SUMA_COLOR_MAP *Cmp, TFORM targ);
char * SUMA_help_Plot_message_Info(void);
char *SUMA_help_SPEC_symbolic(void);
void SUMA_Help_open (void *p);
void SUMA_Help_destroyed (void *p);
void SUMA_Help_Cmap_open (void *p);
void SUMA_Help_Cmap_destroyed (void *p);
void SUMA_Help_Plot_open (void *p);
void SUMA_Help_Plot_destroyed (void *p);
void SUMA_Whereami_open (void *p);
void SUMA_Whereami_destroyed (void *p);
void SUMA_Message_open (void *p);
void SUMA_Message_destroyed (void *p);
void SUMA_VolSurf_help (FILE *Out);
void SUMA_Version (FILE *Out);
char * SUMA_New_Additions_perver (int ver, SUMA_Boolean StampOnly);
char * SUMA_New_Additions (int ver, SUMA_Boolean StampOnly);
char * SUMA_env_list_help(int DEFAULT_values, TFORM targ);
char *SUMA_All_Programs(void );
void SUMA_SetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
void SUMA_unSetcSV (Widget w, XtPointer clientData, XEvent * event, Boolean * cont);
char *SUMA_sources_Info(void );
char * SUMA_OptList_string(HELP_OPT *hol);
char *SUMA_OptList_get(HELP_OPT *hol, char *opname, char *what);
char *SUMA_NIDO_Info(void );
void SUMA_Snap_AllROICont (char *froot);
char * SUMA_Help_AllROICont (TFORM targ);
void SUMA_Snap_AllSurfCont (char *froot);
void SUMA_Snap_AllGraphCont (char *froot);
void SUMA_Snap_AllVolCont (char *froot);
void SUMA_Snap_AllMaskCont (char *froot);
void SUMA_Snap_AllTractCont (char *froot);
char * SUMA_Help_AllTractCont (TFORM targ);
char * SUMA_Help_AllMaskCont (TFORM targ);
char * SUMA_Help_AllVolCont (TFORM targ);
char * SUMA_Help_AllGraphCont (TFORM targ);
char * SUMA_Help_AllSurfCont (TFORM targ);
char * SUMA_Help_AllSurfCont_old (void);
char *SUMA_Name_GUI_Help(GUI_WIDGET_HELP *gwh);
char *SUMA_All_GUI_Help_Info(DList *dl, int detail, int format);
void SUMA_Show_All_GUI_Help(DList *dl, FILE *fout, int detail, int format);
GUI_WIDGET_HELP *SUMA_Get_GUI_Help( char *gname, TFORM format, 
                                    char **helpout, char **hintout, 
                                    int whelp_off);
int SUMA_Register_GUI_Help(char *which, char *hint, char *help, int type);
void SUMA_Free_Widget_Help(void *data);
char *SUMA_do_type_2_contwname(SUMA_DO_Types do_type);
void SUMA_suggest_GUI_Name_Match(char *wname, int nmx, DList *dl);



#endif



