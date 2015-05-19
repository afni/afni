#ifndef SUMA_HELP_INCLUDED
#define SUMA_HELP_INCLUDED

char * SUMA_gsf_eng(char *wname, TFORM target, char **hintout, char **helpout);
char * SUMA_hkcf(char *keyi, TFORM target);
char *SUMA_Sphinx_Widget_Name_2_Link(char *name) ;
char * SUMA_hkf(char *keyi, TFORM target);
char * SUMA_hkf_eng(char *keyi, TFORM target, char *cm);
char *SUMA_Name_GUI_Help_eng(GUI_WIDGET_HELP *gwh, int lvl);
GUI_WIDGET_HELP *SUMA_Get_GUI_Help( char *gname, TFORM format, 
                                    char **helpout, char **hintout, 
                                    int whelp_off);
SUMA_Boolean SUMA_Register_Widget_Help(Widget w, int type, char *name, 
                                       char *hint, char *help);
SUMA_Boolean SUMA_Register_Widget_Children_Help(Widget, int type, char *name, 
                                                char *hint, char *help);  
char *SUMA_Name_GUI_Help(GUI_WIDGET_HELP *gwh);
GUI_WIDGET_HELP *SUMA_Get_Widget_Help( Widget w );
char *SUMA_All_GUI_Help_Info(DList *dl, int detail, int format);
void SUMA_Show_All_GUI_Help(DList *dl, FILE *fout, int detail, int format);
int SUMA_Register_GUI_Help(char *which, char *hint, char *help, 
                           Widget w, int type);
void SUMA_Free_Widget_Help(void *data);
void SUMA_suggest_GUI_Name_Match(char *wname, int nmx, DList *dl);
SUMA_Boolean SUMA_is_Documented_Widget(char *wname);
char *SUMA_get_DocumentedWidgets(void);
void SUMA_free_DocumentedWidgets(void); 

#endif
