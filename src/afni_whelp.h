#ifndef AFNI_WHELP_INCLUDED
#define AFNI_WHELP_INCLUDED

char *AFNI_All_Documented_Widgets(void);
char * AFNI_gsf(char *uwname, TFORM target, char **hintout, char **helpout);
char * AFNI_Help_AllMainCont (TFORM targ);

extern char *SUMA_set_DocumentedWidgets(char **s) ;

#endif
