#ifndef SUMA_STRING_MANIP_INCLUDED
#define SUMA_STRING_MANIP_INCLUDED

char *SUMA_Break_String(char *si, int mxln);
char *SUMA_Offset_Lines(char *si, int off);
char *SUMA_Cut_String(char *s, char *sc);
char *SUMA_Sphinx_DeRef(char *s, char *r);
char *SUMA_Swap_String(char *s, char *sc, char *sw);
NI_str_array *SUMA_Split_String(char *s, char *sc);
char *SUMA_Cut_Between_String(char *s, char *sc0, char *sc1, char *save);
void SUMA_Sphinx_String_Edit_Help(FILE *fout);
char *SUMA_Sphinx_File_Edit(char *fname, int targ, int off);
char *sphinxize_prog_help (char *prog, int verb);

#endif
