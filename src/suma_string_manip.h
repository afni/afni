#ifndef SUMA_STRING_MANIP_INCLUDED
#define SUMA_STRING_MANIP_INCLUDED

#include "suma_utils.h"

char *SUMA_EscapeChars(char *s1, char *ca, char *es);
char *SUMA_ReplaceChars(char *s1, char *ca, char *es);
char *insert_in_string(char **s, char *pos, char *ins, int *nalloc);
char *summarize_string(char *us, int lmax);
char *SUMA_strncat(char *s1, char *s2, int nmax);
char *line_begins_with(char *cur, char *opt, int *nb, 
                       char *term, char *bracketers, int mintoend);
void write_string(char *s, char *prelude, char *postscript,
                 int nmax, int multiline, FILE *fout);

NI_str_array * SUMA_NI_decode_string_list( char *ss , char *sep );
NI_str_array * SUMA_NI_string_vec_to_str_array( char **ss , int nss );
char  * SUMA_NI_get_ith_string( char *ss , char *sep, int i );
int  SUMA_NI_find_in_cs_string( char *ss , char *sep, char *str );
int SUMA_NI_get_num_strings( char *ss , char *sep);
void SUMA_Show_NI_str_ar(NI_str_array *nisa, FILE *out);
char *SUMA_NI_str_ar_2_comp_str (NI_str_array *nisa, char *sep);
NI_str_array *SUMA_comp_str_2_NI_str_ar(char *s, char *sep);
NI_str_array *SUMA_NI_str_array(NI_str_array *clss, char *what, char *action); 
int SUMA_NI_str_array_find( char *targ , NI_str_array *sar, int partial, int ci);


char *SUMA_Get_Sub_String(char *cs, char *sep, int ii);
int SUMA_Find_Sub_String(char *cs, char *sep, char *ss);
SUMA_Boolean SUMA_Set_Sub_String(char **cs, char *sep, int ii, char *str);
int SUMA_Remove_Sub_String(char *cs, char *sep, char *strn);

void *SUMA_AdvancePastNumbers(char *op, char **opend, SUMA_VARTYPE tp);
void *SUMA_strtol_vec(char *op, int nvals, int *nread, 
                      SUMA_VARTYPE vtp, char **opend);

char *SUMA_Break_String(char *si, int mxln);
char *SUMA_Offset_Lines(char *si, int off);
char *SUMA_Offset_SLines(char *si, int off);
char *SUMA_Cut_String(char *s, char *sc);
char *SUMA_Sphinx_DeRef(char *s, char *r);
char *SUMA_Swap_String(char **s, char *sc, char *sw);
NI_str_array *SUMA_Split_String(char *s, char *sc);
char *SUMA_Cut_Between_String(char *s, char *sc0, char *sc1, char *save);
char *SUMA_Sphinx_String_Edit(char **s, TFORM targ, int off);
char *SUMA_Sphinx_SetVars(char **s, TFORM targ);
void SUMA_Sphinx_String_Edit_Help(FILE *fout, int forweb);
char *SUMA_Sphinx_File_Edit(char *fname, TFORM targ, int off);
int SUMA_Demote_Underlining(char *sh);
int SUMA_is_underline(char *sh, char *ul, int *nread);
char *sphinxize_prog_help (char *prog, int verb);
char *sphinxize_prog_shelp (char *prog, char *oh, int verb);
SUMA_Boolean SUMA_Known_Sphinx_Dir(char *s);
SUMA_Boolean SUMA_Known_Sphinx_ADir(char *s);
char *SUMA_Sphinx_LineSpacer(char *s, TFORM targ);

int sphinx_offprintf(TFORM targ, int off, FILE *fout, char *str, ... );

#endif
