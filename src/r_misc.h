
#ifndef _R_MISC_H_
#define _R_MISC_H_

/* --- functions that are open to use:  ------------------------- */

int r_sprintf_long_to_hex ( char * dest, unsigned long lsrc,
                            int bytes, int pad ); 
unsigned long r_hex_str_to_long ( char * src, int hex_digits );

char * cat_strings        (char * slist[], int nstr, char * sepstr);
int disp_strings(FILE * fp, char * mesg, int nstr, char * slist[],
                 int bot, int top, char * sepstr, int newline);

/* -------------------------------------------------------------- */


typedef struct { double xyz[3]; } THD_dvec3;

#ifndef ORCODE
#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )
#endif  /* ORCODE */

#ifndef OR3OK
#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )
#endif

#endif
