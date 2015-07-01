#ifndef SUMA_UTILS_INCLUDED
#define SUMA_UTILS_INCLUDED

/* Do not include matrix.h if matrix_f.h has been included
Otherwise any program using matrix_f.h will have type conflicts.
Also, any inclusion of matrix_f.h should precede inclusion of
mri_lib.h, 3ddata.h, or suma_string_utils.h */
#ifndef  MATRIX_F_INCLUDED
#include "matrix.h"
#endif

typedef enum { SUMA_notypeset = -1, 
               SUMA_byte = NI_BYTE, 
               SUMA_short = NI_SHORT, 
               SUMA_int = NI_INT, 
               SUMA_float = NI_FLOAT32, 
               SUMA_double = NI_FLOAT64, 
               SUMA_complex = NI_COMPLEX64,
               SUMA_string = NI_STRING} SUMA_VARTYPE;

typedef enum { SUMA_noGLtuples = -1, 
               SUMA_b = 1001, /* unsigned byte */
               SUMA_bbb = 1002, /* 3* unsigned byte */ 
               SUMA_bbbb = 1003, /* 3* unsigned byte */ 
               SUMA_F = 1011, /* one float */
               SUMA_FFF = 1012, /* 3* float */
               SUMA_FFFF = 1013, /* 4 * float */
              } SUMA_GL_TUPLES;

#define SUMA_MX_VEC_MAX_DIMS 50
typedef struct {
   SUMA_VARTYPE tp;
   int N_dims;
   int N_vals;
   int dims[SUMA_MX_VEC_MAX_DIMS];
   int fdfm[SUMA_MX_VEC_MAX_DIMS];
   void *v;
   byte *bv;
   short *sv;
   int *iv;
   float *fv;
   double *dv;
   complex *cv;
   byte fdf;
   matrix *m;
} SUMA_MX_VEC;

/*! filename and path */
typedef struct {
   char *Path;
   char *FileName;
}SUMA_FileName;

/*! filename, extension and path */
typedef struct {
   char *AbsPath;
   char *RelPath;
   char *RelDir;
   char *Path;
   char *FileName;
   char *FileName_NoExt;
   char *FullName;
   char *FullName_NoSel;
   char *Ext;
   char *Prefix;
   char *View;
   char *TypeExt;
   char *StorageModeName;
   int   StorageMode;
   char *NodeSelect;
   char *ColSelect;
   char *RowSelect;
   char *RangeSelect;
   int only_index;
   int OnDisk;
   unsigned long Size;
   char *HeadName;
   char *BrikName;
   char *NameAsParsed;
   char *cwdAsParsed;
   char *ExistsAs;
}SUMA_PARSED_NAME;

typedef struct {
   char *envhelp;
   char *envname;
   char *envval;  /* This is the default */
}ENV_SPEC;

typedef struct {
   char *name;
   char *help;
   char *val;  /* This is the default */
}HELP_OPT;

typedef struct {
   char name[10][64]; /*Name of gui with lineage. 
                        E.g. for SurfCont->Coloring Controls->more
                        [0] "SurfCont"
                        [1] "Coloring Controls" 
                        [2] "more"
                        [3] ""                     */
   int name_lvl;
   char hint[256];  /* Whatever is registered under "hint" */
   char *help;  /* Whatever is registered under "help", this one
                      will be a pointer copy so don't free it.*/
   int type; /* 0 -- A container widget, not one to be pressed 
                1 -- A regular widget */
   void *w; /* A copy of the widget pointer... Is this wise?*/
}GUI_WIDGET_HELP;


/*! string stucture */
typedef struct {
   int N_alloc;  /*!< space allocated for s */
   char *s; /*!< string s */
} SUMA_STRING;


#define SUMA_Boolean byte
#define NOPE 0
#define YUP 1


#define SUMA_MAX_NAME_LENGTH 500   /*!< Maximum number of characters in a filename */
#define SUMA_MAX_DIR_LENGTH 2000    /*!< Maximum number of characters in a directory name */
#define SUMA_MAX_FILENAME_LENGTH (SUMA_MAX_NAME_LENGTH+SUMA_MAX_DIR_LENGTH+1)
#ifndef SUMA_IDCODE_LENGTH
   #define SUMA_IDCODE_LENGTH 50
#endif

#define SUMA_VERSION_VECTOR 20060703, 20041229, 20040610, 20040116, \
                            20040106, -10000 /*!< modify this dude and you must update SUMA_New_Additions_perver 
                                       in SUMA_help.c. 
                                       Add to the left of the vector, leave the last value of -10000 untouched
                                       If you like to think of floating point version numbers,divide by 10000
                                       This define is stuck here so that non-SUMA DataSet manipulating programs 
                                       can use it, one hopes.
                                       Numbering is yyyymmdd */

/* Do not use SUMA_IS_NUM_E inside SUMA_IS_DIGIT_CHAR 
   See also SUMA_IS_DIGIT */
#define SUMA_IS_DIGIT_CHAR(s,n) (\
   (isdigit(s[n]) || s[n] == '.' || s[n] == '-' || s[n] == '+') )
#define SUMA_IS_NUM_E(s, n) (\
   (n > 0 && (s[n] == 'e' || s[n] == 'E') && SUMA_IS_DIGIT_CHAR(s,n-1)) )
#define SUMA_IS_NUM_CHAR(s,n) (SUMA_IS_DIGIT_CHAR(s,n) ||  SUMA_IS_NUM_E(s,n))


#define SUMA_EMPTY_ATTR "~"
#define AFNI_NI_CSS "~"    
#define AFNI_NI_cSS '~'    /* AFNI's NIML Column String Separator (used to separate strings belonging to different columns of input) 
                              AFNI takes the niml attribute string and turns it into its internal attribute structure.
                              AFNI_NI_CSS[0] is replaced by \0 and the string is broken into its per-column string list.
                              That is not the case for BRICK_STATSYM which is treated as one ';' separated string that gets
                              separately transformed into BRICK_STATAUX attribute entry in AFNI.
                              This is somewhat confusing when viewing an AFNI dataset in NIML format because BRICK_STATSYM is of ni_type = "String"
                              like say BRICK_LABS. However this makes sense in AFNI's format because these strings actually
                              get transformed to attributes of different types in AFNI's internal format.
                              SUMA will use the same separator for all attributes of ni_type = "String".
                              Use Macros SUMA_2_AFNI_NI_PCS and AFNI_2_SUMA_NI_PCS if you need to change between SUMA- and AFNI-per-column-strings  
                              */
#define SUMA_NI_CSS ";"    /* SUMA's NIML  Per-Column-String Separator 
                              (used to separate strings belonging to 
                              different columns of input)  */
#define SUMA_NI_cSS ';'

#define SUMA_2_AFNI_NI_PCS(a) {\
   int m_i = 0; \
   if ((a)) { while ((a)[m_i]) { if ((a)[m_i] == SUMA_NI_cSS) (a)[m_i] = AFNI_NI_cSS; ++m_i; } }\
}

#define AFNI_2_SUMA_NI_PCS(a) {\
   int m_i = 0; \
   if ((a)) { while ((a)[m_i]) { if ((a)[m_i] == AFNI_NI_cSS) (a)[m_i] = SUMA_NI_cSS; ++m_i; } }\
}

/*! macros to access pointers to elements in type double multiplexed vectors with mxv->fdf = 1;
  \sa SUMA_NewMxVec*/ 
#define mxvdp4(mxv,i,j,k,l) ( mxv->dv + ( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1] + (int)(l) * mxv->fdfm[2] ) )
#define mxvdp3(mxv,i,j,k)   ( mxv->dv + ( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1]   ) )
#define mxvdp2(mxv,i,j  )   ( mxv->dv + ( (int)(i) + (int)(j) * mxv->fdfm[0]   ) )
#define mxvdp1(mxv,i  )     ( mxv->dv + ( (int)(i)   ) )
/*! macros to access elements in type double multiplexed vectors with mxv->fdf = 1;
  \sa SUMA_NewMxVec*/ 
#define mxvd4(mxv,i,j,k,l) ( mxv->dv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1] + (int)(l) * mxv->fdfm[2] )] )
#define mxvd3(mxv,i,j,k)   ( mxv->dv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1]   )] )
#define mxvd2(mxv,i,j  )   ( mxv->dv[( (int)(i) + (int)(j) * mxv->fdfm[0]   )] )
#define mxvd1(mxv,i  )     ( mxv->dv[( (int)(i)   )] )
/*! macros to access elements in type complex multiplexed vectors with mxv->fdf = 1;
  \sa SUMA_NewMxVec*/ 
#define mxvc4(mxv,i,j,k,l) ( mxv->cv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1] + (int)(l) * mxv->fdfm[2] )] )
#define mxvc3(mxv,i,j,k)   ( mxv->cv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1]   )] )
#define mxvc2(mxv,i,j  )   ( mxv->cv[( (int)(i) + (int)(j) * mxv->fdfm[0]   )] )
#define mxvc1(mxv,i  )     ( mxv->cv[( (int)(i)   )] )

#define IN_MASK(mm,k) ( (!(mm) || (mm)[k]) )

SUMA_MX_VEC *SUMA_FreeMxVec(SUMA_MX_VEC *mxv);
SUMA_MX_VEC *SUMA_NewMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, 
                           byte first_dim_first);
char *SUMA_MxVec_Info (SUMA_MX_VEC *mxv, int detail, char *title);
void SUMA_ShowMxVec (SUMA_MX_VEC *mxv, int detail, FILE *out, char *title);
int SUMA_MxVecInit(SUMA_MX_VEC *mxv, void *val);
int SUMA_NewMxAllocVec(SUMA_MX_VEC *mxv) ;
SUMA_MX_VEC *SUMA_NewMxNullVec(SUMA_VARTYPE tp, int N_dims, int *dims, 
                               byte first_dim_first);
SUMA_MX_VEC *SUMA_VecToMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, 
                              byte first_dim_first, void *vec);

char * SUMA_to_lower(char *s) ;
int SUMA_filexists (char *f_name);
int SUMA_search_file(char **fnamep, char *epath);
char *SUMA_help_basics();
char *SUMA_help_cmap();
char *SUMA_help_talk();
char *SUMA_help_mask();
char *SUMA_help_dset();
int get_Domemtrace(void);
void set_Domemtrace(int s);
int get_Doiotrace(void);
void set_Doiotrace(int s) ;
int get_IgnoreXforms(void);
void setIgnoreXforms(int s) ;
void SUMA_process_environ(void);
int NoSumaRcFound (void);
void SUMA_ParseInput_basics_ns (char *argv[], int argc); 
int SUMA_ParseInput_basics_eng (char *argv[], int argc); 
void WorkErrLog_ns(void);
SUMA_FileName SUMA_StripPath (char *FileName);
SUMA_PARSED_NAME * SUMA_ParseFname (char *FileName, char *cwd);
SUMA_PARSED_NAME * SUMA_ParseFname_eng (char *FileName, char *ucwd, 
                                        int diskcheck);
SUMA_PARSED_NAME * SUMA_DuplicateParsedName(SUMA_PARSED_NAME *pn);
SUMA_PARSED_NAME * SUMA_ModifyParsedName (SUMA_PARSED_NAME *pn, 
                                          char *what, char *val);
char * SUMA_ModifyName(char *name, char *what, char *val, char *cwd);
SUMA_PARSED_NAME * SUMA_ParseModifyName(char *name, 
                                        char *what, char *val, char *cwd);
char *SUMA_Extension(char *filename, char *ext, SUMA_Boolean Remove);
SUMA_Boolean SUMA_isExtension(char *filename, char *ext);
char * SUMA_CropExtension(char *filename, char *ext);
void *SUMA_Free_Parsed_Name(SUMA_PARSED_NAME *Test);
char *SUMA_FnameGet(char *Fname, char *sel, char *cwd);
int SUMA_NumStringUnits (char *s, int marktip); 
int SUMA_strtod(char *n, double *valp);
int SUMA_StringToNum (char *s, void *vv, int N, int p);
int SUMA_StringToNumSide (char *s, void *vv, int N, int p, int *sd);
int SUMA_isNumString (char *s, void *p);
int SUMA_CleanNumString (char *s, void *p);
int SUMA_CleanNumStringSide (char *s, void *p);
char *SUMA_copy_string(char *buf);
char * SUMA_replace_string(char *s1, char *s2);
char *SUMA_copy_quoted( char *s, char *eop, 
                        char q1, char q2, int deblank,
                        int withquotes, int *is_closed );
char *args_in_quotes(char **argv, int *kar, int N_argv, char *opq, 
                     char *cloq, int clearused);
char *args_in_niml_quotes(char **argv, int *kar, int N_argv, int clearused);
char *args_in_simple_quotes(char **argv, int *kar, int N_argv, int clearused);
char * SUMA_append_string(char *s1, char *s2);
char * SUMA_append_extension(char *s1, char *s2);
char * SUMA_append_replace_string(char *s1, char *s2, char *Spc,int whichTofree);
char * SUMA_ar_string(char *s1, char *s2, char *Spc, int whichTofree);
char * SUMA_append_replace_string_eng(char *s1, char *s2, char *Spc, 
                                      int whichTofree, int cleanstart);
char * SUMA_append_replace_num(char *s1, char *form, double num, 
                               SUMA_VARTYPE tp, int whichTofree);
char * SUMA_truncate_string (char *s1, int length);
char *SUMA_set_string_length(char *buf, char cp, int n);
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring);
SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... );

void SUMA_sigfunc(int sig);
char * SUMA_pad_string(char *buf, char cp, int n, int add2end);
NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa);
int SUMA_AddColAtt_CompString(NI_element *nel, int col, char *lbl, 
                              char *sep, int insert_mode);

SUMA_Boolean SUMA_ShowParsedFname(SUMA_PARSED_NAME *pn, FILE *out);

char *SUMA_isEnv(char *env, char *sval);
float SUMA_floatEnv(char *env, float defval);
ENV_SPEC SUMA_envlistelement(int i);
char * SUMA_EnvVal(char *env);
int SUMA_EnvEquals(char *env, char *sval, byte ci, char *sep);


int SUMA_NodeIndex_To_Index(int *NodeIndex, int N_Node, int n);
SUMA_Boolean SUMA_binSearch(float *nodeList, float target,int *seg, byte ematch);
int SUMA_binFind( float *indexList, int N_node, float target, byte ematch);
SUMA_Boolean SUMA_ibinSearch( int *indexList, int target, int *seg);
int SUMA_ibinFind( int *indexList, int N_node, int target);
int *SUMA_reorder(int *y, int *isort, int N_isort);
char **SUMA_sreorder(char **y, int *isort, int N_isort);
double *SUMA_dreorder(double *y, int *isort, int N_isort);
byte *SUMA_breorder(byte *y, int *isort, int N_isort);
float *SUMA_freorder(float *y, int *isort, int N_isort);
float *SUMA_freorder_triplets(float *y, int *isort, int N_isort);
char *SUMA_floats_to_string(float *rgba, int N, float scl, char *here, int *Err,
                            char *sep, int MVf);
#define SUMA_RGBA_to_string SUMA_floats_to_string
float *SUMA_string_to_RGBA(char *s, float *here, float scl, int *Err);            
#endif
