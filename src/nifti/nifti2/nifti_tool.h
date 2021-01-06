#ifndef _NIFTI_TOOL_H_
#define _NIFTI_TOOL_H_

#define NT_CMD_LEN 2048

typedef struct{
   int     len;
   const char ** list;
} str_list;

typedef struct{
   int     len;
   int   * list;
} int_list;

typedef struct{
            /* action options (flags) */
   int      check_hdr,  check_nim;
   int      diff_hdr,   diff_hdr1, diff_hdr2, diff_nim;
   int      disp_hdr1,  disp_hdr2, disp_hdr,  disp_nim,  disp_ana;
   int      disp_exts,  add_exts,  rm_exts,   disp_cext;
   int      run_misc_tests;
   int      mod_hdr,    mod_hdr2,  mod_nim;
   int      swap_hdr,   swap_ana,  swap_old;

   int      strip;               /* strip extras from dataset(s)  */
   int      cbl, cci;            /* -copy_XXX option flags        */
   int      dts, dci, dci_lines; /* display collapsed img flags   */
   int      make_im;             /* create a new image on the fly */
   int64_t  ci_dims[8];          /* user dims list (last 7 valid) */
   int64_t  new_dim[8];          /* user dim list for new image   */
   int      new_datatype;        /* datatype for new image        */
   int      debug, keep_hist;    /* debug level and history flag  */
   int      overwrite;           /* overwrite flag                */
   char *   prefix;              /* for output file               */
   str_list elist;               /* extension strings             */
   int_list etypes;              /* extension type list           */
   str_list flist;               /* fields (to display or modify) */
   str_list vlist;               /* values (to set fields to)     */
   str_list infiles;             /* input files                   */
   char     command[NT_CMD_LEN]; /* for inserting the command     */
} nt_opts;

#define USE_SHORT       1
#define USE_FULL        2
#define USE_HIST        3
#define USE_FIELD_HDR1 11
#define USE_FIELD_HDR2 12
#define USE_FIELD_NIM1 21
#define USE_FIELD_NIM2 22
#define USE_FIELD_ANA  31
#define USE_DTYPES     41
#define USE_VERSION    51
#define USE_VER_MAN    52       /* for unix man-page formatting */
#define USE_SEE_ALSO   53       /* for unix man-page formatting */

#define CHECK_NEXT_OPT(n,m,str)                                       \
   do { if ( (n) >= (m) ) {                                           \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: 'nifti_tool -help'\n");       \
           return 1;      }                                           \
      } while(0)

#define CHECK_NEXT_OPT_MSG(n,m,str,msg)                               \
   do { if ( (n) >= (m) ) {                                           \
           fprintf(stderr,"** option '%s': %s\n",str,msg);            \
           fprintf(stderr,"   consider: 'nifti_tool -help'\n");       \
           return 1;      }                                           \
      } while(0)

/*----------------------------------------------------------------------
 * this structure and definitions will be used to process the nifti_1_header
 * and nifti_image structure fields (actions disp, diff, mod)
 *----------------------------------------------------------------------*/

#define NT_FIELD_NAME_LEN  20       /* more than length of longest name */
#define NT_HDR1_NUM_FIELDS 43       /* in the nifti_1_header struct     */
#define NT_HDR2_NUM_FIELDS 37       /* in the nifti_2_header struct     */
#define NT_ANA_NUM_FIELDS  47       /* in the  nifti_analyze75 struct   */
#define NT_NIM_NUM_FIELDS  63       /* in the nifti_image struct        */
#define NT_HDR_TIME_NFIELDS 8       /* num slice timing fields in hdr   */
#define NT_NIM_TIME_NFIELDS 11      /* num slice timing fields in nim   */
#define NT_DT_STRING      -0xfff    /* some strange number to abuse...  */
#define NT_DT_POINTER     -0xfef    /* some strange number to abuse...  */
#define NT_DT_CHAR_PTR    -0xfee    /* another...                       */
#define NT_DT_EXT_PTR     -0xfed    /* and another...                   */

typedef struct {
   int  type;                    /* one of the DT_* types from nifti1.h */
   int  offset;                  /* bytes from the start of the struct  */
   int  size;                    /* size of one element type            */
   int  len;                     /* number of elements                  */
   char name[NT_FIELD_NAME_LEN]; /* actual structure name used          */
} field_s;

/* for computing the offset from the start of the struct */
#define NT_OFF(str,field) ((int)( ((char *)&str.field) - ((char *)&str) ))
#define SHOW_STRUCT_OFFSET(str,field,mesg) do { str ss;                 \
   if(mesg)printf("%s ",(char*)mesg);                                   \
   printf("%s.%s @ offset %d\n", #str, #field, NT_OFF(ss,field)); }     \
   while (0)

/* call fill_field() for a single type, name and number of elements */
/* nstr is the base struct, and fldp is a field pointer */
#define NT_SFILL(nstr,fldp,type,name,num,rv) do{                   \
           rv=fill_field(fldp,type,NT_OFF(nstr,name),num,#name);   \
           fldp++; } while (0)

#define NT_MAKE_IM_NAME "MAKE_IM"

/* ================================================================= */
/* matrix operations                                                 */
/* (macros allow them to apply to either mat44 or dmat44)            */ 

/* fill MAT44 with MAT33 fields, then pad with 0.0 and a 1.0 at 3,3  */
#define NT_MAT33_TO_MAT44(m33, m44) do {                 \
   m44.m[0][0] = m33.m[0][0]; m44.m[0][1] = m33.m[0][1]; \
   m44.m[0][2] = m33.m[0][2];                            \
   m44.m[1][0] = m33.m[1][0]; m44.m[1][1] = m33.m[1][1]; \
   m44.m[1][2] = m33.m[1][2];                            \
   m44.m[2][0] = m33.m[2][0]; m44.m[2][1] = m33.m[2][1]; \
   m44.m[2][2] = m33.m[2][2];                            \
   /* and fill out the 4x4 mat */                        \
   m44.m[0][3] = m44.m[1][3] = m44.m[2][3] = 0.0;        \
   m44.m[3][0] = m44.m[3][1] = m44.m[3][2] = 0.0;        \
   m44.m[3][3] = 1.0;                                    \
   } while(0)

/* fill MAT33 with initial subset of MAT44 fields */
#define NT_MAT44_TO_MAT33(m44, m33) do {                 \
   m33.m[0][0] = m44.m[0][0]; m33.m[0][1] = m44.m[0][1]; \
   m33.m[0][2] = m44.m[0][2];                            \
   m33.m[1][0] = m44.m[1][0]; m33.m[1][1] = m44.m[1][1]; \
   m33.m[1][2] = m44.m[1][2];                            \
   m33.m[2][0] = m44.m[2][0]; m33.m[2][1] = m44.m[2][1]; \
   m33.m[2][2] = m44.m[2][2];                            \
   } while(0)

/* subtract 2 mat44 matrices */
#define NT_MAT44_SUBTRACT(mout, min0, min1) do {        \
   mout.m[0][0] = min0.m[0][0] - min1.m[0][0];          \
   mout.m[0][1] = min0.m[0][1] - min1.m[0][1];          \
   mout.m[0][2] = min0.m[0][2] - min1.m[0][2];          \
   mout.m[0][3] = min0.m[0][3] - min1.m[0][3];          \
   mout.m[1][0] = min0.m[1][0] - min1.m[1][0];          \
   mout.m[1][1] = min0.m[1][1] - min1.m[1][1];          \
   mout.m[1][2] = min0.m[1][2] - min1.m[1][2];          \
   mout.m[1][3] = min0.m[1][3] - min1.m[1][3];          \
   mout.m[2][0] = min0.m[2][0] - min1.m[2][0];          \
   mout.m[2][1] = min0.m[2][1] - min1.m[2][1];          \
   mout.m[2][2] = min0.m[2][2] - min1.m[2][2];          \
   mout.m[2][3] = min0.m[2][3] - min1.m[2][3];          \
   mout.m[3][0] = min0.m[3][0] - min1.m[3][0];          \
   mout.m[3][1] = min0.m[3][1] - min1.m[3][1];          \
   mout.m[3][2] = min0.m[3][2] - min1.m[3][2];          \
   mout.m[3][3] = min0.m[3][3] - min1.m[3][3];          \
   } while(0)

/* subtract 2 mat33 matrices */
#define NT_MAT33_SUBTRACT(mout, min0, min1) do {        \
   mout.m[0][0] = min0.m[0][0] - min1.m[0][0];          \
   mout.m[0][1] = min0.m[0][1] - min1.m[0][1];          \
   mout.m[0][2] = min0.m[0][2] - min1.m[0][2];          \
   mout.m[1][0] = min0.m[1][0] - min1.m[1][0];          \
   mout.m[1][1] = min0.m[1][1] - min1.m[1][1];          \
   mout.m[1][2] = min0.m[1][2] - min1.m[1][2];          \
   mout.m[2][0] = min0.m[2][0] - min1.m[2][0];          \
   mout.m[2][1] = min0.m[2][1] - min1.m[2][1];          \
   mout.m[2][2] = min0.m[2][2] - min1.m[2][2];          \
   } while(0)

/* fill with identity matrix */
#define NT_MAT44_SET_TO_IDENTITY(M) do {                                \
   M.m[0][0] = 1.0; M.m[0][1] = 0.0; M.m[0][2] = 0.0; M.m[0][3] = 0.0;  \
   M.m[1][0] = 0.0; M.m[1][1] = 1.0; M.m[1][2] = 0.0; M.m[1][3] = 0.0;  \
   M.m[2][0] = 0.0; M.m[2][1] = 0.0; M.m[2][2] = 1.0; M.m[2][3] = 0.0;  \
   M.m[3][0] = 0.0; M.m[3][1] = 0.0; M.m[3][2] = 0.0; M.m[3][3] = 1.0;  \
   } while(0)

#define NT_MAT33_SET_TO_IDENTITY(M) do {                                \
   M.m[0][0] = 1.0; M.m[0][1] = 0.0; M.m[0][2] = 0.0;                   \
   M.m[1][0] = 0.0; M.m[1][1] = 1.0; M.m[1][2] = 0.0;                   \
   M.m[2][0] = 0.0; M.m[2][1] = 0.0; M.m[2][2] = 1.0;                   \
   } while(0)



/*----------------------------------------------------------------------*/
/*-----  prototypes  ---------------------------------------------------*/
/*----------------------------------------------------------------------*/
int    act_add_exts   ( nt_opts * opts );
int    act_cbl        ( nt_opts * opts );  /* copy brick list */
int    act_cci        ( nt_opts * opts );  /* copy collapsed dimensions */
int    act_check_hdrs ( nt_opts * opts );  /* check for valid hdr or nim */
int    act_diff_hdrs  ( nt_opts * opts );
int    act_diff_hdr1s ( nt_opts * opts );
int    act_diff_hdr2s ( nt_opts * opts );
int    act_diff_nims  ( nt_opts * opts );
int    act_disp_ci    ( nt_opts * opts );  /* display general collapsed data */
int    act_disp_exts  ( nt_opts * opts );
int    act_disp_cext  ( nt_opts * opts );
int    act_disp_hdr   ( nt_opts * opts );
int    act_disp_hdr1  ( nt_opts * opts );
int    act_disp_hdr2  ( nt_opts * opts );
int    act_disp_nims  ( nt_opts * opts );
int    act_disp_anas  ( nt_opts * opts );
int    act_disp_ts    ( nt_opts * opts );  /* display time series */
int    act_mod_hdrs   ( nt_opts * opts );
int    act_mod_hdr2s  ( nt_opts * opts );
int    act_mod_nims   ( nt_opts * opts );
int    act_swap_hdrs  ( nt_opts * opts );
int    act_rm_ext     ( nt_opts * opts );
int    act_run_misc_tests( nt_opts * opts );
int    act_strip      ( nt_opts * opts );  /* strip extras from datasets */


field_s * get_hdr1_field( const char * fname, int show_fail );
field_s * get_hdr2_field( const char * fname, int show_fail );
field_s * get_nim_field( const char * fname, int show_fail );
const char    * field_type_str (int type);

int diff_hdr1s    (nifti_1_header *s0, nifti_1_header *s1, int display);
int diff_hdr1s_list(nifti_1_header *s0, nifti_1_header *s1, str_list *slist,
                    int display);
int diff_hdr2s    (nifti_2_header *s0, nifti_2_header *s1, int display);
int diff_hdr2s_list(nifti_2_header *s0, nifti_2_header *s1, str_list *slist,
                    int display);
int diff_nims     (nifti_image *s0,nifti_image *s1,        int display);
int diff_nims_list(nifti_image *s0,nifti_image *s1,str_list *slist,int display);

int add_int          (int_list * ilist, int val);
int add_string       (str_list * slist, const char * str);
int check_total_size ( const char *mesg, field_s *fields, int nfields, int tot_size);
int clear_float_zeros( char * str );
int diff_field       (field_s *fieldp, void * str0, void * str1, int nfields);
int disp_cifti_extension ( const char *mesg, nifti1_extension * ext, int maxlen);
int disp_nifti1_extension( const char *mesg, nifti1_extension * ext, int maxlen);
int disp_field       (const char *mesg,field_s *fieldp,void *str,int nfields,int header);
int disp_field_s_list(const char * mesg, field_s *, int nfields);
int disp_nt_opts     ( const char *mesg, nt_opts * opts);
int disp_raw_data    (void * data, int type, int nvals, char space,int newline);
int fill_cmd_string  (nt_opts * opts, int argc, char * argv[]);
int fill_field       (field_s *fp, int type, int offset, int num, const char *name);
int fill_hdr1_field_array(field_s * nh_fields);
int fill_hdr2_field_array(field_s * nh_fields);
int fill_nim1_field_array(field_s * nim_fields);
int fill_nim2_field_array(field_s * nim_fields);
int fill_ana_field_array(field_s * ah_fields);
int modify_all_fields(void *basep, nt_opts *opts, field_s *fields, int flen);
int modify_field     (void * basep, field_s * field, const char * data);
int process_opts     (int argc, char * argv[], nt_opts * opts);
int remove_ext_list  (nifti_image * nim, const char ** elist, int len);
int usage            (char * prog, int level);
int use_full         ();
int verify_opts      (nt_opts * opts, char * prog);
int write_hdr_to_file (nifti_1_header * nhdr, const char * fname);
int write_hdr2_to_file(nifti_2_header * nhdr, const char * fname);


/* wrappers for nifti reading functions (allow MAKE_IM) */
nifti_image    * nt_image_read (nt_opts * opts, const char * fname,
                                int doread, int make_ver);
nifti_image    * nt_read_bricks(nt_opts * opts, char * fname, int len,
                                int64_t * list, nifti_brick_list * NBL);
void * nt_read_header(const char * fname, int * nver, int * swapped, int check,
                      int new_datatype, int64_t new_dim[8]);


/* misc functions */
int           nt_run_misc_nim_tests (nifti_image * nim);
static int    nt_disp_mat44_orient(const char * mesg, mat44 mat);
static int    nt_test_dmat44_quatern(nifti_image * nim);
static double dmat44_max_fabs(nifti_dmat44 m);
static double mat44_max_fabs(mat44 m);




#endif  /* _NIFTI_TOOL_H_ */
