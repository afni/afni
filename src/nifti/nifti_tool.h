#ifndef _NIFTI_TOOL_H_
#define _NIFTI_TOOL_H_

typedef struct{
   int     len;
   char ** list;
} str_list;

typedef struct{
   int      diff_hdr,  diff_nim;
   int      disp_hdr,  disp_nim;
   int      disp_exts, add_exts, rm_exts;
   int      help_hdr,  help_nim; /* show struct fields            */
   int      mod_hdr,   mod_nim;
   int      debug, overwrite;    /* overwrite input files?        */
   char *   prefix;              /* for output file               */
   str_list elist;               /* extensions                    */
   str_list flist;               /* fields (to display or modify) */
   str_list vlist;               /* values (to set fields to)     */
   str_list infiles;             /* input files                   */
} nt_opts;

#define USE_SHORT      1
#define USE_FULL       2
#define USE_HIST       3
#define USE_FIELD_HDR  4
#define USE_FIELD_NIM  5
#define USE_VERSION    6

#define CHECK_NEXT_OPT(n,m,str)                                       \
   do { if ( (n) >= (m) ) {                                           \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: 'nifti_tool -help'\n");       \
           return 1;      }                                           \
      } while(0)

/*----------------------------------------------------------------------
 * this structure and definitions will be used to process the nifti_1_header
 * and nifti_image structure fields (actions disp, diff, mod)
 *----------------------------------------------------------------------*/

#define NT_FIELD_NAME_LEN  20       /* more than length of longest name */
#define NT_HDR_NUM_FIELDS  43       /* in the nifti_1_header struct     */
#define NT_NIM_NUM_FIELDS  63       /* in the nifti_image struct        */
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

/* call fill_field() for a single type, name and number of elements */
/* nstr is the base struct, and fldp is a field pointer */
#define NT_SFILL(nstr,fldp,type,name,num,rv) do{                   \
           rv=fill_field(fldp,type,NT_OFF(nstr,name),num,#name);   \
           fldp++; } while (0)

/*----------------------------------------------------------------------*/
/*-----  prototypes  ---------------------------------------------------*/
/*----------------------------------------------------------------------*/
int    act_add_exts   ( nt_opts * opts );
int    act_diff_hdrs  ( nt_opts * opts );
int    act_diff_nims  ( nt_opts * opts );
int    act_disp_exts  ( nt_opts * opts );
int    act_disp_hdrs  ( nt_opts * opts );
int    act_disp_nims  ( nt_opts * opts );
int    act_mod_hdrs   ( nt_opts * opts );
int    act_mod_nims   ( nt_opts * opts );
int    act_rm_ext     ( nt_opts * opts );

field_s * get_hdr_field( char * fname, int show_fail );
field_s * get_nim_field( char * fname, int show_fail );
char    * field_type_str (int type);

int diff_hdrs     (nifti_1_header *s0, nifti_1_header *s1, int display);
int diff_hdrs_list(nifti_1_header *s0, nifti_1_header *s1, str_list *slist,
                   int display);
int diff_nims     (nifti_image *s0,nifti_image *s1,        int display);
int diff_nims_list(nifti_image *s0,nifti_image *s1,str_list *slist,int display);

int add_string       (str_list * slist, char * str);
int check_total_size (char *mesg, field_s *fields, int nfields, int tot_size);
int diff_field       (field_s *fieldp, void * str0, void * str1, int nfields);
int disp_nifti1_extension(char *mesg, nifti1_extension * ext);
int disp_field       (char *mesg,field_s *fp,void *str,int nfields,int header);
int disp_field_s_list(char * mesg, field_s *, int nfields);
int disp_nt_opts     (char * mesg, nt_opts * opts);
int fill_field       (field_s *fp, int type, int offset, int num, char *name);
int fill_hdr_field_array(field_s * nh_fields);
int fill_nim_field_array(field_s * nim_fields);
int modify_all_fields(void *basep, nt_opts *opts, field_s *fields, int flen);
int modify_field     (void * basep, field_s * field, char * data);
int process_opts     (int argc, char * argv[], nt_opts * opts);
int usage            (char * prog, int level);
int use_full         (char * prog);
int verify_opts      (nt_opts * opts, char * prog);
int write_hdr_to_file(nifti_1_header * nhdr, char * fname);



#endif  /* _NIFTI_TOOL_H_ */
