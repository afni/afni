#ifndef _NIFTI_TOOL_H_
#define _NIFTI_TOOL_H_

typedef struct{
   int     len;
   char ** list;
} str_list;

typedef struct{
   int      diff_nhdr, diff_nim;
   int      disp_nhdr, disp_nim;
   int      mod_nhdr,  mod_nim;
   int      debug;
   str_list elist;               /* extensions                    */
   str_list flist;               /* fields (to display or modify) */
   str_list vlist;               /* values (to set fields to)     */
   str_list infiles;             /* input files                   */
} nt_opts;

#define USE_SHORT   1
#define USE_FULL    2
#define USE_HIST    3
#define USE_VERSION 4

#define CHECK_NEXT_OPT(n,m,str)                                       \
   do { if ( (n) >= (m) ) {                                           \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: 'nifti_tool -help'\n");       \
           return 1;      }                                           \
      } while(0)

/* prototypes */
int  add_string     (str_list * slist, char * str);
int  disp_nt_opts   (char * mesg, nt_opts * opts);
int  process_opts   (int argc, char * argv[], nt_opts * opts);
int  usage          (char * prog, int level);


#endif  /* _NIFTI_TOOL_H_ */
