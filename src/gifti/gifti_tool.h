#ifndef GIFTI_TOOL_H
#define GIFTI_TOOL_H

typedef struct { int len; char ** list; } gt_str_list;
typedef struct { int len; int   * list; } gt_int_list;

typedef struct {
    /* start with GIFTI user options */
    int           verb;         /* verbose level */
    int           indent;       /* spaces per indent level */
    int           buf_size;     /* buffer space for expat library */
    int           b64_check;    /* check level */
    int           zlevel;       /* compression level for output data    */

    int           dstore;       /* whether to store read data */
    int           encoding;     /* encoding for output data             */
    int           show_gifti;   /* do we read data */

    char        * ofile_1D;     /* 1D output filename           */
    char        * ofile_asc;    /* 'asc' output filename        */
    char        * ofile_gifti;  /* GIFTI output filename        */
    gt_int_list   DAlist;       /* list of DataArray indices    */
    gt_str_list   infiles;
} gt_opts;

#undef CHECK_NEXT_OPT
#define CHECK_NEXT_OPT(n,m,str)                                     \
   do { if ( (n) >= (m) ) {                                          \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: '-help' option\n");            \
           return 1;      }                                             \
      } while(0)

#undef CHECK_NEXT_OPT2
#define CHECK_NEXT_OPT2(n,m,s1,s2)                                        \
   do { if ( (n) >= (m) ) {                                                \
           fprintf(stderr,"** option '%s': missing parameter '%s'\n",s1,s2);\
           fprintf(stderr,"   consider: '-help' option\n");                  \
           return 1;      }                                                   \
      } while(0)

int ewrite_data_line (void *, int, int, int, int, int, FILE *);
int ewrite_many_lines(void **, int, long long, long long, int, FILE *);
int write_1D_file    (giiDataArray **, int, char *, int);
int write_as_asc     (gifti_image *, char *);
int write_surf_file  (giiDataArray *, giiDataArray *, char *, int);

#endif /* GIFTI_TOOL_H */
