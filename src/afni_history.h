#ifndef _AFNI_HISTORY_HEADER_
#define _AFNI_HISTORY_HEADER_

#include <stdio.h>

#define JAN  1
#define FEB  2
#define MAR  3
#define APR  4
#define MAY  5
#define JUN  6
#define JUL  7
#define AUG  8
#define SEP  9
#define OCT 10
#define NOV 11
#define DEC 12

#define RWC "RW Cox"
#define ZSS "ZS Saad"
#define DRG "DR Glen"
#define RCR "RC Reynolds"
#define GC  "G Chen"         /* no middle initial (clearly evil) */
#define PPC "PP Christidis"
#define BGP "BG Pittman"

/* importance levels */
#define MIN_PROG_LEVEL  1  /* min in list */
#define MICRO           1  /* level for changes that users don't see */
#define MINOR           2  /* level for small changes that users see */
#define MAJOR           3  /* level for larger changes               */
#define SUPER           4  /* level for big changes, or new programs */
#define SUPERDUPER      5  /* level for changes that users must know */
#define MAX_PROG_LEVEL  5  /* max in list */

/* change types (we mostly care about new things and bug fixes) */
#define TYPE_INVALID   -1  /* bad, naughty type */
#define TYPE_GENERAL    0  /* if it doesn't fit any other category type */
#define TYPE_NEW_PROG   1  /* new program */
#define TYPE_NEW_OPT    2  /* new program option */
#define TYPE_NEW_ENV    3  /* environmental change or new env variable */
#define TYPE_BUG_FIX    4  /* enhancement of an existing bug */
#define TYPE_MODIFY     5  /* a change (not new, not a bug fix) */
#define TYPE_ENHANCE    6  /* general enhancement */
#define MAX_TYPE_VAL    6  /* maximum type value */


typedef struct {
  short   dd;          /* Day: 1..31 */
  short   mm;          /* Month: 1..12 (use a macro) */
  short   yyyy;        /* Year: 2001..9999 */
  char  * author;      /* use a macro */
  char  * program;     /* will be checked against allowed names */
  short   level;       /* importance:  use a macro */
  short   type;        /* change type: use a macro */
  char  * desc;        /* one line description */
  char  * verbtext;    /* can be NULL, but shouldn't be */
} afni_history_struct;
typedef afni_history_struct hist_type;


#define MAX_LINE_CHARS  73  /* max description chars without newline   */
                            /* 80 - ( 3 spaces + " + \n" )             */
#define NUM_HIST_USERS  20  /* increase this if we go beyond 20 people */
#define FIRST_YEAR    1994  /* start of AFNI, just in case (RWCox..)   */


/* refereces to the structures in each history file */
extern afni_history_struct bpittman_history[];
extern afni_history_struct christip_history[];
extern afni_history_struct dglen_history[];
extern afni_history_struct gangc_history[];
extern afni_history_struct rickr_history[];
extern afni_history_struct rwcox_history[];
extern afni_history_struct ziad_history[];


/*----------------------------------------------------------------------*/
/*-- specific to afni_history.c ----------------------------------------*/

typedef struct {
    hist_type * hlist;
    char      * author;
} histpair;

typedef struct {
    /* user options */
    char     * author;
    char     * program;
    int        html;
    int        dline;
    int        type;
    int        level;
    int        min_level;
    int        past_days, past_months, past_years, past_entries;
    int        sort_dir;
    int        verb;

    /* assigned variables */
    int        plen;
    histpair * histpairs;

    int        argc;
    char    ** argv;
} global_data;

#undef INT_IN_RANGE
#define INT_IN_RANGE(val,min,max) (((val)<(min)) ? 0 : ((val)>(max)) ? 0 : 1)

#undef CHECK_NULL_STR
#define CHECK_NULL_STR(str) ((str) ? (str) : "NULL")

#undef CHECK_NEXT_OPT
#define CHECK_NEXT_OPT(n,m,str)                                     \
   do { if ( (n) >= (m) ) {                                          \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: '-help' option\n");            \
           return -1;      }                                            \
      } while(0)

#undef CHECK_NEXT_OPT2
#define CHECK_NEXT_OPT2(n,m,s1,s2)                                        \
   do { if ( (n) >= (m) ) {                                                \
           fprintf(stderr,"** option '%s': missing parameter '%s'\n",s1,s2);\
           fprintf(stderr,"   consider: '-help' option\n");                  \
           return -1;      }                                                  \
      } while(0)

/* main protos */
int add_to_hlist        (hist_type *** hlist, hist_type * hadd,
                         int addlen, int * newlen);
int compare_hlist       (const void *v0, const void *v1);
int compare_hist_dates  (hist_type *h0, hist_type *h1);    /* RWC */
int disp_global_data    (char * mesg, global_data * gd);
int histlists_are_valid (histpair * hpairs, int plen);
int hlist_is_sorted     (hist_type * hlist);
int hlist_len           (hist_type * hlist);
int init_histlist       (global_data * gd);
int process_options     (int argc, char * argv[], global_data * gd);
int restrict_by_date    (global_data * gd, hist_type *** hlist, int * len);
int restrict_by_level   (global_data * gd, hist_type *** hlist, int * len);
int restrict_by_program (global_data * gd, hist_type *** hlist, int * len);
int restrict_by_type    (global_data * gd, hist_type *** hlist, int * len);
int show_author_list    (void);
int show_command        (FILE * fp, int argc, char ** argv);
int show_help           (void);
int show_hist_type      (hist_type * hp, FILE * fp);
int show_history        (global_data * gd, hist_type ** hlist, int len);
int show_html_footer    (FILE * fp);
int show_html_header    (global_data * gd, FILE * fp, int min_level);
int show_html_separator (FILE * fp);
int show_dline_separator(FILE * fp);
int show_results        (global_data * gd);
int show_valid_types    (void);
int show_wrapping_line  (char * str, char * prefix, int indent, FILE * fp);
int type_string2type    (char * tstring);
int valid_dstring       (char * str, int max_line_len);
int valid_histlist      (hist_type * hlist, char * author);
int valid_histstruct    (hist_type * hstr, char * author);
int valid_program       (char * prog);

char * convert_author   (char * name);
char * level_string     (int level);
char * mm2month         (int mm);
char * type_string      (int level);

#endif /* _AFNI_HISTORY_HEADER_ */
