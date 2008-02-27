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
#define PPC "PP Chrisidis"
#define BGP "BG Pittman"

#define MIN_PROG_RANK  1                       /* min in list */
#define MICRO     1  /* rank for changes that users don't see */
#define MINOR     2  /* rank for small changes that users see */
#define MAJOR     3  /* rank for large changes that users see */
#define SUPER     4  /* rank for changes that users must know */
#define MAX_PROG_RANK  4                       /* max in list */

#define micro  1
#define minor  2
#define major  3
#define super  4

#define TYPE_NOT_SET    0
#define TYPE_NEW_OPT    1
#define TYPE_NEW_PROG   2
#define TYPE_FIX        3


typedef struct {
  short   dd;          /* Day: 1..31 */
  short   mm;          /* Month: 1..12 (use a macro) */
  short   yyyy;        /* Year: 2001..9999 */
  char  * author;      /* use a macro */
  char  * program;     /* will be checked against allowed names */
  short   rank;        /* use a macro */
  char  * desc;        /* one line description */
  char  * verbtext;    /* can be NULL, but shouldn't be */
} afni_history_struct;


#define MAX_LINE_CHARS  73  /* max description chars without newline   */
                            /* 80 - ( 3 spaces + " + \n" )             */
#define NUM_HIST_USERS  20  /* increase this if we go beyond 20 people */
#define FIRST_YEAR    2001  /* start of SSCC, just in case (Ziad...)   */


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
    afni_history_struct * hlist;
    char                * author;
} histpair;

typedef struct {
    /* user options */
    char     * author;
    char     * program;
    int        html;
    int        level;
    int        min_level;
    int        past_days, past_months, past_years;
    int        verb;

    /* assigned variables */
    int        plen;
    histpair * histpairs;
} global_data;

#undef INT_IN_RANGE
#define INT_IN_RANGE(val,min,max) (((val)<(min)) ? 0 : ((val)>(max)) ? 0 : 1)

#undef CHECK_NULL_STR
#define CHECK_NULL_STR(str) (str ? str : "NULL")

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

int histlists_are_valid (histpair * hpairs, int plen);
int hlist_is_sorted     (afni_history_struct * hlist);
int hlist_len           (afni_history_struct * hlist);
int init_histlist       (global_data * gd);
int process_options     (int argc, char * argv[], global_data * gd);
int show_help           (void);
int valid_dstring       (char * str, int max_line_len);
int valid_histlist      (afni_history_struct * hlist, char * author);
int valid_histstruct    (afni_history_struct * hstr, char * author);
int valid_program       (char * prog);

#endif /* _AFNI_HISTORY_HEADER_ */
