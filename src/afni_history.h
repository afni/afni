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


typedef struct {
  short dd;          /* Day: 1..31 */
  short mm;          /* Month: 1..12 (use a macro) */
  short yyyy;        /* Year: 2006..9999 */
  char *author;      /* use a macro */
  char *program;     /* will be checked against allowed names */
  short rank;        /* use a macro */
  char *desc;        /* one line description */
  char *verbtext;    /* can be NULL, but shouldn't be */
} afni_history_struct;


#define MAX_LINE_CHARS  73  /* max description chars without newline   */
                            /* 80 - ( 3 spaces + " + \n" )             */
#define NUM_HIST_USERS  20  /* increase this if we go beyond 20 people */


extern afni_history_struct bpittman_history[];
extern afni_history_struct christip_history[];
extern afni_history_struct dglen_history[];
extern afni_history_struct gangc_history[];
extern afni_history_struct rickr_history[];
extern afni_history_struct rwcox_history[];
extern afni_history_struct ziad_history[];


#endif /* _AFNI_HISTORY_HEADER_ */
