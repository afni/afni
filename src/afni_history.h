#ifndef _AFNI_HISTORY_HEADER_
#define _AFNI_HISTORY_HEADER_

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

#define Jan  1
#define Feb  2
#define Mar  3
#define Apr  4
#define May  5
#define Jun  6
#define Jul  7
#define Aug  8
#define Sep  9
#define Oct 10
#define Nov 11
#define Dec 12

#define RWC "RW Cox"
#define ZSS "ZS Saad"
#define DRG "DR Glen"
#define RCR "RC Reynolds"
#define GC  "G Chen"         /* no middle initial! */
#define PPC "PP Chrisidis"

#define MICRO  1  /* rank for changes that users don't see */
#define MINOR  2  /* rank for small changes that users see */
#define MAJOR  3  /* rank for large changes that users see */
#define SUPER  4  /* rank for changes that users must know */

#define micro  1
#define minor  2
#define major  3
#define super  4

typedef struct {
  short dd;          /* Day: 1..31 */
  short mm;          /* Month: 1..12 (use a macro) */
  short yyyy;        /* Year: 2008..9999 */
  char *author;      /* use a macro */
  char *progname;    /* will be checked against allowed names */
  short commentrank; /* use a macro */
  char *oneliner;    /* one line description */
  char *verbtext;    /* can be NULL, but shouldn't be */
} g_afni_history_struct;


#endif /* _AFNI_HISTORY_HEADER_ */
