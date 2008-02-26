
#include "string.h"
#include "afni_history.h"

static char g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of afni_history):\n"
  "\n"
  "0.0  26 Feb, 2008 [rickr]\n"
  "     - started the joy\n"
};

static char g_version[] = "afni_history version 0.0, 26 February 2008";


/* local structs that don't need to confuse the issue */
typedef struct {
    afni_history_struct * hlist;
    char                * author;
} histpair;

typedef struct {
    /* user options */
    int        verb;

    /* assigned variables */
    int        plen;
    histpair * histpairs;
} global_data;

histpair g_histpairs[NUM_HIST_USERS];  /* will initialize internally */

/* use this for top-level access */
global_data GD = { 1, 0, NULL };

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


/* local protos */
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

int main(int argc, char *argv[])
{
    int rv;

    rv = process_options(argc, argv, &GD);
    if( rv < 0 )        return 1;
    else if( rv > 0 )   return 0;       /* gentle exit */

    /* fill hist people's history_structs */
    if( init_histlist(&GD) ) return 1;

    /* are they valid? */
    if( ! histlists_are_valid(GD.histpairs, GD.plen) ) return 1;

    return 0;
}

/* return -1 on error, +1 for gentle exit, and 0 to continue */
int process_options(int argc, char * argv[], global_data * gd)
{
    int ac, c;

    if( argc < 0 || !argv || !gd ) return -1;

    /* if( argc <= 1 ) { show_help(); return 1; } maybe just run */

    for( ac = 1; ac < argc; ac++ ) {
        if( !strcmp(argv[ac], "-help") ) {
            show_help();
            return 1;
        } else if( !strcmp(argv[ac], "-hist") ) {
            puts(g_history);
            return 1;
        } else if( !strcmp(argv[ac], "-ver") ) {
            puts(g_version);
            return 1;
        } else if( !strcmp(argv[ac], "-verb") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-verb");
            gd->verb = atoi(argv[ac]);
        }
    }

    return 0;
}

int show_help(void)
{
    printf(
        "afni_history:  show AFNI changes per user, dates or levels\n"
          );

    return 0;
}

int histlists_are_valid(histpair * hpairs, int plen)
{
    afni_history_struct * hp;
    int                   errs = 0, c;

    if( GD.verb > 2 )
        fprintf(stderr,"-- checking for %d valid hlists...\n", plen);

    for( c = 0; c < plen; c++ )
        if( ! valid_histlist(hpairs[c].hlist, hpairs[c].author) )
            errs++;

    if( GD.verb > 2 ) fprintf(stderr,"++ number of bad hlists: %d\n", errs);

    return errs;
}

/* print any error messages here */
int valid_histlist(afni_history_struct * hlist, char * author)
{
    int c, len, errs;

    if( !hlist || !author || !*author ) {
        fprintf(stderr,"** valid_histlist: invalid params\n");
        return 0;
    }

    if( GD.verb > 2 ) {
        fprintf(stderr,"-- testing hlist for author ");
        fprintf(stderr,"%s ...\n", author);
    }

    len = hlist_len(hlist);
    if( hlist_len <= 0 ) return 1;

    errs = 0;
    for( c = 0; c < len; c++ )
        if( ! valid_histstruct(&hlist[c], author) ) {
            fprintf(stderr,"** bad hstruct[%d]\n", c);
            errs++;
        }

    if( ! hlist_is_sorted(hlist) ) errs++;

    if( errs )
        fprintf(stderr,"** author %s, found %d bad structs\n", author, errs);
    else if ( GD.verb > 1 )
        fprintf(stderr,"++ author %s, %d structs, OK\n", author, len);

    if( errs ) return 0;
    else       return 1;
}

/* rcr - finish this, check that the dates are sorted (one way or the other) */
int hlist_is_sorted(afni_history_struct * hlist)
{
    return 1;
}

int valid_histstruct(afni_history_struct * hstr, char * author)
{
    int errs = 0;

    if( !hstr || !author ) {
        if( GD.verb > 1 ) fprintf(stderr,"** VHS: bad params\n");
        return 0;
    }

    if( ! INT_IN_RANGE(hstr->dd, 1, 31) ) {
        fprintf(stderr,"** invalid day of month: %d\n", hstr->dd);
        errs++;
    }

    if( ! INT_IN_RANGE(hstr->mm, 1, 12) ) {
        fprintf(stderr,"** invalid day of month: %d\n", hstr->mm);
        errs++;
    }

    if( ! INT_IN_RANGE(hstr->yyyy, 2008, 2050) ) {
        fprintf(stderr,"** invalid year: %d\n", hstr->yyyy);
        errs++;
    }

    if( ! hstr->author || strcmp(author, hstr->author) ) {
        fprintf(stderr,"** author mis-match: %s != %s\n",
                author, CHECK_NULL_STR(hstr->author));
        errs++;
    }

    if( ! valid_program(hstr->program) ) {
        fprintf(stderr,"** invalid program: %s\n",
                CHECK_NULL_STR(hstr->program));
        errs++;
    }

    if( ! INT_IN_RANGE(hstr->rank, MIN_PROG_RANK, MAX_PROG_RANK) ) {
        fprintf(stderr,"** invalid rank: %d\n", hstr->rank);
        errs++;
    }

    if( ! valid_dstring(hstr->desc, MAX_LINE_CHARS) ) {
        fprintf(stderr,"** invalid desc: %s\n", CHECK_NULL_STR(hstr->desc));
        errs++;
    }

    if( ! valid_dstring(hstr->verbtext, MAX_LINE_CHARS) ) {
        fprintf(stderr,"** invalid verbtext: %s\n",
                CHECK_NULL_STR(hstr->verbtext));
        errs++;
    }

    if( errs ) return 0;
    else       return 1;
}

/* max_line_chars is the most that can pass w/out a \n,
 * except for one more at the end
 */
int valid_dstring(char * str, int max_line_len)
{
    int c, past, len;

    if( !str || !*str ) return 1;       /* okay to be empty */

    len = strlen(str);

    for( c = 1, past = 1; c < len; c++ ) {
        if( str[c] == '\n' ) { past = 0; continue; }

        if( past >= max_line_len) {
            if( past == max_line_len && !str[c+1] ) break;  /* this is okay */

            /* otherwise, whine and fail */
            fprintf(stderr,"** string has long lines (max should be %d)\n",
                           max_line_len);

            if( max_line_len > 40 )
                fprintf(stderr,"-- okay up until '%40s'\n", str+c-40);

            return 0;
        }
    }

    return 1;
}

/* consider checking a valid list */
int valid_program(char * prog)
{
    if( ! prog ) return 0;
    else         return 1;
}

int hlist_len(afni_history_struct * hlist)
{
    int len = 0;

    if( !hlist ) return 0;
    
    for( len = 0; hlist[len].dd != 99; len++ )
        ;

    if( GD.verb > 1 ) fprintf(stderr,"-- hlist has %d entries\n", len);

    return len;
}

/* return 1 on some problem, else fill histpairs and set len */
int init_histlist( global_data * gd )
{
    histpair * plist;
    int        c;

    if( !gd ) {
        fprintf(stderr,"** init_histlist: bad param\n");
        return 1;
    }

    if( gd->verb > 2 ) fprintf(stderr,"++ init histlist...\n");

    gd->histpairs = g_histpairs;        /* point to global list */

    plist = gd->histpairs;              /* for convenience */
    c = 0;
    plist[c].hlist = bpittman_history;  plist[c++].author = BGP;
    plist[c].hlist = christip_history;  plist[c++].author = PPC;
    plist[c].hlist = dglen_history;     plist[c++].author = DRG;
    plist[c].hlist = gangc_history;     plist[c++].author = GC;
    plist[c].hlist = rickr_history;     plist[c++].author = RCR;
    plist[c].hlist = rwcox_history;     plist[c++].author = RWC;
    plist[c].hlist = ziad_history;      plist[c++].author = ZSS;
    gd->plen = c;

    if( c > NUM_HIST_USERS ) {
        fprintf(stderr,"** NUM_HIST_USERS too small for %d users\n",c);
        return 1;
    }

    if( gd->verb > 1 )
        fprintf(stderr,"-- histlist initialized with %d lists\n", c);

    return 0;
}

