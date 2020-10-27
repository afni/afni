
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "afni_history.h"

static char g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of afni_history):\n"
  "\n"
  "1.0  27 Feb 2008 [rickr] : initial release\n"
  "\n"
  "1.1  28 Feb 2008 [rickr] : added -list_authors option\n"
  "1.2  29 Feb 2008 [rwcox] : separated dates in html output\n"
  "1.3  29 Feb 2008 [rickr]\n"
  "     - added -type option, to restrict output to a specific type\n"
  "     - added a string to explain the output level\n"
  "     - added TYPE field, and a new SUPERDUPER level\n"
  "1.4  19 May 2008 [rickr]\n"
  "     - added MY_INT_COMPARE macro for comparison speed-up\n"
  "     - in compare: if strings are not set, ignore rather than claim equal\n"
  "1.5  25 Jun 2008 [rickr] : added -past_entries option\n"
  "1.6  14 Jul 2008 [rickr] : single integer arg is used as -past_entries\n"
  "1.7  29 Mar 2010 [RWCox] : add '-dline' option\n"
  "1.8   3 Jun 2010 [rickr] : added TYPE_ENHANCE\n"
  "1.9  19 Jul 2010 [rickr] : added -check_date option\n"
  "1.10 30 Nov 2010 [rickr] : added -final_sort_by_prog option\n"
  "1.11 29 Mar 2017 [rickr] : added justin: JKR\n"
  "1.12 28 Jul 2020 [rickr] : added laurenpd: PDL\n"
  "1.13 25 Aug 2020 [rickr] : added TYPE_REMOVE, TYPE_REINSTATE\n"
  "1.14  2 Sep 2020 [rickr] : added -show_type, -show_type_names\n"
};

static char g_version[] = "afni_history version 1.14, 2 Sep 2020";

static char * g_author_list[] = {
    "rwcox",    "RWC",  RWC,
    "ziad",     "ZSS",  ZSS,
    "dglen",    "DRG",  DRG,
    "rickr",    "RCR",  RCR,
    "gangc",    "GC",   GC,
    "christip", "PPC",  PPC,
    "bpittman", "BGP",  BGP,
    "ptaylor",  "PT" ,  PT,
    "discoraj", "JKR",  JKR,
    "laurenpd", "PDL",  PDL
};

/* ======================================================================= */
/* field names and defines, for use with the -show_field option
 *
 * This is basically a list of fields in the afni_history_struct, in case
 * one wants to print out just the individual fields, rather than each
 * entire struct.
 *
 * The "date"/DATE entry is a convenience to combine YEAR/MONTH/DAY
 * (to make chronological == alphabetical).
 *
 * This string order must match the non-negative FIELD_* #define's, which
 * follow.  The #define's are here (instead of afni_history.h) to keep
 * these lists together.                                                   */

static char * g_show_fields[] = {
    "all", "firstline",         /* the rest are (mostly) struct fields */
    "day", "month", "year", "date",
    "author", "program", "level", "type",
    "desc", "verbtext"
};

/* field type values             (for show_field)               */
#define FIELD_INVALID     -1  /* bad, naughty type             */
#define FIELD_ALL          0  /* show all fields               */
#define FIELD_FIRST_LINE   1  /* show the standard first line */
#define FIELD_DAY          2  /* day of month               */
#define FIELD_MONTH        3  /* month of year             */
#define FIELD_YEAR         4  /* year of calendar system  */
#define FIELD_DATE         5  /* year, month, day        */
#define FIELD_AUTHOR       6  /* author                 */
#define FIELD_PROGRAM      7  /* program               */
#define FIELD_LEVEL        8  /* level                */
#define FIELD_TYPE         9  /* type                */
#define FIELD_DESC        10  /* description        */
#define FIELD_VERB        11  /* verbtext          */
#define MAX_FIELD_VAL     12  /* max field index  */

/* end field names and defines                                             */
/* ======================================================================= */


histpair g_histpairs[NUM_HIST_USERS];  /* will initialize internally */

/* use this for top-level access */
global_data GD;

int main(int argc, char *argv[])
{
    global_data * gd = &GD;
    hist_type  ** hlist = NULL;
    int           rv, hlen = 0;

    /* check -show_field entries */
    if ( quick_field_test() ) return 1;

    rv = process_options(argc, argv, gd);
    if( rv < 0 )        return 1;
    else if( rv > 0 )   return 0;       /* gentle exit */

    /* fill hist people's history_structs */
    if( init_histlist(gd) ) return 1;

    /* are they valid? */
    if( ! histlists_are_valid(gd->histpairs, gd->plen) ) return 1;

    /* restrict results based on options    19 Jul 2010 [rickr] */
    rv = restrict_results(gd, &hlist, &hlen);
    if( rv ) return rv;

    /* either check the date or return history info */
    if( gd->check_date ) rv = check_date(gd, hlist, hlen);
    else                 rv = show_history(gd, hlist, hlen);

    if( hlist ) free(hlist);

    return rv;
}

/* return -1 on error, +1 for gentle exit, and 0 to continue */
int process_options(int argc, char * argv[], global_data * gd)
{
    int ac;

    if( argc < 0 || !argv || !gd ) return -1;

    memset(gd, 0, sizeof(global_data));
    gd->sort_dir = 1;   /* can reverse this */
    gd->verb = 1;
    gd->cd_day = NULL;  gd->cd_month = NULL;  gd->cd_year = NULL;


    /* if( argc <= 1 ) { show_help(); return 1; } maybe just run */

    for( ac = 1; ac < argc; ac++ ) {
        /* early quitters, then the rest alphabetically */
        if( !strcmp(argv[ac], "-help") ) {
            show_help();
            return 1;
        } else if( !strcmp(argv[ac], "-hist") ) {
            puts(g_history);
            return 1;
        } else if( !strcmp(argv[ac], "-list_authors") ) {
            show_author_list();
            return 1;
        } else if( !strcmp(argv[ac], "-show_field_names" ) ) {
            show_valid_fields();
            return 1;
        } else if( !strcmp(argv[ac], "-list_types") ) {
            show_valid_types();
            return 1;
        } else if( !strcmp(argv[ac], "-ver") ) {
            puts(g_version);
            return 1;
        /* alphabetical continuation */
        } else if( !strcmp(argv[ac], "-author" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-author", "AUTHOR");
            gd->author = convert_author(argv[ac]);
        } else if( !strcmp(argv[ac], "-check_date" ) ) {
            if( ac+3 >= argc ) {
                fprintf(stderr,"** -check_date: requiers DD MM YYYY params\n");
                return -1;
            }
            gd->check_date = 1;
            gd->cd_day   = argv[++ac];
            gd->cd_month = argv[++ac];
            gd->cd_year  = argv[++ac];
        } else if( !strcmp(argv[ac], "-dline" ) ) {
            gd->html = 0; gd->dline = 1;
        } else if( !strcmp(argv[ac], "-html" ) ) {
            gd->html = 1; gd->dline = 0;
        } else if( !strcmp(argv[ac], "-level" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-level", "LEVEL");
            gd->level = atol(argv[ac]);
        } else if( !strcmp(argv[ac], "-min_level" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-min_level", "LEVEL");
            gd->min_level = atol(argv[ac]);
        } else if( !strcmp(argv[ac], "-past_days" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-past_days", "DAYS");
            gd->past_days = atol(argv[ac]);
            if( gd->past_days < 0 ) {
                fprintf(stderr,"** DAYS should be at least zero\n");
                return 1;
            }
        } else if( !strcmp(argv[ac], "-past_entries" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-past_entries", "ENTRIES");
            gd->past_entries = atol(argv[ac]);
            if( gd->past_entries < 1 ) {
                fprintf(stderr,"** ENTRIES should be at least one\n");
                return 1;
            }
        } else if( !strcmp(argv[ac], "-past_months" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-past_months", "MONTHS");
            gd->past_months = atol(argv[ac]);
            if( gd->past_months < 0 ) {
                fprintf(stderr,"** MONTHS should be at least zero\n");
                return 1;
            }
        } else if( !strcmp(argv[ac], "-past_years" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-past_years", "YEARS");
            gd->past_years = atol(argv[ac]);
            if( gd->past_years < 0 ) {
                fprintf(stderr,"** YEARS should be at least zero\n");
                return 1;
            }
        } else if( !strcmp(argv[ac], "-program" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-program", "PROGRAM");
            gd->program = argv[ac];
        } else if( !strcmp(argv[ac], "-reverse" ) ) {
            gd->sort_dir = -1;
        } else if( !strcmp(argv[ac], "-final_sort_by_prog" ) ) {
            gd->final_sort_by_prog = 1;
        } else if( !strcmp(argv[ac], "-show_field" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-show_field", "FIELD");
            gd->show_field = field_name2index(argv[ac]);
            if( gd->show_field < 0 ) {
                fprintf(stderr,"** invalid show field '%s'\n", argv[ac]);
                fprintf(stderr,"   see 'afni_history -show_field_names'\n");
                return -1;
            }
        } else if( !strcmp(argv[ac], "-type" ) ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-type", "TYPE");
            if( isdigit(*argv[ac]) )
                gd->type = atol(argv[ac]);              /* integer */
            else
                gd->type = type_string2type(argv[ac]);  /* string */
            if( ! INT_IN_RANGE(gd->type, 0, MAX_TYPE_VAL) ) {
                fprintf(stderr,"** invalid type (number or string): %s\n",
                        argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-verb") ) {
            ac++;
            CHECK_NEXT_OPT2(ac, argc, "-verb", "LEVEL");
            gd->verb = atoi(argv[ac]);
        /* last possibility, check for shortcut like "afni_history 3" */
        } else if( (ac == argc-1) && isdigit(argv[ac][0]) ) {
            gd->past_entries = atol(argv[ac]);
            if( gd->past_entries < 1 ) {
                fprintf(stderr,"** invalid last option: '%s'\n", argv[ac]);
                return 1;
            }
        } else {
            fprintf(stderr,"** invalid option: '%s'\n", argv[ac]);
            return -1;
        }
    }

    if( gd->verb > 2 ) disp_global_data("-- options read: ", gd);

    /* save options for later, in case we want to print them out */
    gd->argc = argc;
    gd->argv = argv;

    return 0;
}

char * convert_author(char * name)
{
    if( !name ) return NULL;

    if( !strcmp(name, "RWC") )      return RWC;
    if( !strcmp(name, "rwcox") )    return RWC;
    if( !strcmp(name, "bob") )      return RWC;
    if( !strcmp(name, "Bob") )      return RWC;

    if( !strcmp(name, "ZSS") )      return ZSS;
    if( !strcmp(name, "Ziad") )     return ZSS;
    if( !strcmp(name, "ziad") )     return ZSS;

    if( !strcmp(name, "PT") )       return PT;
    if( !strcmp(name, "Paul") )     return PT;
    if( !strcmp(name, "PTaylor") )  return PT;
    if( !strcmp(name, "ptaylor") )  return PT;

    if( !strcmp(name, "DRG") )      return DRG;
    if( !strcmp(name, "dglen") )    return DRG;
    if( !strcmp(name, "Daniel") )   return DRG;
    if( !strcmp(name, "daniel") )   return DRG;

    if( !strcmp(name, "RCR") )      return RCR;
    if( !strcmp(name, "rickr") )    return RCR;
    if( !strcmp(name, "Rick") )     return RCR;
    if( !strcmp(name, "rick") )     return RCR;

    if( !strcmp(name, "GC") )       return GC;
    if( !strcmp(name, "gangc") )    return GC;
    if( !strcmp(name, "Gang") )     return GC;
    if( !strcmp(name, "gang") )     return GC;

    if( !strcmp(name, "PPC") )      return PPC;
    if( !strcmp(name, "christip") ) return PPC;
    if( !strcmp(name, "Peggy") )    return PPC;
    if( !strcmp(name, "peggy") )    return PPC;

    if( !strcmp(name, "BGP") )      return BGP;
    if( !strcmp(name, "bpittman") ) return BGP;
    if( !strcmp(name, "pittmanb") ) return BGP;
    if( !strcmp(name, "Brian") )    return BGP;
    if( !strcmp(name, "brian") )    return BGP;

    if( !strcmp(name, "JKR") )      return JKR;
    if( !strcmp(name, "discoraj") ) return JKR;
    if( !strcmp(name, "justin") )   return JKR;
    if( !strcmp(name, "Justin") )   return JKR;
    if( !strcmp(name, "rajendrajk"))return JKR;

    if( !strcmp(name, "PDL") )      return PDL;
    if( !strcmp(name, "laurenpd") ) return PDL;
    if( !strcmp(name, "Peter") )    return PDL;
    if( !strcmp(name, "peter") )    return PDL;

    return name;   /* give up and stick with what we have */
}

/* if author is set, results are for only that author
 * otherwise, create a combined list, return it and its length
 */
int restrict_results(global_data * gd, hist_type *** H, int * Hlen)
{
    hist_type ** hlist = NULL, * h1 = NULL;
    int          c, rv = 0, hlen = 0;

    if( gd->author ) {
        if( gd->verb > 3 )
            fprintf(stderr,"-- searching for history of '%s'\n", gd->author);

        for( c = 0; c < gd->plen; c++ )
            if( ! strcmp(gd->histpairs[c].author, gd->author) )
                break;

        if( c >= gd->plen ) {
            fprintf(stderr,"** could not find history for author '%s'\n",
                            gd->author);
            return 1;
        }

        if( gd->verb > 1 )
            fprintf(stderr,"++ found history for author '%s'\n", gd->author);

        /* allocate hlist, and set pointers */
        h1 = gd->histpairs[c].hlist;
        if( add_to_hlist(&hlist, h1, hlist_len(h1), &hlen) ) return 1;
    } else {
        if( gd->verb > 3 ) fprintf(stderr,"-- compiling composite history\n");

        /* no author, so create hlist as a composite list */

        for( c = 0; c < gd->plen; c++ ) {
            h1 = gd->histpairs[c].hlist;
            if( gd->verb > 3 )
                fprintf(stderr,"\n++ adding history for author '%s'\n",
                         gd->histpairs[c].author);
            if( add_to_hlist(&hlist, h1, hlist_len(h1), &hlen) ) {
                if( hlist ) free(hlist);
                return 1;
            }
        }
    }

    /* maybe restrict the list to a specific program */
    if( !rv && gd->program )
        rv = restrict_by_program(gd, &hlist, &hlen);

    /* maybe restrict the list to a level or a set of levels */
    if( !rv && (gd->level || gd->min_level) )
        rv = restrict_by_level(gd, &hlist, &hlen);

    /* maybe restrict the list to a level or a set of levels */
    if( !rv && gd->type )
        rv = restrict_by_type(gd, &hlist, &hlen);

    /* sort by date, author, level and program */
    if( !rv && hlen > 0 )
        qsort(hlist, hlen, sizeof(hist_type *), compare_hlist);

    /* maybe restrict by the date */
    if( !rv && hlen>0 && (gd->past_days || gd->past_months || gd->past_years) )
        rv = restrict_by_date(gd, &hlist, &hlen);

    /* to show the past_entries, restrict the indices based on sort_dir */
    /*                                              25 Jun 2008 [rickr] */
    if( gd->past_entries > 0 && gd->past_entries < hlen ) {
        int first = 0;
        if( gd->sort_dir == 1 ) first = hlen - gd->past_entries;

        if( gd->verb > 1 )
            fprintf(stderr,"++ restricting to entires from %d to %d\n",
                    hlen, gd->past_entries);

        if( restrict_hlist(&hlist, first, hlen, gd->past_entries) ) return 1;

        hlen = gd->past_entries;
    }

    /* maybe resort by program name first */
    if( !rv && hlen > 0 && gd->final_sort_by_prog ) {
        gd->sort_by_prog = 1;
        qsort(hlist, hlen, sizeof(hist_type *), compare_hlist);
        gd->sort_by_prog = 0;
    }

    *H = hlist;
    *Hlen = hlen;

    return 0;
}

/* check whether there is an entry as recent as the user date
 *
 * if so, return 0  (good)
 * else,  return 1  (bad)
 */
int check_date(global_data * gd, hist_type ** hlist, int len)
{
    hist_type * hh = NULL;
    int yy, mm, dd;

    if( gd->verb > 1 )
        fprintf(stderr,"\n-- checking date, length %d...\n\n",len);

    if( !hlist || !*hlist || len <= 0 ) {
        if( gd->verb > 0 )
            printf("no history to check, given the restrictions\n");
        return 1;
    }

    if( set_dates(gd, &dd, &mm, &yy) ) return 1;

    if( gd->sort_dir ) hh = hlist[len-1];       /* last is most recent */
    else               hh = hlist[0];

    if( gd->verb > 1 ) {
        fprintf(stderr,"-- comparing date against hist entry:\n");
        show_hist_entry(hh, stderr, FIELD_ALL);
    }

    if(  hh->yyyy  < yy || 
        (hh->yyyy == yy && hh->mm  < mm ) ||
        (hh->yyyy == yy && hh->mm == mm && hh->dd < dd ) ) {
        if( gd->verb )
            printf("** too old: afni_history older than: %02d %s %04d\n"
                   "            most recent entry is:    %02d %s %04d\n",
                dd, mm2month(mm), yy, hh->dd, mm2month(hh->mm), hh->yyyy);
        return 1;
    }

    if( gd->verb )
        printf("-- is current: afni_history as new as: %02d %s %04d\n"
               "               most recent entry is:   %02d %s %04d\n",
            dd, mm2month(mm), yy, hh->dd, mm2month(hh->mm), hh->yyyy);

    return 0;
}

/* extract and validate DD/MM/YY values from the user options
 * return 0 if they seem okay, 1 otherwise */
int set_dates(global_data * gd, int * dd, int * mm, int * yy )
{
    int rv = 0;

    if( !gd->cd_day || !gd->cd_month || !gd->cd_year ) {
        fprintf(stderr,"** set_dates: dd/mm/yy not set: %p, %p, %p\n",
                gd->cd_day, gd->cd_month, gd->cd_year);
        return 1;
    }

    *dd = atoi(gd->cd_day);
    *mm = month2mm(gd->cd_month); /* if month is a bad string, get as int */
    if(*mm <= 0 || *mm > 12) *mm = atoi(gd->cd_month);
    *yy = atoi(gd->cd_year);

    if( gd->verb > 1 )
        fprintf(stderr,"\n-- set dates, dd/mm/yy = %d/%d/%d\n", *dd, *mm, *yy);

    if( *dd < 0 || *dd > 31   ) {
        fprintf(stderr,"** bad day:   %d\n", *dd);
        rv = 1;
    }
    if( *mm < 0 || *mm > 12   ) {
        fprintf(stderr,"** bad month: %d\n", *mm);
        rv = 1;
    }
    if( *yy < 0 || *yy > 3000 ) {
        fprintf(stderr,"** bad year:  %d\n", *yy);
        rv = 1;
    }

    return rv;
}

/* hlist is an array of structure pointers */
int show_history(global_data * gd, hist_type ** hlist, int len)
{
    int c, first = 0;   /* use first for truncating on past_entries */

    if( gd->verb > 1 )
        fprintf(stderr,"\n-- showing history, length %d...\n\n",len);

    if( gd->html ) show_html_header(gd, stdout, gd->min_level);

    if( !hlist || !*hlist || len <= 0 ) {
        if( gd->verb > 0 )
            printf("no history to show, given the restrictions\n");
        if( gd->html ) show_html_footer(stdout);
        return 1;
    }

#if 0
    /* to show the past_entries, restrict the indices based on sort_dir */
    /*                                              25 Jun 2008 [rickr] */
    if( gd->past_entries > 0 && gd->past_entries < len ) {
        if( gd->sort_dir == 1 ) first = len - gd->past_entries;
        else                    len   = gd->past_entries;
        if( gd->verb > 1 )
            fprintf(stderr,"++ restricting to entires from %d to %d\n",
                    first, len-1);
    }
#endif

    if( gd->verb > 0 ) {
       if( gd->sort_dir == 1 )
           printf("  ----  log of AFNI updates (most recent last)  ----\n\n");
       else
           printf("  ----  log of AFNI updates (most recent first)  ----\n\n");
    }

    for( c = first; c < len; c++ ) {
        if( c > 0 && compare_hist_dates(hlist[c],hlist[c-1]) ){
          if( gd->html )
            show_html_separator(stdout) ;  /* RWC */
          else if( gd->dline )
            show_dline_separator(stdout) ;  /* RWC */
        }

        show_hist_entry(hlist[c], stdout, gd->show_field);
    }

    if( gd->html ) show_html_footer(stdout);

    return 0;
}

/* special fields are FIELD_FIRST_LINE and other (field > FIELD_ALL),
 * else show as FIELD_ALL */
int show_hist_entry(hist_type * hp, FILE * fp, int field)
{
    /* if the user wants only 1 field, show it and return            */
    /* (individual fields come after FIELD_ALL and FIELD_FIRST_LINE) */
    if( field > FIELD_FIRST_LINE && field <= MAX_FIELD_VAL ) {
       if( field == FIELD_DAY )     fprintf(fp, "%02d\n", hp->dd);
       if( field == FIELD_MONTH )   fprintf(fp, "%s\n", mm2month(hp->mm));
       if( field == FIELD_YEAR )    fprintf(fp, "%04d\n", hp->yyyy);

       /* DATE is a special field - show a full date */
       if( field == FIELD_DATE )
          fprintf(fp, "%04d %02d %02d\n",
                  hp->yyyy, hp->mm, hp->dd);

       if( field == FIELD_AUTHOR )  fprintf(fp, "%s\n", hp->author);
       if( field == FIELD_PROGRAM ) fprintf(fp, "%s\n", hp->program);
       if( field == FIELD_LEVEL  )  fprintf(fp, "%d (%s)\n",
                                            hp->level, level_string(hp->level));
       if( field == FIELD_TYPE )    fprintf(fp, "%d (%s)\n",
                                            hp->type, type_string(hp->type));
       if( field == FIELD_DESC )    fprintf(fp, "%s\n", hp->desc);
       if( field == FIELD_VERB )    fprintf(fp, "%s\n", hp->verbtext);

       /* we are done with the single entries */
       return 0;
    }

    /* at this point, we want just the first line, or the entire struct */

    /* print the first line, either way */
    fprintf(fp, "%02d %s %04d, %s, %s, level %d (%s), type %d (%s)\n",
            hp->dd, mm2month(hp->mm), hp->yyyy,
            hp->author, hp->program, hp->level, level_string(hp->level),
            hp->type, type_string(hp->type));

    /* if the user wants only the first line, we are done */
    if( field == FIELD_FIRST_LINE )
        return 0;


    /* otherwise, print the rest of the struct */

    fprintf(fp, "    %s\n", hp->desc);
    if( hp->desc[strlen(hp->desc)-1] != '\n' ) fputc('\n', fp);

    if( hp->verbtext )
        show_wrapping_line(hp->verbtext, "    ", 4, fp);

    return 0;
}

/* apply line indent per line, if we exceed MAX_LINE_CHARS, wrap */
int show_wrapping_line(char * str, char * prefix, int indent, FILE * fp)
{
    int c, cline, len;

    if( !str ) return 0;

    if( prefix ) fputs(prefix, fp);

    len = strlen(str);
    if( len < 2 ) return 1;

    if( str[len-1] == '\n' ) len--;     /* ignore trailing newline */

    cline = 0;
    for( c = 0; c < len; c++ ) {
        if( str[c] == '\n' ) {          /* print newline and indent */
            fputc('\n', fp);
            fprintf(fp, "%*s", indent, "");
            cline = 0;
            continue;
        } else if ( cline > MAX_LINE_CHARS ) {  /* fix, and continue */
            fputc('\n', fp);
            fprintf(fp, "%*s", indent, "");
            cline = 0;
        }
        fputc(str[c], fp);
        cline++;
    }

    fprintf(fp,"\n\n");

    return 0;
}

int show_html_header(global_data * gd, FILE * fp, int min_level)
{
    fprintf(fp, "<html><head>\n"
                "<title>AFNI HISTORY</title></head>\n"
                "<body><center><h1>AFNI HISTORY</h1>\n");
    
    if( min_level > 0 )
        fprintf(fp, "<h2>level %d and higher</h2>\n", min_level);
    else
        fprintf(fp, "<h2>all levels</h2>\n");

    fprintf(fp,"</center><h4><pre>\n"
        "The levels of importance go from 1 to 5, with meanings:\n"
        "       1 - users would not care\n"
        "       2 - of little importance, though some users might care\n"
        "       3 - fairly important\n"
        "       4 - a big change or new program\n"
        "       5 - IMPORTANT: we expect users to know\n"
        "\n</h4>\n");

    fprintf(fp, "</center><hr />\n<pre>\n");
    fprintf(fp, "<b>generated via the command : ");
    show_command(fp, gd->argc, gd->argv);
    fprintf(fp, "</b><hr width='75%%' align='left'/>\n");

    return 0;
}

int show_command(FILE * fp, int argc, char ** argv)
{
    int c;

    for( c = 0; c < argc; c++ )
        fprintf(fp, "%s ", argv[c]);
    fputc('\n', fp);

    return 0;
}

int show_html_footer(FILE * fp)
{
    fprintf(fp, "</pre>\n"
                "<hr /><b>auto-generated by afni_history on %s</b>\n",
                __DATE__);
    fprintf(fp, "</body></html>\n");
    
    return 0;
}

int show_html_separator(FILE * fp)  /* RWC */
{
    fprintf(fp, "<hr width='50%%' align='left' />\n") ;

    return 0 ;
}

int show_dline_separator(FILE * fp)  /* RWC */
{
    fprintf(fp,
    "----------------------------------------------------------------------\n\n");
    return 0 ;
}


/* convert a numerical month to a readable word */
char * mm2month(int mm)
{
    if( mm <=  0 ) return "ILLEGAL";
    if( mm ==  1 ) return "Jan";
    if( mm ==  2 ) return "Feb";
    if( mm ==  3 ) return "Mar";
    if( mm ==  4 ) return "Apr";
    if( mm ==  5 ) return "May";
    if( mm ==  6 ) return "Jun";
    if( mm ==  7 ) return "Jul";
    if( mm ==  8 ) return "Aug";
    if( mm ==  9 ) return "Sep";
    if( mm == 10 ) return "Oct";
    if( mm == 11 ) return "Nov";
    if( mm == 12 ) return "Dec";

    return "ILLEGAL";
}

/* convert a numerical month to a readable word */
int month2mm(char * month)
{
    char * copy;
    int    cc, rv = 0, len;

    if( !month || !*month ) return 0;

    /* make a lower case copy (no libraries used so far, keep it that way) */
    len = strlen(month);
    copy = (char *)malloc((len+1)*sizeof(char));
    strcpy(copy, month);
    if( !copy ) {
        fprintf(stderr,"** month2mm: failed to copy '%s'\n", month);
        return 0;
    }
    for(cc = 0; cc < strlen(copy); cc++) copy[cc] = tolower(copy[cc]);

    if     ( ! strncmp(copy, "jan", 3) ) rv = 1;
    else if( ! strncmp(copy, "feb", 3) ) rv = 2;
    else if( ! strncmp(copy, "mar", 3) ) rv = 3;
    else if( ! strncmp(copy, "apr", 3) ) rv = 4;
    else if( ! strncmp(copy, "may", 3) ) rv = 5;
    else if( ! strncmp(copy, "jun", 3) ) rv = 6;
    else if( ! strncmp(copy, "jul", 3) ) rv = 7;
    else if( ! strncmp(copy, "aug", 3) ) rv = 8;
    else if( ! strncmp(copy, "sep", 3) ) rv = 9;
    else if( ! strncmp(copy, "oct", 3) ) rv = 10;
    else if( ! strncmp(copy, "nov", 3) ) rv = 11;
    else if( ! strncmp(copy, "dec", 3) ) rv = 12;

    if(copy) free(copy);

    return rv;
}

/* perhaps we want to remove everything that is not so recent */
int restrict_by_date(global_data * gd, hist_type *** hlist, int * len)
{
    hist_type ** hptr, hstr, *hsptr;
    struct tm  * loctime;               /* to get todays date */
    time_t       tsec;
    long long    offset;
    int          c, gap, nfound, dcount;

    if( !hlist || !*hlist || !len || *len <= 0 ) {
        if( gd->verb > 1 ) fprintf(stderr,"** bad, evil restriction\n");
        return 1;
    }

    /* be sure we have exactly 1 operation here */
    dcount = 0;
    if( gd->past_days   > 0 ) dcount++;
    if( gd->past_months > 0 ) dcount++;
    if( gd->past_years  > 0 ) dcount++;
    if( dcount < 1 ) {
        if(gd->verb > 1) fprintf(stderr,"-- no date restriction to enforce\n");
        return 0;
    } else if ( dcount > 1 ) {
        fprintf(stderr,"** will not apply multiple date restrictions\n");
        return 1;
    }

    /* get time in seconds, and subtract the offset to get a comparison date */
    offset = 24 * 3600;                         /* seconds in a day */
    if( gd->past_days   > 0 ) offset *= gd->past_days;
    if( gd->past_months > 0 ) offset *= (30.5 * gd->past_months);
    if( gd->past_years  > 0 ) offset *= (365.25 * gd->past_years);
    tsec = time(NULL);          
    if( gd->verb > 1 )
        fprintf(stderr,"++ converting current date: %s\n",ctime(&tsec));
    tsec -= offset;
    loctime = localtime(&tsec);
    if( gd->verb > 1 )
        fprintf(stderr,"   to cutoff date: %s\n",ctime(&tsec));

    memset(&hstr, 0, sizeof(hstr));          /* clear and init */
    hstr.dd   =   loctime->tm_mday;          /* 1..31 */
    hstr.mm   =   loctime->tm_mon + 1;       /* from 0..11 */
    hstr.yyyy =   loctime->tm_year + 1900;   /* was offset from 1900 */
    hsptr = &hstr;  /* needed for compar() */

    if( gd->verb > 1 )
        fprintf(stderr,"++ applying cutoff date of dd/mm/yyyy %02d/%02d/%04d\n",
                hstr.dd, hstr.mm, hstr.yyyy);

    /* find a cutoff, and move everything to the top */
    nfound = 0;
    hptr = *hlist;
    for( c = 0; c < *len; c++ ) {
        /* equality is good in the reverse direction */
        if( gd->sort_dir < 0 && compare_hlist(&hsptr, hptr+c) == 0 ) continue;
        if( compare_hlist(&hsptr, hptr+c) <= 0 ) break;
    }

    /* results will be backward, depending on sort */
    if( gd->sort_dir == 1 ) {
        nfound = *len - c;
        gap = c;
        if( nfound > 0 && nfound < *len ) {
            if(gd->verb > 2)
                fprintf(stderr,"-- shifting histlist up by %d\n",gap);
            for( c = 0; c < nfound; c++ )  /* shift lower entries to the top */
                hptr[c] = hptr[c+gap];
        }
    } else
        nfound = c;     /* no shift requred */

    if( nfound == 0 ) {         /* death by 'levels' */
        if(gd->verb>0) fprintf(stderr,"-- no history for date restriction\n");
        free(*hlist);
        *hlist = NULL;
        return 1;
    }

    /* restrict the list */
    if( nfound < *len ) {
        *hlist = (hist_type **)realloc(*hlist, nfound*sizeof(hist_type *));
        if( !*hlist ) {
            fprintf(stderr,"** failed realloc of list ptrs to len %d\n",nfound);
            return 1;
        }
    }

    if( gd->verb > 1 )
        fprintf(stderr,"++ date restriction drops list length from %d to %d\n",
                *len, nfound);

    *len = nfound;

    return 0;
}

/* perhaps we want to remove everything that is not our 'type' */
int restrict_by_type(global_data * gd, hist_type *** hlist, int * len)
{
    hist_type ** hptr;                  /* just for typing */
    int          c, nfound;

    if( !hlist || !*hlist || !len || *len <= 0 ) {
        if( gd->verb > 1 ) fprintf(stderr,"** RBT: bad restriction\n");
        return 1;
    }

    if( !gd->type ) {
        if( gd->verb > 3 ) fprintf(stderr, "-- no type to restrict to\n");
        return 0;
    }

    /* move good pointers to beginning of list, and count num found */
    nfound = 0;
    hptr = *hlist;
    for( c = 0; c < *len; c++) {
        if( gd->type == hptr[c]->type ) {
            if( c > nfound ) hptr[nfound] = hptr[c];  /* move it up */
            nfound++;
        }
    }

    if( nfound == 0 ) {         /* death by 'type' */
        if(gd->verb>0) fprintf(stderr,"-- no history for type restriction\n");
        free(*hlist);
        *hlist = NULL;
        return 1;
    }

    /* restrict the list */
    *hlist = (hist_type **)realloc(*hlist, nfound*sizeof(hist_type *));
    if( !*hlist ) {
        fprintf(stderr,"** failed to realloc hlist ptrs to len %d\n", nfound);
        return 1;
    }

    if( gd->verb > 1 )
        fprintf(stderr,"++ type %d drops list length from %d to %d\n",
                gd->type, *len, nfound);

    *len = nfound;

    return 0;
}

/* perhaps we want to remove everything that is not up to our 'level' */
int restrict_by_level(global_data * gd, hist_type *** hlist, int * len)
{
    hist_type ** hptr;                  /* just for typing */
    int          c, nfound;

    if( !hlist || !*hlist || !len || *len <= 0 ) {
        if( gd->verb > 1 ) fprintf(stderr,"** bad, naughty restriction\n");
        return 1;
    }

    if( !gd->level && !gd->min_level ) {
        if( gd->verb > 3 ) fprintf(stderr, "-- no levels to restrict to\n");
        return 0;
    }

    /* move good pointers to beginning of list, and count num found */
    nfound = 0;
    hptr = *hlist;
    for( c = 0; c < *len; c++) {
        if( (gd->level && gd->level != hptr[c]->level) ||
            (hptr[c]->level < gd->min_level) ) continue;  /* skip it */

        /* we have a winner! */
        if( c > nfound ) hptr[nfound] = hptr[c];  /* move it up */
        nfound++;
    }

    if( nfound == 0 ) {         /* death by 'levels' */
        if(gd->verb>0) fprintf(stderr,"-- no history for level restriction\n");
        free(*hlist);
        *hlist = NULL;
        return 1;
    }

    /* restrict the list */
    *hlist = (hist_type **)realloc(*hlist, nfound*sizeof(hist_type *));
    if( !*hlist ) {
        fprintf(stderr,"** failed to realloc hlist ptrs to len %d\n", nfound);
        return 1;
    }

    if( gd->verb > 1 ) {
        if( gd->level )
            fprintf(stderr,"++ level %d drops list length from %d to %d\n",
                    gd->level, *len, nfound);
        else
            fprintf(stderr,"++ min_level %d drops list length from %d to %d\n",
                    gd->min_level, *len, nfound);
    }

    *len = nfound;

    return 0;
}

/* perhaps we want to remove everything that is not our program of interest */
int restrict_by_program(global_data * gd, hist_type *** hlist, int * len)
{
    hist_type ** hptr;                  /* just for typing */
    int          c, nfound;

    if( !hlist || !*hlist || !len || *len <= 0 ) {
        if( gd->verb > 1 ) fprintf(stderr,"** evil restriction\n");
        return 1;
    }

    if( !gd->program ) {
        if( gd->verb > 3 ) fprintf(stderr, "-- no program to restrict to\n");
        return 0;
    }

    /* move good pointers to beginning of list, and count num found */
    nfound = 0;
    hptr = *hlist;
    for( c = 0; c < *len; c++) {
        if( ! strcmp(gd->program, hptr[c]->program) ) {
            /* we have a winner! */
            if( c > nfound ) hptr[nfound] = hptr[c];  /* move it up */
            nfound++;
        }
    }

    if( nfound == 0 ) {         /* death by 'program' */
        if( gd->verb > 0 )
            fprintf(stderr,"-- no history for program '%s'\n", gd->program);
        free(*hlist);
        *hlist = NULL;
        return 1;
    }

    /* restrict the list */
    *hlist = (hist_type **)realloc(*hlist, nfound*sizeof(hist_type *));
    if( !*hlist ) {
        fprintf(stderr,"** failed to realloc hlist ptrs to len %d\n", nfound);
        return 1;
    }

    if( gd->verb > 1 )
        fprintf(stderr,"++ program '%s' drops list length from %d to %d\n",
                gd->program, *len, nfound);

    *len = nfound;

    return 0;
}

/* meant to use as a return value, this macro assumes the ints are different */
/* (history lists are now long, so this is a speed-up)   19 May 2008 [rickr] */
#define MY_INT_COMPARE(a,b) (((a)<(b)) ? -GD.sort_dir : GD.sort_dir )

/* sort by date, then by author, level and program */
int compare_hlist(const void *v0, const void *v1)
{
    hist_type * h0 = *(hist_type **)v0;
    hist_type * h1 = *(hist_type **)v1;
    int         rv = 0;

    if( GD.sort_by_prog ) {
       if( h0->program && h1->program ) rv = strcmp(h0->program, h1->program);
       if( rv ) return MY_INT_COMPARE(rv, 0);
    }

    if( h0->yyyy != h1->yyyy ) return MY_INT_COMPARE(h0->yyyy, h1->yyyy);
    if( h0->mm   != h1->mm   ) return MY_INT_COMPARE(h0->mm  , h1->mm  );
    if( h0->dd   != h1->dd   ) return MY_INT_COMPARE(h0->dd  , h1->dd  );

    /* ignore any string that is not set (rather than return as equal) */
    /*                                             19 May 2008 [rickr] */

    if( h0->author && h1->author ) rv = strcmp(h0->author, h1->author);
    if( rv ) return MY_INT_COMPARE(rv, 0);

    if( h0->level != h1->level ) return MY_INT_COMPARE(h0->level, h1->level);

    if( ! GD.sort_by_prog ) {
       if( h0->program && h1->program ) rv = strcmp(h0->program, h1->program);
       if( rv ) return MY_INT_COMPARE(rv, 0);
    }

    return 0;
}

/* compare only the dates -- RWC */
int compare_hist_dates(hist_type *h0 , hist_type *h1)
{
    if( h0->yyyy != h1->yyyy ) return MY_INT_COMPARE(h0->yyyy, h1->yyyy);
    if( h0->mm   != h1->mm   ) return MY_INT_COMPARE(h0->mm  , h1->mm  );
    if( h0->dd   != h1->dd   ) return MY_INT_COMPARE(h0->dd  , h1->dd  );
    return 0 ;
}

/* reallocate space, and fill the array of longer struct pointers
 * (note that 'hadd' is a an array of structures) */
int add_to_hlist(hist_type *** hlist, hist_type * hadd,
                 int addlen, int * newlen)
{
    int c, prevlen;

    if( GD.verb > 4 )
        fprintf(stderr,"++ appending len %d hist to len %d total\n",
                addlen, *newlen);

    if( addlen <= 0 ) return 0; /* nothing to add */

    prevlen = *newlen;  /* store for starting index to adjust */
    *newlen += addlen;
    *hlist = (hist_type **)realloc(*hlist, *newlen*sizeof(hist_type *));
    if( !*hlist ) {
        fprintf(stderr,"** failed to realloc hlist ptrs of len %d\n", *newlen);
        return 1;
    }

    for(c = 0; c < addlen; c++)
        (*hlist)[c+prevlen] = hadd + c;   /* applies struct offsets */

    if( GD.verb > 5 && addlen > 2 ) {
        fprintf(stderr,"++ first 3 new addresses are: %p, %p, %p\n",
                (*hlist)[prevlen], (*hlist)[prevlen+1], (*hlist)[prevlen+2]);
        fprintf(stderr,"   programs are %s, %s, %s\n",
                CHECK_NULL_STR((*hlist)[prevlen]->program),
                CHECK_NULL_STR((*hlist)[prevlen+1]->program),
                CHECK_NULL_STR((*hlist)[prevlen+2]->program)); 
    }

    return 0;
}


/* reallocate space, and fill the array of struct pointers
 * (fill from first of length newlen) */
int restrict_hlist(hist_type *** hlist, int first, int oldlen, int newlen)
{
    hist_type ** horig = *hlist;        /* store the current list */
    int c;

    if( GD.verb > 2 )
        fprintf(stderr,"++ making new len %d (of %d) list at offset %d\n",
                newlen, oldlen, first);

    if( newlen < 0 || first < 0 || oldlen < 0) {
        fprintf(stderr,"** restrict_hlist: bad newlen, first, old = %d, %d, %d\n",
                newlen, first, oldlen);
        return 1;
    } else if( newlen+first > oldlen ) {
        fprintf(stderr,"** restrict_hlist: newlen+first (%d+%d) > oldlen (%d)\n",
                newlen, first, oldlen);
        return 1;
    }

    if( newlen == oldlen ) return 0;    /* easy... */

    *hlist = (hist_type **)malloc(newlen*sizeof(hist_type *));
    if( !*hlist ) {
        fprintf(stderr,"** RH: failed to malloc hlist ptrs of len %d\n", newlen);
        return 1;
    }

    for(c = 0; c < newlen; c++)
        (*hlist)[c] = horig[first+c];   /* applies struct offsets */

    if( GD.verb > 5 && newlen > 2 ) {
        fprintf(stderr,"++ RH: first 3 new addresses are: %p, %p, %p\n",
                (*hlist)[0], (*hlist)[1], (*hlist)[2]);
        fprintf(stderr,"   programs are %s, %s, %s\n",
                CHECK_NULL_STR((*hlist)[0]->program),
                CHECK_NULL_STR((*hlist)[1]->program),
                CHECK_NULL_STR((*hlist)[2]->program)); 
    }

    free(horig);        /* incoming list is garbage */

    return 0;
}


int disp_global_data(char * mesg, global_data * gd)
{
    if( mesg ) fputs(mesg, stderr);

    if( !gd ) return 1;

    fprintf(stderr,"global_data struct: \n"
            "    author                    = %s\n"
            "    program                   = %s\n"
            "    check_date                = %d\n"
            "    cd_day, cd_month, cd_year = %s, %s, %s\n"
            "    show_field                = %d\n"
            "    html, dline, type         = %d, %d, %d\n"
            "    level, min_level          = %d, %d\n"
            "    past_days, months, years  = %d, %d, %d\n"
            "    past_entries              = %d\n"
            "    sort_dir, final_by_prog   = %d, %d\n"
            "    verb, SBP, plen           = %d, %d, %d\n",
            CHECK_NULL_STR(gd->author), CHECK_NULL_STR(gd->program),
            gd->check_date, CHECK_NULL_STR(gd->cd_day),
            CHECK_NULL_STR(gd->cd_month), CHECK_NULL_STR(gd->cd_year), 
            gd->show_field, gd->html, gd->dline, gd->type,
            gd->level, gd->min_level,
            gd->past_days, gd->past_months, gd->past_years,
            gd->past_entries, gd->sort_dir, gd->final_sort_by_prog,
            gd->verb, gd->sort_by_prog, gd->plen);

    return 0;
}

int show_help(void)
{
    printf(
  "afni_history:           show AFNI updates per user, dates or levels\n"
  "\n"
  "This program is meant to display a log of updates to AFNI code, the\n"
  "website, educational material, etc.  Users can specify a level of\n"
  "importance, the author, program or how recent the changes are.\n"
  "\n"
  "The levels of importance go from 1 to 4, with meanings:\n"
  "       1 - users would not care\n"
  "       2 - of little importance, though some users might care\n"
  "       3 - fairly important\n"
  "       4 - a big change or new program\n"
  "       5 - IMPORTANT: we expect users to know\n"
  "\n"
  "-----------------------------------------------------------------\n"
  "\n"
  "common examples:\n"
  "\n"
  "  0. get help\n"
  "\n"
  "     a. afni_history -help\n"
  "\n"
  "  1. display all of the history, possibly subject to recent days/entries\n"
  "\n"
  "     a. afni_history\n"
  "     b. afni_history -past_days 5\n"
  "     c. afni_history -past_months 6\n"
  "     d. afni_history -past_entries 1\n"
  "\n"
  "  2. select a specific type, level or minimum level\n"
  "\n"
  "     a. afni_history -level 2\n"
  "     b. afni_history -min_level 3 -type BUG_FIX\n"
  "     c. afni_history -type 1 -min_level 3 -past_years 1\n"
  "\n"
  "  3. select a specific author or program\n"
  "\n"
  "     a. afni_history -author rickr\n"
  "     b. afni_history -program afni_proc.py\n"
  "\n"
  "     display the past year of updates for rickr, sorted by program name...\n"
  "\n"
  "     c. afni_history -author rickr -past_days 365 -final_sort_by_prog\n"
  "\n"
  "  4. select level 3+ suma updates from ziad over the past year\n"
  "\n"
  "     a. afni_history -author ziad -min_level 3 -program suma\n"
  "\n"
  "  5. generate a web-page, maybe from the past year at at a minimum level\n"
  "\n"
  "     a. afni_history -html -reverse > afni_hist_all.html\n"
  "     b. afni_history -html -reverse -min_level 2  > afni_hist_level2.html\n"
  "     c. afni_history -html -reverse -min_level 3  > afni_hist_level3.html\n"
  "     d. afni_history -html -reverse -min_level 4  > afni_hist_level4.html\n"
  "\n"
  "  5. verify that the distribution is new enough\n"
  "\n"
  "     Compare the most recent history entry against the passed date.  If\n"
  "     there is a history entry as recent as the given date, it is current.\n"
  "     Otherwise, the distribution is considered old.\n"
  "\n"
  "     a. afni_history -check_date 1 1 2010\n"
  "     b. afni_history -check_date 15 Mar 2050\n"
  "\n"
  "  6. show particular fields\n"
  "\n"
  "     a. show the program name for the most recent 10 entries\n"
  "           afni_history -show_field program 10\n"
  "     b. show the program name for the most recent 10 NEW_PROG entries\n"
  "           afni_history -type NEW_PROG -show_field program 10\n"
  "     c. show the authors of the past 10 BUG_FIX entries\n"
  "           afni_history -type BUG_FIX -show_field author 10\n"
  "     d. show all unique program fields, sorted\n"
  "           afni_history -show_field program | sort | uniq\n"
  "     e. show all unique program fields of NEW_PROG entries, sorted\n"
  "           afni_history -show_field program -type NEW_PROG | sort | uniq\n"
  "\n"
  "     f. list FIELD parameters available for use with -show_field\n"
  "           afni_history -show_field_names\n"
  "\n"
  "-----------------------------------------------------------------\n"
  "\n"
  "------------------ informational options: -----------------------\n"
  "\n"
  "  -help                    : show this help\n"
  "  -hist                    : show this program's history\n"
  "  -list_authors            : show the list of valid authors\n"
  "  -list_types              : show the list of valid change types\n"
  "  -ver                     : show this program's version\n"
  "\n"
  "------------------ output restriction options: ------------------\n"
  "\n"
  "  -author AUTHOR           : restrict output to the given AUTHOR\n"
  "  -level LEVEL             : restrict output to the given LEVEL\n"
  "  -min_level LEVEL         : restrict output to at least level LEVEL\n"
  "  -program PROGRAM         : restrict output to the given PROGRAM\n"
  "\n"
  "  -past_entries ENTRIES    : restrict output to final ENTRIES entries\n"
  "  -past_days DAYS          : restrict output to the past DAYS days\n"
  "  -past_months MONTHS      : restrict output to the past MONTHS months\n"
  "  -past_years YEARS        : restrict output to the past YEARS years\n"
  "\n"
  "  -type TYPE               : restrict output to the given TYPE\n"
  "                             (TYPE = 0..5, or strings 'NEW_PROG', etc.)\n"
  "                             e.g.  -type NEW_ENV\n"
  "                             e.g.  -type BUG_FIX\n"
  "\n"
  "------------------ verification options: ------------------------\n"
  "\n"
  "  -check_date DD MM YYYY   : check history against given date\n"
  "\n"
  "     If most recent afni_history is older than the passed date, the\n"
  "     distribution version might be considered out of date.  Otherwise, it\n"
  "     might be considered current.\n"
  "\n"
  "     If the version seems okay, afni_history returns 0, else 1.\n"
  "     That way a script can check the status.\n"
  "\n"
  "------------------ general options: -----------------------------\n"
  "\n"
  "  -html                    : add html formatting\n"
  "  -dline                   : put a divider line between dates\n"
  "  -reverse                 : reverse the sorting order\n"
  "                             (sort is by date, author, level, program)\n"
  "\n"
  "  -show_field FIELD        : restrict entry output to field FIELD\n"
  "\n"
  "     For each history entry printed in the output restrict the output\n"
  "     to only that implied by FIELD.\n"
  "\n"
  "     Valid FIELDs include:\n"
  "\n"
  "        FIELD        : what to show\n"
  "        ----------     ----------------------------------------\n"
  "        all          : all fields (same as without -show_field)\n"
  "        firstline    : only the standard first line\n"
  "        day          : day of month\n"
  "        month        : month of year\n"
  "        year         : year of calendar system\n"
  "        date         : year, month, day\n"
  "        author       : author\n"
  "        program      : program\n"
  "        level        : level\n"
  "        type         : type\n"
  "        desc         : description\n"
  "        verbtext     : verbtext\n"
  "\n"
  "     Only one -field option is allowed at the moment.\n"
  "\n"
  "     Valid FIELD values can be shown by\n"
  "        afni_history -show_field_names\n"
  "\n"
  "  -show_field_names        : list valid FIELD names for -show_field\n"
  "\n"
  "     Each entry in the output list can be passed to -show_field\n"
  "\n"
  "  -verb LEVEL              : request verbose output\n"
  "                             (LEVEL is from 0-6)\n"
  "\n"
  "\n"
  "                                           Author: Rick Reynolds\n"
  "                                           Thanks to: Ziad, Bob\n"
  "\n"
    );

    return 0;
}

int histlists_are_valid(histpair * hpairs, int plen)
{
    int errs = 0, c;

    if( GD.verb > 2 )
        fprintf(stderr,"-- checking for %d valid hlists...\n", plen);

    for( c = 0; c < plen; c++ )
        if( ! valid_histlist(hpairs[c].hlist, hpairs[c].author) )
            errs++;

    if( GD.verb > 2 ) fprintf(stderr,"++ number of bad hlists: %d\n", errs);

    if( errs ) return 0;
    else       return 1;
}

/* print any error messages here */
int valid_histlist(hist_type * hlist, char * author)
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
        fprintf(stderr,"++ author %s, %d structs, OK\n\n", author, len);

    if( errs ) return 0;
    else       return 1;
}

/* rcr - finish this, check that the dates are sorted (one way or the other) */
int hlist_is_sorted(hist_type * hlist)
{
    return 1;
}

int valid_histstruct(hist_type * hstr, char * author)
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

    if( ! INT_IN_RANGE(hstr->yyyy, FIRST_YEAR, 2050) ) {
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

    if( ! INT_IN_RANGE(hstr->level, MIN_PROG_LEVEL, MAX_PROG_LEVEL) ) {
        fprintf(stderr,"** invalid level: %d\n", hstr->level);
        errs++;
    }

    if( ! INT_IN_RANGE(hstr->type, 0, MAX_TYPE_VAL) ) {
        fprintf(stderr,"** invalid type: %d\n", hstr->type);
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

int hlist_len(hist_type * hlist)
{
    int len = 0;

    if( !hlist ) return 0;
    
    for( len = 0; hlist[len].dd != 99; len++ )
        ;

    if( GD.verb > 1 ) fprintf(stderr,"-- hlist '%s' has %d entries\n",
                              CHECK_NULL_STR(hlist->author), len);

    return len;
}

char * level_string(int level)
{
    switch( level ) {
        default:         return "INVALID";
        case MICRO:      return "MICRO";
        case MINOR:      return "MINOR";
        case MAJOR:      return "MAJOR";
        case SUPER:      return "SUPER";
        case SUPERDUPER: return "SUPERDUPER";
    }
}

char * type_string(int type)
{
    switch( type ) {
        default:                return "INVALID";
        case TYPE_GENERAL:      return "GENERAL";
        case TYPE_NEW_PROG:     return "NEW_PROG";
        case TYPE_NEW_OPT:      return "NEW_OPT";
        case TYPE_NEW_ENV:      return "NEW_ENV";
        case TYPE_BUG_FIX:      return "BUG_FIX";
        case TYPE_MODIFY:       return "MODIFY";
        case TYPE_ENHANCE:      return "ENHANCE";
        case TYPE_REMOVE:       return "REMOVE";
        case TYPE_REINSTATE:    return "REINSTATE";
    }
}

int type_string2type(char * tstring)
{
    if( !tstring || !*tstring ) {
        fprintf(stderr,"** missing type string\n");
        return TYPE_INVALID;
    }

    if( ! strcmp("GENERAL",  tstring) ) return TYPE_GENERAL;
    if( ! strcmp("NEW_PROG", tstring) ) return TYPE_NEW_PROG;
    if( ! strcmp("NEW_OPT",  tstring) ) return TYPE_NEW_OPT;
    if( ! strcmp("NEW_ENV",  tstring) ) return TYPE_NEW_ENV;
    if( ! strcmp("BUG_FIX",  tstring) ) return TYPE_BUG_FIX;
    if( ! strcmp("MODIFY",   tstring) ) return TYPE_MODIFY;
    if( ! strcmp("ENHANCE",  tstring) ) return TYPE_ENHANCE;
    if( ! strcmp("REMOVE",   tstring) ) return TYPE_REMOVE;
    if( ! strcmp("REINSTATE",tstring) ) return TYPE_REINSTATE;

    return TYPE_INVALID;
}

int show_author_list(void)
{
    char ** alist = g_author_list;
    int     len   = sizeof(g_author_list)/sizeof(char *);
    int     c;

    printf("\nafni_history author list:\n\n");
    for( c = 0; c < len-2; c+= 3 )
        printf("    %-12s %-3s      %s\n", alist[c], alist[c+1], alist[c+2]);
    putchar('\n');

    return 0;
}

int show_valid_types(void)
{
    int c;

    printf("\nvalid history types:\n\n");
    for( c = 0; c <= MAX_TYPE_VAL; c++ )
        printf("   %2d  :  %s\n", c, type_string(c));
    putchar('\n');

    return 0;
}

/* return an index into g_show_fields, or -1 on error */
int field_name2index(char * fname)
{
    int nfields = sizeof(g_show_fields) / sizeof(char *);
    int c;

    if( ! fname ) return -1;

    for( c = 0; c < nfields; c++ )
        if( !strcasecmp(fname, g_show_fields[c]) )
            return c;

    return -1; /* failure */
}

int show_valid_fields(void)
{
    int nfields = sizeof(g_show_fields) / sizeof(char *);
    int c;

    printf("\nvalid field names (%d):\n\n", nfields);
    for( c = 0; c < nfields; c++ )
        printf("   %s\n", g_show_fields[c]);
    putchar('\n');

    (void)quick_field_test();

    return 0;
}

/* just test some of the g_show_fields list against the #define's
 *
 * return 0 on success, 1 on error                                */
int quick_field_test(void)
{
   /* "firstline" is first, "verbtext" is (currently) last */
   if(    strcmp(g_show_fields[FIELD_FIRST_LINE], "firstline" )
       || strcmp(g_show_fields[FIELD_ALL],        "all"       )
       || strcmp(g_show_fields[FIELD_DATE],       "date"      )
       || strcmp(g_show_fields[FIELD_PROGRAM],    "program"   )
       || strcmp(g_show_fields[FIELD_VERB],       "verbtext"  ) )
   {
      fprintf(stderr,"** quick_field_test failure of g_show_fields\n");
      return 1;
   }

   return 0;   /* no problem */
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
    plist[c].hlist = discoraj_history;  plist[c++].author = JKR;
    plist[c].hlist = gangc_history;     plist[c++].author = GC;
    plist[c].hlist = laurenpd_history;  plist[c++].author = PDL;
    plist[c].hlist = ptaylor_history;   plist[c++].author = PT;
    plist[c].hlist = rickr_history;     plist[c++].author = RCR;
    plist[c].hlist = rwcox_history;     plist[c++].author = RWC;
    plist[c].hlist = ziad_history;      plist[c++].author = ZSS;
    gd->plen = c;

    if( gd->plen > NUM_HIST_USERS ) {
        fprintf(stderr,"** NUM_HIST_USERS too small for %d users\n",gd->plen);
        return 1;
    }

    if( gd->verb > 1 )
        fprintf(stderr,"-- histlist initialized with %d lists\n", gd->plen);

    if( gd->verb > 4 ) {
        fprintf(stderr, "-- most recent programs are:\n");
        for( c = 0; c < gd->plen; c++ )
            fprintf(stderr,"     author %s, program %s\n",
                    CHECK_NULL_STR(plist[c].hlist->author),
                    CHECK_NULL_STR(plist[c].hlist->program));
    }

    return 0;
}

