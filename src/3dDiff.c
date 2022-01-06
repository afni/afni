#include "mrilib.h"
#include "3ddata.h"

#define MRI_SIMPLE_DIFF_ERROR -1
#define MRI_SIMPLE_DIFF_DEFAULT_TOL 1e-8f

void repchar(char c, int reps) {
    for (int i = 0; i < reps; ++i) putchar(c);
}

int jbt_get_terminal_width() {
    const int DEFAULT_WIDTH = 79;
    int width = 0;
    char * term_width = getenv("COLUMNS");
    if ( !term_width ) width = DEFAULT_WIDTH;
    else {
        width = atoi(term_width);
        if ( width < 0 ) width = DEFAULT_WIDTH;
    }
    return width;
}
void dashline() {
    repchar('-', jbt_get_terminal_width());
    putchar('\n');
}

char * random_tps_signoff() {
    static char signoffs[5][256] = {
        "Yeahhhhh, if you could look at your data, that'd be greeeat"
        " --Beefield Lumbergh",
        "I will analyze this place to the ground"
        " --Magnilton",
        "It's just a few p-values, who will notice?"
        " -- Statisticir",
        "So, what exactly do you collect here?"
        " -- the Beep Beeps",
        "No. No, man. Science, no. I believe you'd get your paper"
        " rejected saying something like that, man."
        " -- P-value Gibbons"
    };
    srand(time(NULL));
    return signoffs[rand() % 5];
}

int help_3dDiff()
{
char * author = "JB Teves";

printf(
"\n"
"This is a program to examine element-wise differences between two images.\n"
"\n"
"written by: %s\n"
"\n"
"Usage: 3dDiff  [display opt] [-tol TOLERANCE] <-a DSET_1> <-b DSET_2>\n"
"\n"
"where: \n"
"\n"
"  -tol TOLERANCE   :(opt) the floating-point tolerance/epsilon\n"
"\n"
"  -a DSET_1        :(req) input dataset a\n"
"\n"
"  -b DSET_2        :(req) input dataset b\n"
"\n"
"... and there are the following (mutually exclusive) display options:\n"
"\n"
"  -q               :(opt) quiet mode, indicate 0 for no differences and\n"
"                          1 for differences. (aka \"Rick Mode\")\n"
"  -tabular         :(opt) display only a table of differences, plus\n"
"                          a summary line (the same one as -brutalist)\n"
"  -brutalist       :(opt) display one-liner. The first number indicates\n"
"                          whether there is a difference, the second number \n"
"                          indicates how many elements (3D) or volumes (4D)\n"
"                          were different, and the last number indicates the\n"
"                          total number of elements/volumes compared.\n"
"  -tps_report      :(opt) print a large report with lots of information.\n"
"\n"
"If no display options are used, a short message with a summary will print.\n"
"\n"
"===========================================================================\n"
"\n"
"See Also ~2~\n"
"\n"
"    If this program reports that the images cannot be element-wise compared,\n"
"you can examine the header information with 3dinfo. In particular, check out\n"
"the section, \"Options requiring dataset pairing at input\", most notably\n"
"options starting with \"same\", for example, -same_grid. You may also use 3dinfo\n"
"in place of this program, using -val_ndiff. However, options such as -tol and\n"
"-brutalist will not be available."
"\n",
author );

return 0;
}

int main( int argc , char * argv[] )
{
    /* Variables for reading in the dsets */
    char *a_fname, *b_fname = NULL;
    THD_3dim_dataset *ds1, *ds2 = NULL;
    /* Variables for program args */
    int iarg=1 ; /* position in argument parser */
    float tol = MRI_SIMPLE_DIFF_DEFAULT_TOL ; /* tolerance for equality */

    int disp_opt_sum = 0;   /* init to zero, to check excl inp opts */
    int brutalist    = 0;   /* whether we'll just output one line */
    int tabular      = 0;   /* whether we'll print a table */
    int quiet        = 0;   /* whether to run in quiet mode */
    int report       = 0;   /* whether to print a short report */
    int tps_report   = 0;

    /* TODO: make error summary for all user mistakes simultaneously.
     * Strategy could be to count up arguments, store each error message in
     * a 2D char array, and then print it to the terminal in the desired
     * format*/
    /* Variables for reporting */
    int nt = 0;
    int nv = 0;
    int * counts;
    int total_volumes_differing = 0;
    int64_t total_elements_differing = 0;
    int64_t total_elements = 0;
    double frac_elements = 0.0;
    double frac_volumes = 0.0;
    /* Variables for 4D reporting */
    int max_diffs = 0;
    int min_diffs_nz = INT_MAX;

    mainENTRY("3dDiff main");
    machdep();
    AFNI_logger("3dDiff", argc, argv);

    /* Parse the args */
    if (argc == 1) { help_3dDiff(); exit(0); } /* No args supplied */

    while ( iarg < argc && argv[iarg][0] == '-'){
        /* OPTIONAL: tolerance */
        if ( strncmp(argv[iarg],"-tol",4) == 0) {
            if (iarg >= argc) ERROR_exit("Need value after -tol");
            if ( sscanf( argv[++iarg] , "%f", &tol ) != 1 ) {
                ERROR_exit("Cannot parse tolerance!\n") ;
            }
            iarg++; continue;
        }
        
        /* OPTIONAL: brutalist */
        if ( strcmp(argv[iarg],"-brutalist") == 0) {
            brutalist = 1;
            iarg++; continue;
        }

        /* OPTIONAL: tabular */
        if ( strcmp(argv[iarg],"-tabular") == 0) {
            tabular = 1;
            iarg++; continue;
        }

        /* OPTIONAL: quiet */
        if ( strcmp(argv[iarg],"-q") == 0) {
            quiet = 1;
            iarg++; continue;
        }

        /* OPTIONAL: tps_report */
        if ( strcmp(argv[iarg],"-tps_report") == 0) {
            tps_report = 1;
            iarg++; continue;
        }

        /* left image */
        if ( strncmp(argv[iarg],"-a",2) == 0) {
            if (++iarg >= argc) ERROR_exit("Need dset after -a");
            a_fname = strdup(argv[iarg]);
            iarg++; continue;
        }

        /* right image */
        if ( strncmp(argv[iarg],"-b",2) == 0) {
            if (++iarg >= argc) ERROR_exit("Need dset after -b");
            b_fname = strdup(argv[iarg]);
            iarg++; continue;
        }

        ERROR_message("ILLEGAL option: %s\n", argv[iarg]) ;
                suggest_best_prog_option(argv[0], argv[iarg]);
        exit(1);
    }

    /* Validate args */
    if ( !a_fname )
        ERROR_exit("No dset a supplied!");
    if ( !b_fname )
        ERROR_exit("No dset b supplied!");
    /* tolerance is guaranteed to exist, validation performed above */

    /*   Thanks to PT for simplifying options for me */
    disp_opt_sum = brutalist + tabular + quiet + tps_report;
    if ( !disp_opt_sum ){
        report = 1;
    }
    else if ( disp_opt_sum > 1 ){
       ERROR_exit("Must choose ONLY one of these display opts:"
                  "  -brutalist, -tabular, -q, -tps_report");
    }
    // ... and in any other case, one should have exactly 1 option
    // entered by the user, and be OK


    /* Load the images and check for mutual compatibility*/
    set_obliquity_report(0); /* We'll check that ourselves below */
    ds1 = THD_open_dataset(a_fname);
    if ( ds1 == NULL ) ERROR_exit("Can't open dataset %s", a_fname);
    DSET_load(ds1); CHECK_LOAD_ERROR(ds1);
    ds2 = THD_open_dataset(b_fname);
    if ( ds2 == NULL ) ERROR_exit("Can't open dataset %s", b_fname);
    DSET_load(ds2); CHECK_LOAD_ERROR(ds2);
    if ( THD_dataset_mismatch( ds1, ds2 ) ) {
        ERROR_exit("Mismatch between dsets!\n");
    }
    if ( DSET_NVALS(ds1) != DSET_NVALS(ds2) ) {
        ERROR_exit(
            "Incompatible time points: dset %s contains %d timepoints, dset %s contains %d timepoints",
            a_fname, DSET_NVALS(ds1), b_fname, DSET_NVALS(ds2)
        );
    }
    nt = DSET_NVALS(ds1);
    nv = DSET_NVOX(ds1);
    total_elements = (int64_t) nt * nv;
    /* Allocate array for storing each volume's number of differing
     * elements, so we can print a table later */
    counts = calloc(nt, sizeof( int ));

    for (int i = 0; i < nt; ++i) {
        counts[i] = THD_count_diffs(ds1, ds2, i, tol);
        if ( counts[i] ) {
            ++total_volumes_differing;
            total_elements_differing += counts[i];
        }
    }
    frac_elements = (double)total_elements_differing / total_elements;
    frac_volumes = (double)total_volumes_differing / DSET_NVALS(ds1);

    /* Do the reporting */
    if ( tabular ) {
        for (int i = 0; i < nt; ++i ) {
            printf(
                "%*d:\t%d\n",
                (int)ceil(log10(nt)), i, counts[i]
            );
        }
    }
    /* Note: in practice, the user would have to scroll up to see this as a
     * header, so we'll treat it as a footer instead */
    if ( brutalist || tabular ) {
        if ( nt == 1 ) {
            printf(
                "%d %lld %lld %.5f\n",
                (total_volumes_differing != 0), total_elements_differing,
                total_elements, frac_elements
            );
        }
        else {
            printf(
                "%d %d %d %.5f\n",
                (total_volumes_differing != 0), total_volumes_differing,
                DSET_NVALS(ds1), frac_volumes
            );
        }
    }
    if ( quiet ) {
        printf("%d\n", ( total_elements_differing == 0 ));
    }
    if ( report ) {
        if ( total_elements_differing == 0) {
            INFO_message("Images do NOT differ");
        }
        if ( nt == 1 ) {
            INFO_message(
                "Images differ: %d of %d elements differ (%5.2f%%)",
                total_elements_differing, total_elements,
                frac_elements * 100
            );
        }
        else {
            INFO_message(
                "Images differ: %d of %d volumes differ (%5.2f%%).",
                total_volumes_differing, DSET_NVALS(ds1),
                frac_elements * 100
            );
        }
    }
    if ( tps_report ) {
        dashline();
        printf("3dDiff TPS Report for %s\n", getenv("USER"));
        dashline();
        printf(
            "Called with options\n  a: %s\n  b: %s\n  tol: %e\n",
            a_fname, b_fname, tol
        );

        if ( total_elements_differing == 0) {
            printf("No image differences!\n");
        }
        else {
            if ( nt > 1 ) {
                printf(
                    "%d of %d timepoints (%5.2f%%) contained differences.\n",
                    total_volumes_differing, DSET_NVALS(ds1),
                    frac_volumes * 100.0
                );

                /* Calculate the max number of differing elements */
                for (int i = 0; i < nt; ++i) {
                    max_diffs = (counts[i] > max_diffs) ? counts[i] : max_diffs;
                }
                /* Calculate the minimum nonzero number of differing
                 * elements*/
                for (int i = 0; i < nt; ++i) {
                    int val = counts[i];
                    if ( val != 0 ) {
                        min_diffs_nz = (val < min_diffs_nz) ? val : min_diffs_nz;
                    }
                }
                printf("Statistics\n");
                printf("  max number of diffs: %d\n", max_diffs);
                printf("  min number of nonzero diffs: %d\n", min_diffs_nz);
            }
            else {
                printf(
                    "%lld of %lld elements (%5.2f%%) contained differences.\n",
                    total_elements_differing, total_elements,
                    frac_elements * 100.0
                );
            }

        }
        dashline();
        printf("%s\n", random_tps_signoff());
        dashline();
        /* If 4D, calculate summary statistics */
    }
    /* Free the memory! */
    free(a_fname);
    free(b_fname);
}
