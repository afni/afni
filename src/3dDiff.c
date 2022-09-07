#include "mrilib.h"
#include "3ddata.h"

#define MRI_SIMPLE_DIFF_ERROR -1
#define MRI_SIMPLE_DIFF_DEFAULT_TOL 1e-8f

#define diff3d_crash(...) \
    do { \
        if ( quiet ) { \
            printf("-1\n"); \
            RETURN ( -1 ); \
        } \
        if ( brutalist ) { \
            printf("-1 -1 -1 -1\n"); \
            RETURN ( -1 ); \
        } \
        if ( report || long_report ) { \
            ERROR_exit(__VA_ARGS__); \
        } \
    } \
    while ( 0 )

void repchar(char c, int reps) {
    int i;
    for (i = 0; i < reps; ++i) putchar(c);
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

int help_3dDiff()
{
char * author = "JB Teves";

printf(
"\n"
"This is a program to examine element-wise differences between two images.\n"
"\n"
"Usage ~1~\n"
"\n"
"3dDiff  [display opt] [-tol TOLERANCE] [-mask MASK] <DSET_1> <DSET_2>\n"
"\n"
"where: \n"
"\n"
"  -tol TOLERANCE   :(opt) the floating-point tolerance/epsilon\n"
"\n"
"  -mask MASK:      :(opt) the mask to use when comparing\n"
"\n"
"  -a DSET_1        :(req) input dataset a\n"
"\n"
"  -b DSET_2        :(req) input dataset b\n"
"\n"
"... and there are the following (mutually exclusive) display options:\n"
"\n"
"  -q               :(opt) quiet mode, indicate 0 for no differences and\n"
"                          1 for differences. -1 indicates that an error has \n"
"                          occurred (aka \"Rick Mode\").\n"
"  -tabular         :(opt) display only a table of differences, plus\n"
"                          a summary line (the same one as -brutalist)\n"
"                          Mostly for use with 4D data.\n"
"  -brutalist       :(opt) display one-liner. The first number indicates\n"
"                          whether there is a difference, the second number \n"
"                          indicates how many elements (3D) or volumes (4D)\n"
"                          were different, and the last number indicates the\n"
"                          total number of elements/volumes compared.\n"
"                          if there is a dataset dimension mismatch or an\n"
"                          error, then this will be a line of all -1s.\n"
"                          See examples below for sample output.\n"
"  -long_report      :(opt) print a large report with lots of information.\n"
"\n"
"If no display options are used, a short message with a summary will print.\n"
"\n"
"===========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Basic Example: comparing two images\n"
"   A) In the 3D case, you get a short message indicating if there is no\n"
"      difference:\n"
"      $ 3dDiff -a image.nii -b image.nii\n"
"      ++ Images do NOT differ\n"
"\n"
"      ... or a bit more information if there is a difference:\n"
"      $ 3dDiff -a mine.nii -b yours.nii\n"
"      ++ Images differ: 126976 of 126976 elements differ (100.00%%)\n"
"\n"
"   B) In the 4D case, the total number of elements AND total number of\n"
"      volumes which differ are reported:\n"
"      $ 3dDiff -a mine.nii -b yours.nii\n"
"      ++ Images differ: 10 of 10 volumes differ (100.00%%) and 5965461 of 6082560 elements (98.07%%)\n"
"\n"
"2) A tolerance can be used to be more permissive of differences.  In this\n"
"   example, any voxel difference of 100 or less is considered equal:\n"
"   $ 3dDiff -tol 100 -a mine.nii -b yours.nii\n"
"   ++ Images differ: 234529 of 608256 elements differ (38.56%%)\n"
"\n"
"3) A mask can be used to limit which regions are being compared:\n"
"   $ 3dDiff -mask roi.nii -a mine.nii -b yours.nii\n"
"   ++ Images differ: 5 of 10 volumes differ (50.00%%) and 675225 of 1350450 elements (50.00%%)\n"
"\n"
"   NB: The mask is assumed to have a single time point;  volumes in the mask\n"
"   beyond the [0]th are ignored.\n"
"\n"
"===========================================================================\n"
"\n"
"Modes of output/reporting ~1~\n"
"\n"
"There are a variety of reporting modes for 3dDiff, with varying levels\n"
"of verbosity. They can be used to view the image comparison in both human\n"
"and machine-readable formats. The default mode is the version shown in the\n"
"above examples, where a short statement is made summarizing the differences.\n"
"Reporting modes are mutually exclusive, but may be used with any of the\n"
"other program options without restriction.\n"
"\n"
"1) Quiet Mode (-q) ~2~\n"
"   Returns a single integer value in the range [-1, 1]:"
"      -1 indicates a program error (e.g., grids do not match)\n"
"       0 indicates that the images have no differences\n"
"       1 indicates that the images have differences\n"
"\n"
"   Examples:\n"
"   $ 3dDiff -q -a image.nii # no image b supplied\n"
"   -1\n"
"\n"
"   $ 3dDiff -q -a image.nii -b image.nii # an image agrees with itself\n"
"   0\n"
"\n"
"   $ 3dDiff -q -a mine.nii -b yours.nii # two different images\n"
"   1\n"
"\n"
"2) Tabular Mode (-tabular) ~2~\n"
"   Prints out a table of values. Useful for 4D data, but not recommended\n"
"   for 3D data.\n"
"   Each row of the table will indicate the volume index and number of\n"
"   differing elements.  At the end of the table, a summary line will\n"
"   appear (see -brutalist).\n"
"\n"
"   Example (just using the first 10 volumes of two datasets):\n"
"   $ 3dDiff -tabular -a \"mine.nii[0..9]\" -b \"yours.nii[0..9]\"\n"
"   0:	596431\n"
"   1:	596465\n"
"   2:	596576\n"
"   3:	596644\n"
"   4:	596638\n"
"   5:	596658\n"
"   6:	596517\n"
"   7:	596512\n"
"   8:	596500\n"
"   9:	596520\n"
"   1 10 10 1.00000\n"
"\n"
"3) Brutalist Mode (-brutalist) ~2~\n"
"   Creates a one-line summary of the differences. The numbers appear in the\n"
"   following order:\n"
"     Summary         [-1, 1], -1 failure, 1 differences, 0 agreement\n"
"     Differences     [0, NV/NT], the number of differing elements (3D) or\n"
"                     volumes (4D)\n"
"     Total Compared  NV/NT,      the number of elements/volumes compared\n"
"     Fraction Diff   [0, 1.0],   the fraction of differing elements/volumes\n"
"\n"
"   Examples:\n"
"   $ 3dDiff -brutalist -a \"mine.nii[0]\" -b \"yours.nii[0]\" # 3D\n"
"   1 596431 608256 0.98056\n"
"\n"
"   ... which means: There is a difference, 596431 elements differed,\n"
"   608256 elements were compared. The fraction of differing elements is\n"
"   0.98056.)\n"
"\n"
"   $ 3dDiff -brutalist -a \"mine.nii[0..9]\" -b \"yours.nii[0..9]\" # 4D\n"
"   1 10 10 1.00000\n"
"\n"
"   ... which means: There is a difference, 10 volumes differed, 10 volumes\n"
"   were compared.  The fraction of differing volumes is 1.0).\n"
"\n"
"   If the program fails for some reason, brutalist output will be an array\n"
"   of all -1s, like this:\n"
"   $ 3dDiff -brutalist -a image.nii # no dataset b to compare to\n"
"   -1 -1 -1 -1\n"
"\n"
"4) Long Report Mode (-long_report)\n"
"   Prints a very large report with lots of information.\n"
"   **WARNING:** this report is intended for use with humans, not machines!\n"
"   The author makes no guarantee of backwards compatibility for this mode,\n"
"   and will add or remove report outputs at his own (shocking whimsical)\n"
"   discretion.\n"
"\n"
"===========================================================================\n"
"\n"
"Note on unhappy comparisons ~1~\n"
"\n"
"If this program reports that the images cannot be element-wise compared,\n"
"you can examine the header information with 3dinfo. In particular, check out\n"
"the section, \"Options requiring dataset pairing at input\", most notably\n"
"options starting with \"same\", for example, -same_grid.\n"
"===========================================================================\n"
"\n"
"Author note: ~1~\n"
"Written by %s, who notes:\n"
"  \"Perfection is achieved not when there is no data left to\n"
"  add, but when there is no data left to throw away.\"\n"
"\n",
author );

PRINT_COMPILE_DATE;

return 0;
}

int main( int argc , char * argv[] )
{
    /* Variables for reading in the dsets */
    char *a_fname=NULL, *b_fname=NULL, *mask_fname=NULL;
    THD_3dim_dataset *ds1=NULL, *ds2=NULL, *ds_mask=NULL;
    /* Variables for program args */
    int iarg=1 ; /* position in argument parser */
    float tol = MRI_SIMPLE_DIFF_DEFAULT_TOL ; /* tolerance for equality */
    float av, bv;
    float *maskarr=NULL, *ds1_masked=NULL, *ds2_masked=NULL;

    int disp_opt_sum = 0;   /* init to zero, to check excl inp opts */
    int brutalist    = 0;   /* whether we'll just output one line */
    int tabular      = 0;   /* whether we'll print a table */
    int quiet        = 0;   /* whether to run in quiet mode */
    int report       = 0;   /* whether to print a short report */
    int long_report   = 0;

    /* TODO: make error summary for all user mistakes simultaneously.
     * Strategy could be to count up arguments, store each error message in
     * a 2D char array, and then print it to the terminal in the desired
     * format*/
    /* Variables for reporting */
    int nt = 0;
    int nv = 0;
    int * counts=NULL;
    int total_volumes_differing = 0;
    int64_t total_elements_differing = 0;
    int64_t total_elements = 0;
    double frac_elements = 0.0;
    double frac_volumes = 0.0;
    /* Variables for 4D reporting */
    int max_diffs = 0;
    int min_diffs_nz = INT_MAX;
    int i, t, val;

    mainENTRY("3dDiff main");
    machdep();
    AFNI_logger("3dDiff", argc, argv);

    /* Parse the args */
    if (argc == 1) { help_3dDiff(); exit(0); } /* No args supplied */

    while ( iarg < argc && argv[iarg][0] == '-'){
        /* Show help if requested */
        if ( strncmp(argv[iarg],"-h",2) == 0 ||
             strncmp(argv[iarg],"--help",6) == 0) {
            help_3dDiff(); exit(0);
        }

        /* OPTIONAL: tolerance */
        if ( strncmp(argv[iarg],"-tol",4) == 0) {
            if (iarg >= argc) diff3d_crash("Need value after -tol");
            if ( sscanf( argv[++iarg] , "%f", &tol ) != 1 ) {
                diff3d_crash("Cannot parse tolerance!\n") ;
            }
            iarg++; continue;
        }

        /* OPTIONAL: mask */
        if ( strncmp(argv[iarg],"-mask",5) == 0) {
            if (++iarg >= argc) diff3d_crash("Need dset after -mask");
            mask_fname = strdup(argv[iarg]);
            iarg++; continue;
        }
        
        /* OPTIONAL: brutalist */
        if ( strncmp(argv[iarg],"-brutalist",10) == 0) {
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

        /* OPTIONAL: long_report */
        if ( strcmp(argv[iarg],"-long_report") == 0) {
            long_report = 1;
            iarg++; continue;
        }

        /* left image */
        if ( strncmp(argv[iarg],"-a",2) == 0) {
            if (++iarg >= argc) diff3d_crash("Need dset after -a");
            a_fname = strdup(argv[iarg]);
            iarg++; continue;
        }

        /* right image */
        if ( strncmp(argv[iarg],"-b",2) == 0) {
            if (++iarg >= argc) diff3d_crash("Need dset after -b");
            b_fname = strdup(argv[iarg]);
            iarg++; continue;
        }
        
        ERROR_message("ILLEGAL option: %s\n", argv[iarg]) ;
        suggest_best_prog_option(argv[0], argv[iarg]);
        exit(1);
    }

    /* Validate args */
    if ( !a_fname )
        diff3d_crash("No dset a supplied!");
    if ( !b_fname )
        diff3d_crash("No dset b supplied!");
    /* tolerance is guaranteed to exist, validation performed above */

    /*   Thanks to PT for simplifying options for me */
    disp_opt_sum = brutalist + tabular + quiet + long_report;
    if ( !disp_opt_sum ){
        report = 1;
    }
    else if ( disp_opt_sum > 1 ){
       diff3d_crash("Must choose ONLY one of these display opts:"
                  "  -brutalist, -tabular, -q, -long_report");
    }
    // ... and in any other case, one should have exactly 1 option
    // entered by the user, and be OK


    /* Load the images and check for mutual compatibility*/
    set_obliquity_report(0); /* We'll check that ourselves below */
    ds1 = THD_open_dataset(a_fname);
    if ( ds1 == NULL ) diff3d_crash("Can't open dataset %s", a_fname);
    if ( mask_fname ) DSET_mallocize(ds1);
    DSET_load(ds1); CHECK_LOAD_ERROR(ds1);
    ds2 = THD_open_dataset(b_fname);
    if ( ds2 == NULL ) diff3d_crash("Can't open dataset %s", b_fname);
    if ( mask_fname ) DSET_mallocize(ds2);
    DSET_load(ds2); CHECK_LOAD_ERROR(ds2);
    if ( THD_dataset_mismatch( ds1, ds2 ) ) {
        diff3d_crash("Mismatch between dsets!\n");
    }
    if ( DSET_NVALS(ds1) != DSET_NVALS(ds2) ) {
        diff3d_crash(
            "Incompatible time points: dset %s contains %d timepoints, dset %s contains %d timepoints",
            a_fname, DSET_NVALS(ds1), b_fname, DSET_NVALS(ds2)
        );
    }
    nt = DSET_NVALS(ds1);
    nv = DSET_NVOX(ds1);
    counts = (int*) calloc(nv, sizeof(int));
    /* If there's a mask, validate and apply it to the dsets */
    if ( mask_fname ) {
        total_elements = 0;
        ds_mask = THD_open_dataset(mask_fname);
        DSET_load(ds_mask); CHECK_LOAD_ERROR(ds_mask);
        if ( THD_dataset_mismatch( ds1, ds_mask ) ) {
            diff3d_crash("Mismatch between input and mask dsets!\n");
        }

        /* Allocate an array to get a binary mask */
        maskarr = (float*) calloc( nv, sizeof(float));
        ds1_masked = (float*) calloc( nv, sizeof(float));
        ds2_masked = (float*) calloc( nv, sizeof(float));

        /* Binarize the mask in an array */
        for (i = 0; i < nv; ++i) {
            maskarr[i] = THD_get_voxel(ds_mask, i, 0) != 0.0;
            total_elements += maskarr[i];
        }
        total_elements *= nt;

        /* Iterate over volumes and times */
        for (t = 0; t < nt; ++t) {
            for (i = 0; i < nv; ++i) {
                av = THD_get_voxel(ds1, i, t) * maskarr[i];
                bv = THD_get_voxel(ds2, i, t) * maskarr[i];
                counts[t] += ABS(av - bv) > tol;
            }
        }
    }
    else {
        total_elements = (int64_t)nv * nt;
        for (t = 0; t < nt; ++t) {
            for (i = 0; i < nv; ++i) {
                av = THD_get_voxel(ds1, i, t);
                bv = THD_get_voxel(ds2, i, t);
                counts[t] += ABS( THD_get_voxel(ds1, i, t) - THD_get_voxel(ds2, i, t)) > tol;
            }
        }
    }
    for (i = 0; i < nt; ++i) {
        total_volumes_differing += (counts[i] != 0);
        total_elements_differing += counts[i];
    }
    frac_elements = (double)total_elements_differing / total_elements;
    frac_volumes = (double)total_volumes_differing / nt;

    /* Do the reporting */
    if ( tabular ) {
        for (i = 0; i < nt; ++i ) {
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
                "%d %" PRId64 " %" PRId64 " %.5f\n",
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
        /* We should short-circuit early and exit, so if we made it here
         * then there should be no differences; leaving check in place in
         * case some future JBT or collaborator messes that up. */
        printf("%d\n", ( total_elements_differing != 0 ));
    }
    if ( report ) {
        if ( total_elements_differing == 0) {
            INFO_message("Images do NOT differ");
        }
        else {
            if ( nt == 1 ) {
                INFO_message(
                    "Images differ: %d of %d elements differ (%5.2f%%)",
                    total_elements_differing, total_elements,
                    frac_elements * 100
                );
            }
            else {
                INFO_message(
                    "Images differ: %d of %d volumes differ (%5.2f%%) "
                    "and %lld of %lld elements (%5.2f%%)",
                    total_volumes_differing, DSET_NVALS(ds1), frac_volumes * 100,
                    total_elements_differing, total_elements, frac_elements * 100
                );
            }
        }
    }
    if ( long_report ) {
        dashline();
        printf("3dDiff Report for %s\n", getenv("USER"));
        dashline();
        printf(
            "Called with options\n  a: %s\n  b: %s\n  mask: %s\n  tol: %e\n",
            a_fname, b_fname, ( mask_fname ) ? mask_fname : "None", tol
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
                printf(
                    "%" PRId64 " of %" PRId64 " elements (%5.2f%%) contained"
                    " differences.\n",
                    total_elements_differing, total_elements,
                    frac_elements * 100.0
                );


                /* Calculate the max number of differing elements */
                for (i = 0; i < nt; ++i) {
                    max_diffs = (counts[i] > max_diffs) ? counts[i] : max_diffs;
                }
                /* Calculate the minimum nonzero number of differing
                 * elements*/
                for (i = 0; i < nt; ++i) {
                    val = counts[i];
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
                    "%" PRId64 " of %" PRId64 " elements (%5.2f%%) contained"
                    " differences.\n",
                    total_elements_differing, total_elements,
                    frac_elements * 100.0
                );
            }

        }
        dashline();
    }
    /* Free the memory! */
    free(a_fname);
    free(b_fname);
    free(mask_fname);
    free(counts);
    free(ds1);
    free(ds2);
}
