#include "mrilib.h"
#include "3ddata.h"

#define MRI_SIMPLE_DIFF_ERROR -1
#define MRI_SIMPLE_DIFF_DEFAULT_TOL 1e-8f

int help_3dDiff()
{
char * author = "JB Teves";

printf(
"\n"
"This is a program to examine element-wise differences between two images.\n"
"\n"
"written by: %s\n"
"\n"
"Usage: 3dDiff  [-tol TOLERANCE] [-brutalist] <-a DSET_1> <-b DSET_2>\n"
"\n"
"where: \n"
"\n"
"  -tol TOLERANCE   :(opt) the floating-point tolerance/epsilon\n"
"\n"
"  -brutalist       :(opt) no text report; only numbers. Heed well the\n"
"                          tribulations of this option, o Mortals with\n"
"                          no scripts to bear upon this output!\n"
"\n"
"  -a DSET_1        :(req) input dataset a\n"
"\n"
"  -b DSET_2        :(req) input dataset b\n"
"\n"
"===========================================================================\n"
"\n"
"Examples ~2~\n"
"\n"
"1) Diff two images with equivalent grids, no matching elements\n"
"%% 3dDiff -a a.nii -b b.nii\n"
"++ Images differ: 42663936 of 42663936 elements (100.00%%)\n"
"\n"
"2) Diff two images with different dimensions entirely\n"
"%% 3dDiff -a grid1.nii -b grid2.nii\n"
"++ Images cannot be compared element-wise.\n"
"\n"
"3) Diff two images with an optional (and more permissive) tolerance\n"
"%% 3dDiff -tol .5 -a a.nii -b b.nii\n"
"++ Images differ:  23999522 of 42663936 (56.252%%)\n"
"\n"
"4) Diff two images that do not differ at all\n"
"%% 3dDiff -a image.nii -b image_copy.nii\n"
"++ Images do NOT differ\n"
"\n"
"5) Brutalist output: a series of numbers only, images do not differ\n"
"%% 3dDiff -brutalist -a a.nii -b a.nii\n"
"0 0 42663936\n"
"\n"
"6) Brutalist output, images cannot be element-wise compared\n"
"%% 3dDiff -brutalist -a grid_a.nii -b grid_b.nii\n"
"-1 -1 -1\n"
"\n"
"8) Brutalist output with permissive tolerance, images differ \n"
"%% 3dDiff -tol .5 -brutalist -a a.nii -b a.nii\n"
"1 23999522 42663936\n"
"\n"
"===========================================================================\n"
"\n"
"Brutalist Output ~2~\n"
"\n"
"Brutalist output is useful for scripting. The output is an 3-integer \n"
"array with the following elements:\n"
"\t0:\tSummary (-1 failure, 0 no difference, 1 some difference)\n"
"\t1:\tDiffering (total differing elements, -1 if cannot)\n"
"\t2:\tTotal Elements (total elements compared, -1 if cannot)\n"
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

typedef struct {
    int summary;
    int elements;
    int total_elements;
} DiffResult;

/* By default, the result indicates all failures */
#define DEFAULT_DIFFRESULT { -1, -1, -1}

DiffResult diff3d( char * a, char * b, float tolerance) {
    int diff, nvox, dim_check = 0;
    THD_3dim_dataset *a_dset, *b_dset = NULL;
    DiffResult result = DEFAULT_DIFFRESULT;

    ENTRY("diff3d");
#ifdef USING_MCW_MALLOC
    enable_mcw_malloc() ;
#endif

    /* We'll investigate obliquity ourselves */
    set_obliquity_report(0);

    /* Load things up */
    a_dset = THD_open_dataset( a ) ;
    if ( a_dset == NULL )
        ERROR_exit("Cannot open dataset a!\n") ;
    DSET_load(a_dset); CHECK_LOAD_ERROR(a_dset);
    b_dset = THD_open_dataset( b ) ;
    if ( b_dset == NULL )
        ERROR_exit("Cannot open dataset b!\n") ;
    DSET_load(b_dset); CHECK_LOAD_ERROR(b_dset);

    
    /* Check for dimension mismatch */
    if ( THD_dataset_mismatch(a_dset, b_dset) ) {
        RETURN( result );
    }

    diff = THD_count_diffs(a_dset, b_dset, tolerance);
    if ( diff == -1 ) {
        /* Note: this shouldn't really happen because we've checked it
         * above, but this is included for completeness */
        RETURN( result );
    }
    /* Final check for summary; whether elements agree */
    result.summary = (diff != 0);
    result.elements = diff;
    /* How many voxels did we just diff, anyway? */
    result.total_elements = DSET_NVOX(a_dset) * DSET_NVALS(a_dset);
    /* Free the images */
    DSET_delete(a_dset); free(a_dset);
    DSET_delete(b_dset); free(b_dset);

    RETURN( result );
}

int main( int argc , char * argv[] )
{
    char *a_fname, *b_fname ;
    /* iarg counts where we are in the passed args; start at 1 */
    int iarg=1 ;
    /* tolerance we'll use; defaults to the value specified above */
    float tol = MRI_SIMPLE_DIFF_DEFAULT_TOL ;
    /* whether we are using brutalist output */
    int brutalist = 0;
    /* perc_div is the percentage diverging */
    double perc_div = 0.0;
    /* the difference object */
    DiffResult r;

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

    /* With args parsed, ensure required ones were present */
    if ( argc < 3 ){
        help_3dDiff();
        PRINT_COMPILE_DATE ; exit(0) ;
    }

    if ( !a_fname )
        ERROR_exit("No dset a supplied!");
    if ( !b_fname )
        ERROR_exit("No dset b supplied!");

    r = diff3d(a_fname, b_fname, tol);

    if ( brutalist ) {
        printf( "%d %d %d\n", r.summary, r.elements, r.total_elements);
    }
    else {
        switch ( r.summary ) {
            case -1:
                INFO_message("Images cannot be compared element-wise");
                break;
            case 0:
                INFO_message("Images do NOT differ");
                break;
            case 1:
                INFO_message(
                    "Images differ: %d of %d elements (%2.2f%%) ",
                    r.elements, r.total_elements,
                    (r.elements / (r.total_elements * 1.0)) * 100.0
                );
                break;
        }
    }

    /* Cleanup */
    free(a_fname);
    free(b_fname);
}
