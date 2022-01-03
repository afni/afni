#include "mrilib.h"
#include "3ddata.h"

#define MRI_SIMPLE_DIFF_ERROR -1
#define MRI_SIMPLE_DIFF_DEFAULT_TOL 1e-8f

int help_3dDiff()
{
char * author = "JB Teves";

printf(
"\n"
"Overview ~1~ \n"
"\n"
"This is a program to show the differences between two images.\n"
"\n"
"written by: %s\n"
"\n"
"===========================================================================\n"
"\n"
"Command usage and option list ~1~ \n"
"\n"
" 3dDiff [-tol TOLERANCE] <-a DSET_1> <-b DSET_2>\n"
"\n"
"where: \n"
"\n"
"  -tol TOLERANCE   :(opt) the floating-point tolerance/epsilon\n"
"\n"
"  -brutalist       :(opt) no next report; only numbers. Heed well the\n"
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
"++ Images differ\n"
"++ Elements differ: 42663935 of 42663936 (100.00%%)\n"
"\n"
"2) Diff two images with different dimensions entirely\n"
"%% 3dDiff -a grid1.nii -b grid2.nii\n"
"++ Images differ\n"
"++ Dimensions differ: (64, 64, 31) vs. (32, 32, 15)\n"
"\n"
"3) Diff two images with different centers, obliquities\n"
"%% 3dDiff -a ob1.nii -b ob2.nii\n"
"++ Images differ\n"
"++ Centers differ\n"
"++ Orientations differ\n"
"++ Obliquities differ: 3.783151 apart\n"
"\n"
"4) Diff two images with an optional (and more permissive) tolerance\n"
"%% 3dDiff -tol .5 a.nii -right b.nii\n"
"++ Images differ\n"
"++ Elements differ: 23999522 of 42663936 (56.252%%)\n"
"\n"
"5) Diff two images that agree completely\n"
"%% 3dDiff -a a.nii -b a.nii\n"
"++ Images agree\n"
"\n"
"6) Brutalist output: a series of numbers only, images do not differ\n"
"%% 3dDiff -brutalist -a a.nii -b a.nii\n"
"0 0 0 0 0.000000 0 0 42663936\n"
"\n"
"7) Brutalist output, images aligned but differ for 42 elements\n"
"%% 3dDiff -brutalist -a a.nii -b a.nii\n"
"1 0 0 0 0.000000 0 42 42663936\n"
"\n"
"8) Brutalist output, two image dimensions differ \n"
"%% 3dDiff -brutalist -a a.nii -b a.nii\n"
"1 2 0 0 0.000000 0 -1 -1\n"
"\n"
"===========================================================================\n"
"\n"
"Brutalist Output ~3~\n"
"\n"
"Brutalist output is useful for scripting. The output is an 8-integer \n"
"array with the following elements:\n"
"\t0:\tSummary (-1 failure, 0 no difference, 1 some difference)\n"
"\t1:\tDimensions (the number of differing dimensions)\n"
"\t2:\tGrid Spacing (0 if no difference, 1 if some)\n"
"\t3:\tImage Center (0 if no difference, 1 if some)\n"
"\t4:\tObliquity (the angle of difference)\n"
"\t5:\tOrientation (0 if no difference, 1 if some)\n"
"\t6:\tElements (total differing elements, -1 if cannot)\n"
"\t7:\tTotal Elements (total elements compared, -1 if cannot)\n"
"\n",
author );

return 0;
}

typedef struct {
    int summary;
    int dimensions;
    int grid;
    int center;
    float obliquity;
    int orientation;
    int elements;
    int total_elements;
    int a_nd[4];
    int b_nd[4];
    float a_dd[3];
    float b_dd[3];
    } DiffResult;

#define DEFAULT_DIFFRESULT \
    { -1, -1, -1, -1, -1.0f, -1, -1, -1, {0, 0, 0, 0}, {0, 0, 0, 0}, {0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f}}

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
        ERROR_exit("Cannot open dataset a!\n") ;
    DSET_load(b_dset); CHECK_LOAD_ERROR(b_dset);

    
    /* Check for dimension mismatch */
    dim_check = THD_dataset_mismatch(a_dset, b_dset);
    /* Fill in dimensions */
    result.a_nd[0] = DSET_NX(a_dset);
    result.a_nd[1] = DSET_NY(a_dset);
    result.a_nd[2] = DSET_NZ(a_dset);
    result.a_nd[3] = DSET_NVALS(a_dset);
    result.b_nd[0] = DSET_NX(b_dset);
    result.b_nd[1] = DSET_NY(b_dset);
    result.b_nd[2] = DSET_NZ(b_dset);
    result.b_nd[3] = DSET_NVALS(b_dset);
    /* Fill in grid spacings */
    result.a_dd[0] = DSET_DX(a_dset);
    result.a_dd[1] = DSET_DY(a_dset);
    result.a_dd[2] = DSET_DZ(a_dset);
    result.b_dd[0] = fabs(DSET_DX(b_dset));
    result.b_dd[1] = fabs(DSET_DY(b_dset));
    result.b_dd[2] = fabs(DSET_DZ(b_dset));

    if ( dim_check ) {
        if ( dim_check & MISMATCH_DELTA ) {
            result.grid = 1;
        }
        else { result.grid = 0; }
        if ( dim_check & MISMATCH_ORIENT ) {
            result.orientation = 1;
        }
        else { result.orientation = 0; }
        if ( dim_check & MISMATCH_OBLIQ ) {
            result.obliquity = dset_obliquity_angle_diff(
                a_dset, b_dset, OBLIQ_ANGLE_THRESH
            );
        }
        else { result.obliquity = 0.0f; }
        if ( dim_check & MISMATCH_CENTER ) {
            result.center = 1;
        }
        else {
            result.center = 0;
        }
        if ( dim_check & MISMATCH_DIMEN ) {
            result.dimensions = (
                (DSET_NX(a_dset) != DSET_NX(b_dset)) +
                (DSET_NY(a_dset) != DSET_NY(b_dset)) +
                (DSET_NZ(a_dset) != DSET_NZ(b_dset)) +
                (DSET_NVALS(a_dset) != DSET_NVALS(b_dset))
            );
        }
        else { result.dimensions = 0; }
        result.summary = 1;
        RETURN( result );
    }
    else {
        result.center = 0;
        result.grid = 0;
        result.orientation = 0.0f;
        result.obliquity = 0;
        result.dimensions = 0;
    }

    diff = THD_count_diffs(a_dset, b_dset, tolerance);
    if ( diff == -1 ) {
        RETURN( result );
    }
    /* Final check for summary; whether elements agree */
    if ( diff ) {result.summary = 1;} else {result.summary = 0;}
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
        printf(
            "%d %d %d %d %f %d %d %d\n",
            r.summary, r.dimensions, r.grid, r.center, r.obliquity,
            r.orientation, r.elements, r.total_elements
        );
    }
    else {
        if ( r.summary == -1 ) {
            ERROR_message("An unknown error has occurred, sorry!");
            exit(1);
        }
        if ( r.summary ) INFO_message("Images differ");
        else INFO_message("Images agree");
        if ( r.dimensions ) {
            INFO_message(
                "Dimensions differ: (%d, %d, %d, %d) vs. (%d, %d, %d, %d)",
                r.a_nd[0], r.a_nd[1], r.a_nd[2], r.a_nd[3],
                r.b_nd[0], r.b_nd[1], r.b_nd[2], r.b_nd[3]
            );
        }
        if ( r.center ) {
            INFO_message("Centers differ");
        }
        if ( r.grid ) {
            INFO_message(
                "Grid spacings differ: (%f, %f, %f) vs. (%f, %f, %f) (mm)",
                r.a_dd[0], r.a_dd[1], r.a_dd[2],
                r.b_dd[0], r.b_dd[1], r.b_dd[2]
            );
        }
        if ( r.obliquity ) {
            INFO_message("Obliquities differ: %f degrees apart", r.obliquity);
        }
        if ( r.orientation ) {
            INFO_message("Orientations differ");
        }
        if ( r.elements != 0 && r.elements != -1) {
            INFO_message(
                "Elements differ: %d of %d (%2.2f%%) with tolerance %e",
                r.elements, r.total_elements,
                (r.elements / (r.total_elements * 1.0)) * 100.0,
                tol
            );
        }

    }

    /* Cleanup */
    free(a_fname);
    free(b_fname);
}
