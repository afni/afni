#include "mrilib.h"
#include "3ddata.h"

/*------------------------------------------------------------------------*/
/*! Return a measure of the difference between 2 images bim and nim,
with 0.0 indicating no differences.

The result is the total fraction of voxels that diverge,
dd = sum [ bim[i] != nim[i] ] / NN
i

where NN is the total number of voxels.
A tolerance of 1e-8 is used to determine "equality."
A zero would indicate total agreement between the images, and a positive
value indicates the fraction of voxels which agree.

A negative return value indicates one of the following:

- MRI_SIMPLE_DIFF_DIM_DIVERGE indicates diverging voxel dimensions
- MRI_SIMPLE_DIFF_ERROR indicates some other error
--------------------------------------------------------------------------*/

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
"  -a DSET_1        :(req) input dataset a\n"
"\n"
"  -b DSET_2        :(req) input dataset b\n"
"\n"
"===========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Diff two images with equivalent grids, no matching voxels\n"
"%% 3dDiff -a a.nii -b b.nii\n"
"++ Images diverge: 42663935 of 42663936 voxels disagree (100.000%%)\n"
"\n"
"2) Diff two images with different dimensions entirely\n"
"%% 3dDiff -a grid1.nii -b grid2.nii\n"
"++ Image dimensions mismatch: (64, 64, 31) vs. (32, 32, 15)\n"
"\n"
"3) Diff two images with different centers, obliquities\n"
"%% 3dDiff -a ob1.nii -b ob2.nii\n"
"++ Image centers diverge\n"
"++ Image orientations diverge\n"
"++ Image obliquities diverge: 3.783151 apart\n"
"\n"
"4) Diff two images with an optional (and more permissive tolerance)"
"%% 3dDiff -tol .5 a.nii -right b.nii\n"
"++ Images diverge: 23999522 of 42663936 voxels disagree (56.252%%)\n"
"\n"
"5) Diff two images that agree completely (quietly succeeds)\n"
"%% 3dDiff -a a.nii -b a.nii\n"
"\n"
"===========================================================================\n"
"\n",
author );

return 0;
}

int main( int argc , char * argv[] )
{
/* diff for differing voxels, nvox for total voxels */
    int diff, nvox, dim_check = 0 ;
    char *a_fname, *b_fname ;
    THD_3dim_dataset *a_dset = NULL, *b_dset = NULL;
    /* iarg counts where we are in the passed args; start at 1 */
    int iarg=1 ;
    /* tolerance we'll use; defaults to the value specified above */
    float tol = MRI_SIMPLE_DIFF_DEFAULT_TOL ;
    /* perc_div is the percentage diverging */
    double perc_div = 0.0;


    mainENTRY("3dDiff main");
    machdep();
    AFNI_logger("3dDiff", argc, argv);

#ifdef USING_MCW_MALLOC
    enable_mcw_malloc() ;
#endif

    /* We don't care about obliquity here */
    set_obliquity_report(0);

    /* Parse the args */
    if (argc == 1) { help_3dDiff(); exit(0); } /* No args supplied */

    while ( iarg < argc && argv[iarg][0] == '-'){
        /* left image */
        if ( strncmp(argv[iarg],"-a",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -a");
            a_dset = THD_open_dataset( argv[++iarg] ) ;
            if ( a_dset == NULL )
                ERROR_exit("Cannot open dataset a!\n") ;
            DSET_load(a_dset); CHECK_LOAD_ERROR(a_dset);
            iarg++; continue;
        }

        /* right image */
        if ( strncmp(argv[iarg],"-b",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -b");
            b_dset = THD_open_dataset( argv[++iarg] ) ;
            if ( b_dset == NULL )
                ERROR_exit("Cannot open dataset b!\n") ;
            DSET_load(b_dset); CHECK_LOAD_ERROR(b_dset);
            iarg++; continue;
        }

        /* OPTIONAL: tolerance */
        if ( strncmp(argv[iarg],"-tol",4) == 0) {
            if (iarg >= argc) ERROR_exit("Need value after -tol");
            if ( sscanf( argv[++iarg] , "%f", &tol ) != 1 ) {
                ERROR_exit("Cannot parse tolerance!\n") ;
            }
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

    if ( !a_dset )
        ERROR_exit("No dset a supplied!");
    if ( !b_dset )
        ERROR_exit("No dset b supplied!");

    /* Check for dimension mismatch, report if so */
    dim_check = THD_dataset_mismatch(a_dset, b_dset);
    if ( dim_check ) {
        if ( dim_check & MISMATCH_DELTA ) {
            INFO_message("Image centers differ");
        }
        if ( dim_check & MISMATCH_ORIENT ) {
            INFO_message("Image orientations differ");
        }
        if ( dim_check & MISMATCH_DIMEN ) {
            INFO_message(
                "Image dimensions differ: (%d, %d, %d) vs. (%d, %d, %d)",
                DSET_NX(a_dset), DSET_NY(a_dset), DSET_NZ(a_dset),
                DSET_NX(b_dset), DSET_NY(b_dset), DSET_NZ(b_dset)
            );
        }
        if ( dim_check & MISMATCH_OBLIQ ) {
            INFO_message(
                "Image obliquities differ: %f apart",
                dset_obliquity_angle_diff(
                    a_dset, b_dset, OBLIQ_ANGLE_THRESH
                )
            );
        }
    }
    if ( ! EQUIV_GRIDS( a_dset , b_dset ) ) RETURN ( 1 ) ;

    diff = THD_count_diffs(a_dset, b_dset, tol);
    /* How many voxels did we just diff, anyway? */
    nvox = DSET_NVOX(a_dset) * DSET_NVALS(a_dset);
    /* Free the images */
    DSET_delete(a_dset); free(a_dset);
    DSET_delete(b_dset); free(b_dset);

    /* Report to user */
    if ( diff == MRI_SIMPLE_DIFF_ERROR ) {
        /* TODO: make a more informative error */
        ERROR_exit("3dDiff encountered an unknown error\n");
    }
    else if ( diff ) {
        perc_div = (double) diff / nvox * 100.0;
        INFO_message(
            "Images differ: %d of %d voxels disagree (%2.3f%%) at tolerance %e",
            diff, nvox, perc_div, tol
        );
        RETURN( 1 );
    }
    else {
        INFO_message(
            "Image do NOT differ: %d of %d differ with tolerance %e",
            0, nvox, tol
        );
        RETURN( 0 );
    }
}
