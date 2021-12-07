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
" 3dDiff [-tol TOLERANCE] <-left LEFT_DSET> <-right RIGHT_DSET>\n"
"\n"
"where: \n"
"\n"
"  -tol TOLERANCE   :(opt) the floating-point tolerance/epsilon\n"
"\n"
"  -left LEFT_DSET  :(req) input left dataset\n"
"\n"
"  -right RIGHT_DSET:(req) input right dataset\n"
"\n"
"===========================================================================\n"
"\n"
"Examples ~1~\n"
"\n"
"1) Diff two images with equivalent grids, no matching voxels\n"
"%% 3dDiff -left a.nii -right b.nii\n"
"++ Images diverge: 42663935 of 42663936 voxels disagree (100.000%%)\n"
"\n"
"2) Diff two images with different dimensions entirely\n"
"%% 3dDiff -left grid1.nii -right grid2.nii\n"
"++ Image dimensions mismatch: (64, 64, 31) vs. (32, 32, 15)\n"
"\n"
"3) Diff two images with different centers, obliquities\n"
"%% 3dDiff -left ob1.nii -right ob2.nii\n"
"++ Image centers diverge\n"
"++ Image orientations diverge\n"
"++ Image obliquities diverge: 3.783151 apart\n"
"\n"
"4) Diff two images with an optional (and more permissive tolerance)"
"%% 3dDiff -tol .5 a.nii -right b.nii\n"
"++ Images diverge: 23999522 of 42663936 voxels disagree (56.252%%)\n"
"\n"
"5) Diff two images that agree completely (quietly succeeds)\n"
"%% 3dDiff -left a.nii -right a.nii\n"
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
    char *left_fname, *right_fname ;
    THD_3dim_dataset *left_dset = NULL, *right_dset = NULL;
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
    while ( iarg < argc && argv[iarg][0] == '-'){
        CHECK_HELP(argv[iarg], help_3dDiff);

        /* left image */
        if ( strncmp(argv[iarg],"-left",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -left");
            left_dset = THD_open_dataset( argv[++iarg] ) ;
            if ( left_dset == NULL )
                ERROR_exit("Cannot open left dataset!\n") ;
            DSET_load(left_dset); CHECK_LOAD_ERROR(left_dset);
            iarg++; continue;
        }

        /* right image */
        if ( strncmp(argv[iarg],"-right",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -right");
            right_dset = THD_open_dataset( argv[++iarg] ) ;
            if ( right_dset == NULL )
                ERROR_exit("Cannot open right dataset!\n") ;
            DSET_load(right_dset); CHECK_LOAD_ERROR(right_dset);
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

    if ( !left_dset )
        ERROR_exit("No left dset supplied!");
    if ( !right_dset )
        ERROR_exit("No right dset supplied!");

    /* Check for dimension mismatch, report if so */
    dim_check = THD_dataset_mismatch(left_dset, right_dset);
    if ( dim_check ) {
        if ( dim_check & MISMATCH_DELTA ) {
            INFO_message("Image centers diverge");
        }
        if ( dim_check & MISMATCH_ORIENT ) {
            INFO_message("Image orientations diverge");
        }
        if ( dim_check & MISMATCH_DIMEN ) {
            INFO_message(
                "Image dimensions mismatch: (%d, %d, %d) vs. (%d, %d, %d)",
                DSET_NX(left_dset), DSET_NY(left_dset), DSET_NZ(left_dset),
                DSET_NX(right_dset), DSET_NY(right_dset), DSET_NZ(right_dset)
            );
        }
        if ( dim_check & MISMATCH_OBLIQ ) {
            INFO_message(
                "Image obliquities diverge: %f apart",
                dset_obliquity_angle_diff(
                    left_dset, right_dset, OBLIQ_ANGLE_THRESH
                )
            );
        }
    }
    if ( ! EQUIV_GRIDS( left_dset , right_dset ) ) RETURN ( 1 ) ;

    diff = THD_count_diffs(left_dset, right_dset, tol);
    /* How many voxels did we just diff, anyway? */
    nvox = DSET_NVOX(left_dset) * DSET_NVALS(left_dset);
    /* Free the images */
    DSET_delete(left_dset); free(left_dset);
    DSET_delete(right_dset); free(right_dset);

    /* Report to user */
    if ( diff == MRI_SIMPLE_DIFF_ERROR ) {
        /* TODO: make a more informative error */
        ERROR_exit("3dDiff encountered an unknown error\n");
    }
    else if ( diff ) {
        perc_div = (double) diff / nvox * 100.0;
        INFO_message(
            "Images diverge: %d of %d voxels disagree (%2.3f%%)",
            diff, nvox, perc_div);
        RETURN( 1 );
    }
    else {
        /* Silent success */
        RETURN( 0 );
    }
}
