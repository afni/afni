#include "mrilib.h"
#include "matrix.h"

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

#define MRI_SIMPLE_DIFF_DIM_DIVERGE -2
#define MRI_SIMPLE_DIFF_ERROR -1
#define MRI_SIMPLE_DIFF_DEFAULT_TOL 1e-8f

int mri_ndiff( MRI_IMAGE *bim , MRI_IMAGE *nim , float tol)
{
    /* 
     * nvox is the total number of voxels to be diffed
     * ii is the current position while iterating over images
     * diffs is the total number of differing voxels */
    int nvox , ii , diffs = 0 ;
    /* *fim is the left image pointer, *gim the right */
    MRI_IMAGE *fim , *gim ;
    /* *far is the left float-version image, *gar the right */
    float *far, *gar ;
    /* curr_diff is the current voxel's difference */
    float curr_diff;

ENTRY("mri_simple_diff") ;

    /* Some data validations */
    if ( bim == NULL || nim == NULL ) RETURN(MRI_SIMPLE_DIFF_ERROR) ;
    if ( tol < 0 ) {
        tol = 0.0f - tol ;
        WARNING_message(
            "Tolerance %e was negative, performed sign flip", tol
        );
    }

    nvox = bim->nvox ;
    if ( nim->nvox != nvox ) RETURN(MRI_SIMPLE_DIFF_DIM_DIVERGE) ;

    /* If the data type is not a float, we need to coerce it to one */
    fim = (bim->kind != MRI_float) ? mri_to_float(bim) : bim ;
    gim = (bim->kind != MRI_float) ? mri_to_float(nim) : nim ;

    far = MRI_FLOAT_PTR(fim) ; gar = MRI_FLOAT_PTR(gim) ;

    /* Iterate over the voxels, counting diffs */
    for ( ii=0; ii < nvox; ii++){
        curr_diff = far[ii] - gar[ii];
        if ( !(curr_diff < tol) ||
             !(curr_diff > (0.0f - tol))) {
            ++diffs;
        }
    }

    /* Free the memory we asked for if we converted to float */
    if ( fim != bim ) mri_free(fim) ;
    if ( gim != nim ) mri_free(gim) ;

    /* It is finished */
    RETURN(diffs);
}

int help_3dDiff(TFORM targ, int detail)
{
    if (detail >= 0) {
        sphinx_printf(targ,
"Usage: 3dDiff [-tol TOL] <-left LEFT> <-right RIGHT>\n"
"   A program to calculate how many voxels diverge between two images.\n"
"       -left LSET: the left dataset.\n"
"       -right RSET: the right dataset.\n"
"       -tol TOL: the tolerance to use. (Default 1e-8).\n"
        );
    }
    if (detail >= 1) {
        sphinx_printf(targ,
"\n"
" The calculation is performed by casting to floating point and counting \n"
" the number of voxels that are outside the range (-TOL, TOL).\n"
" If the dimensions of the images diverge, the program will tell you and \n"
" you will not get a count of diverging voxels. If the dimensions agree, \n"
" then the program will give you a count of the diverging voxels and a \n"
" representation of this count as a percentage of total voxels. \n"
" Useful for comparing files quickly without writing a dset to disk, as \n"
" you would with 3dCalc.\n"
        );
    }

    return 0;
}


int main( int argc , char * argv[] )
{
    /* diff for differing voxels, nvox for total voxels */
    int diff, nvox = 0 ;
    char *left_fname, *right_fname ;
    MRI_IMAGE *left_dset , *right_dset ;
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
            left_dset = mri_read( argv[++iarg] ) ;
            if ( left_dset == NULL )
                ERROR_exit("Cannot open left dataset!\n") ;
            iarg++; continue;
        }

        /* right image */
        if ( strncmp(argv[iarg],"-right",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -right");
            right_dset = mri_read( argv[++iarg] ) ;
            if ( right_dset == NULL )
                ERROR_exit("Cannot open right dataset!\n") ;
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
        help_3dDiff(TXT, 0);
        PRINT_COMPILE_DATE ; exit(0) ;
    }

    if ( !left_dset )
        ERROR_exit("No left dset supplied!");
    if ( !right_dset )
        ERROR_exit("No right dset supplied!");

    /* Actually perform the calculation */
    diff = mri_ndiff(left_dset, right_dset, tol);
    /* How many voxels did we just diff, anyway? */
    nvox = left_dset->nvox;

    /* Report to user */
    /* NOTE: nonzero exit codes if there is a difference, to emulate
     * the behavior of the much more common "diff" program */
    if ( diff == MRI_SIMPLE_DIFF_DIM_DIVERGE ) {
        INFO_message("Image dimensions diverge") ;
        RETURN( 1 ) ;
    }
    else if ( diff == MRI_SIMPLE_DIFF_ERROR ) {
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
