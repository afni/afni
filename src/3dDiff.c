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
#define MRI_SIMPLE_DIFF_TOL 1e-8f

int mri_ndiff( MRI_IMAGE *bim , MRI_IMAGE *nim )
{
    int nvox , ii , diffs = 0 ;
    MRI_IMAGE *fim , *gim ;
    float *far, *gar ;
    float sdif , curr_diff;

ENTRY("mri_simple_diff") ;
	 
    if ( bim == NULL || nim == NULL ) RETURN(MRI_SIMPLE_DIFF_ERROR) ;

    nvox = bim->nvox ;
    if ( nim->nvox != nvox ) RETURN(MRI_SIMPLE_DIFF_DIM_DIVERGE) ;

    fim = (bim->kind != MRI_float) ? mri_to_float(bim) : bim ;
    gim = (bim->kind != MRI_float) ? mri_to_float(nim) : nim ;

    far = MRI_FLOAT_PTR(fim) ; gar = MRI_FLOAT_PTR(gim) ;

    for ( ii=0; ii < nvox; ii++){
        curr_diff = far[ii] - gar[ii];
        if ( !(curr_diff < MRI_SIMPLE_DIFF_TOL) ||
             !(curr_diff > (0.0f - MRI_SIMPLE_DIFF_TOL))) {
            ++diffs;
        }
    }
	 
    if ( fim != bim ) mri_free(fim) ;
    if ( gim != nim ) mri_free(gim) ;

    RETURN(diffs);
}

int help_3dDiff(TFORM targ, int detail)
{
    if (detail >= 0) {
        sphinx_printf(targ,
"Usage: 3dDiff <-left LEFT> <-right RIGHT>\n"
"   A program to calculate how many voxels diverge between two images.\n"
        );
    }
    if (detail >= 1) {
        sphinx_printf(targ,
" -left LSET: the left dataset.\n"
" -right RSET: the right dataset.\n"
"\n"
" The calculation is performed by casting to floating point and counting \n"
" the number of voxels that are outside the range (-1e8, 1e8).\n"
" If the dimensions of the images diverge, the program will tell you and \n"
" you will not get a count of diverging voxels. If the dimensions agree, \n"
" then the program will give you a count of the diverging voxels and a \n"
" representation of this count as a percentage of total voxels. \n"
" Useful for comparing files quickly without writing a dset to disk, as \n"
" you would with 3dCalc.\n"
        );
    }
}


int main( int argc , char * argv[] )
{
    int diff, nvox = 0 ;
    char *left_fname, *right_fname ;
    MRI_IMAGE *left_dset , *right_dset ;
    int iarg=1 , mcount , udatum = MRI_float;

    mainENTRY("3dDiff main");
    machdep();
    AFNI_logger("3dDiff", argc, argv);

#ifdef USING_MCW_MALLOC
    enable_mcw_malloc() ;
#endif

    set_obliquity_report(0);

    while ( iarg < argc && argv[iarg][0] == '-'){
        CHECK_HELP(argv[iarg], help_3dDiff);

        if ( strncmp(argv[iarg],"-left",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -left");
            left_dset = mri_read( argv[++iarg] ) ;
            if ( left_dset == NULL )
                ERROR_exit("Cannot open left dataset!\n") ;
            iarg++; continue;
        }

        if ( strncmp(argv[iarg],"-right",5) == 0) {
            if (iarg >= argc) ERROR_exit("Need dset after -right");
            right_dset = mri_read( argv[++iarg] ) ;
            if ( right_dset == NULL )
                ERROR_exit("Cannot open right dataset!\n") ;
            iarg++; continue;
        }

        ERROR_message("ILLEGAL option: %s\n", argv[iarg]) ;
                suggest_best_prog_option(argv[0], argv[iarg]);
        exit(1);
    }

    if ( argc < 3 ){
        help_3dDiff(TXT, 0);
        PRINT_COMPILE_DATE ; exit(0) ;
    }

    if ( !left_dset )
        ERROR_exit("No left dset supplied!");
    if ( !right_dset)
        ERROR_exit("No right dset supplied!");

    diff = mri_ndiff(left_dset, right_dset);
    nvox = left_dset->nvox;

    if ( diff == MRI_SIMPLE_DIFF_DIM_DIVERGE ) {
        INFO_message("Image dimensions diverge") ;
        RETURN( 1 ) ;
    }
    else if ( diff == MRI_SIMPLE_DIFF_ERROR ) {
        ERROR_exit("3dDiff encountered an unknown error\n");
    }
    else if ( diff ) {
        INFO_message("Images diverge: %d of %d voxels disagree", diff, nvox);
        RETURN( 1 );
    }
    else {
        RETURN( 0 );
    }
}
