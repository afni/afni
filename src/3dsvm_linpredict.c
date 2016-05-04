/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/* Cameron Craddock - modification of 3ddot to support linear svm prediction
       from w (bucket) calculated by 3dsvm */

#include "mrilib.h"
#include <string.h>

#define SCALE 4000000

int main( int argc , char * argv[] )
{
    ATR_float * atr_float = NULL;
    double dxy;
    int narg = 0;
    int ndset = 0;
    int nvox = 0;
    THD_3dim_dataset *xset = NULL;
    THD_3dim_dataset *yset = NULL;
    THD_3dim_dataset *mask_dset= NULL;
    float mask_bot = 666.0;
    float mask_top = -666.0 ;
    double bias_val = 0.0;
    float *fxar = NULL;
    float *fyar = NULL;
    byte *mmm = NULL;
    int ivx = 0;
    int ivy = 0;
    int iv = 0;
    void *xar = NULL;
    void *yar = NULL;
    int itypx = 0;
    int itypy = 0;
    int fxar_new = 0;
    int fyar_new = 0;

    /*-- read command line arguments --*/

    if( argc < 3 || strncmp(argv[1],"-help",5) == 0 )
    {
        printf("Usage: 3ddot [options] w dset\n"
               "Output = linear prediction for w from 3dsvm \n"
               "         - you can use sub-brick selectors on the dsets\n"
               "         - the result is a number printed to stdout\n"
               "Options:\n"
               "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
               "                 Only voxels with nonzero values in 'mset'\n"
               "                 will be averaged from 'dataset'.  Note\n"
               "                 that the mask dataset and the input dataset\n"
               "                 must have the same number of voxels.\n"
            );

        printf("\n" MASTER_SHORTHELP_STRING ) ;
        PRINT_COMPILE_DATE; 
        exit(0);
    }

    narg = 1;
    while( narg < argc && argv[narg][0] == '-' )
    {
        if( strncmp(argv[narg],"-mask",5) == 0 )
        {
            if( mask_dset != NULL )
            {
                fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
            }
            if( narg+1 >= argc )
            {
                fprintf(stderr,"*** -mask option requires a following argument!\n");
                exit(1) ;
            }
            mask_dset = THD_open_dataset( argv[++narg] ) ;
            if( mask_dset == NULL )
            {
                fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
            }
            if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
            {
                fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n");
                exit(1) ;
            }
            narg++;
            continue;
        }

        fprintf(stderr,"*** Unknown option: %s\n",argv[narg]);
        exit(1);
    }

    ndset = argc - narg ;
    if( ndset <= 1 )
    {
        fprintf(stderr,"*** No input datasets!?\n"); 
        exit(1);
    }

    /* xset is the linear 3dsvm model, yset is the data to be
       predicted */
    xset = THD_open_dataset( argv[narg++] );
    if( xset == NULL )
    {
        fprintf(stderr,"*** cannot open first input dataset!\n");
        DSET_delete(mask_dset) ;
        exit(1);
    }

    /* get the B value from the header */
    atr_float = THD_find_float_atr ( xset->dblk, "3DSVM_B" );
    if ( atr_float != NULL )
    {
        if( atr_float->nfl == 0 )
        {
            fprintf(stderr, "Could not find 3DSVM_B. Is the first dataset a bucket output from 3dsvm.\n" );
            DSET_delete(mask_dset) ;
            DSET_delete(xset) ;
            exit(1);
        }
        else if( atr_float->nfl > 1 )
        {
            fprintf(stderr, "3dsvm_linpredict cannot handle multi-class models.\n" );
            DSET_delete(mask_dset) ;
            DSET_delete(xset) ;
            exit(1);
        }
        else
        {
            bias_val=atr_float->fl[0];
        }
    }
    else
    {
        fprintf(stderr, "Could not find 3DSVM_B. Is the first dataset a bucket output from 3dsvm.\n" );
        DSET_delete(mask_dset) ;
        DSET_delete(xset) ;
        exit(1);
    }

    yset = THD_open_dataset( argv[narg++] );
    if( yset == NULL )
    {
        fprintf(stderr,"*** cannot open second input datasets!\n");
        DSET_delete(mask_dset);
        DSET_delete(xset);
        exit(1);
    }
    if( DSET_NUM_TIMES(xset) > 1 )
    {
        fprintf(stderr,"*** cannot use time-dependent datasets!\n"); 
        DSET_delete(mask_dset);
        DSET_delete(xset);
        DSET_delete(yset);
        exit(1);
    }
    nvox = DSET_NVOX(xset);
    if( nvox != DSET_NVOX(yset) )
    {
        fprintf(stderr,"*** input datasets dimensions don't match!\n");
        DSET_delete(mask_dset);
        DSET_delete(xset);
        DSET_delete(yset);
        exit(1);
    }
    if( !EQUIV_GRIDS(xset,yset) )
    {
        WARNING_message("input datasets don't have same grids");
    }


    /* make a byte mask from mask dataset */
    if( mask_dset != NULL )
    {
        int mcount;
        if( DSET_NVOX(mask_dset) != nvox )
        {
            fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n");
            DSET_delete(mask_dset);
            DSET_delete(xset);
            DSET_delete(yset);
            exit(1);
        }
        mmm = THD_makemask( mask_dset, 0, mask_bot, mask_top );
        mcount = THD_countmask( nvox , mmm );
        fprintf(stderr,"+++ %d voxels in the mask\n",mcount) ;
        if( mcount <= 5 )
        {
            fprintf(stderr,"*** Mask is too small!\n");
            DSET_delete(mask_dset);
            DSET_delete(xset);
            DSET_delete(yset);
            exit(1);
        }
        DSET_delete(mask_dset) ;
    }

    /* load bricks */

    DSET_load(xset);
    CHECK_LOAD_ERROR(xset);
    ivx   = 0 ;
    itypx = DSET_BRICK_TYPE(xset,ivx) ;
    xar   = DSET_ARRAY(xset,ivx); 
    if( xar == NULL )
    { 
        fprintf(stderr,"Could not access brick %d in first datset\n", ivx);
        exit(1);
    }
    if( itypx == MRI_float )
    {
        fxar = (float *) xar; 
        fxar_new = 0;
    } 
    else 
    {
        fxar = (float *) malloc( sizeof(float) * nvox ); 
        if( fxar == NULL )
        { 
            fprintf(stderr,"Could not allocate fxar\n");
            exit(1);
        }
        fxar_new = 1 ;
        EDIT_coerce_type( nvox, itypx, xar, MRI_float, fxar );
    }

    DSET_load(yset); 
    CHECK_LOAD_ERROR(yset);
 
    for( ivy=0; ivy<DSET_NUM_TIMES(yset); ivy++ )
    {
        itypy = DSET_BRICK_TYPE(yset,ivy);
        yar   = DSET_ARRAY(yset,ivy); 
        if( yar == NULL )
        { 
            fprintf(stderr,"Could not access brick %d in second datset\n", ivy);
            if( fxar_new == 1 )
                free(fxar);
            fxar=NULL;
            DSET_delete(mask_dset);
            DSET_delete(xset);
            DSET_delete(yset);
            exit(1);
        }

        if( itypy == MRI_float )
        {
            fyar = (float *) yar ; fyar_new = 0 ;
        } 
        else 
        {
            if( fyar == NULL )
            {
                fyar = (float *) malloc( sizeof(float) * nvox ); 
                if( fyar == NULL )
                { 
                    fprintf(stderr,"Could not allocate fyar\n");
                    if( fxar_new == 1 )
                    free( fxar);
                    fyar=NULL;
                    DSET_delete(mask_dset);
                    DSET_delete(xset);
                    DSET_delete(yset);
                    exit(1);
                }
                fyar_new = 1;
            }
            EDIT_coerce_type( nvox, itypy, yar, MRI_float, fyar ) ;
        }

        dxy = 0.0;
        for( iv = 0; iv<nvox; iv++ )
        {
            if(( mmm == NULL ) || ( mmm[iv] == 1 ))
            {
                dxy += ((double)fxar[iv]/(double)SCALE)*((double)fyar[iv]);
            }
        }
        dxy -= bias_val;
        printf( "%g\n",dxy );
    }
   
    if ( fxar_new == 1 ) free( fxar ); 
    if ( fyar_new == 1 ) free( fyar );
    if ( mmm != NULL ) free ( mmm );
    DSET_delete(mask_dset);
    DSET_delete(xset);
    DSET_delete(yset);
    exit(0) ;
}
