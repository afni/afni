 /*
afni/src/3dECM.c
*/

// Look for OpenMP macro
#ifdef USE_OMP
#include <omp.h>
#endif

// Include libraries
#include "mrilib.h"
#include <sys/mman.h>
#include <sys/types.h>
#include "sparse_array.h"
#include <limits.h>

// Define constants
#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define ETA2     4

#define MAX_NUM_TAGS 32
#define MAX_TAG_LEN 256

#define SQRT_2 ((double) 1.4142135623 )
#define STRLEN (32)

#undef  MAXCOV
#define MAXCOV 31

#undef  MAX_LABEL_SIZE
#define MAX_LABEL_SIZE 12

#undef  LTRUNC
#define LTRUNC(ss) \
 do{ if( strlen(ss) > MAX_LABEL_SIZE ){(ss)[MAX_LABEL_SIZE] = '\0'; }} while(0)

/* CC - variables for tracking memory usage stats */
static int MEM_PROF = 0;
static int MEM_STAT = 0;
static char mem_tags[MAX_NUM_TAGS][MAX_TAG_LEN];
static long mem_allocated[MAX_NUM_TAGS];
static long mem_freed[MAX_NUM_TAGS];
static long mem_num_tags = 0;
static long running_mem = 0;
static long peak_mem = 0;
static long total_mem = 0;

/* CC macro for updating mem stats */
#define INC_MEM_STATS( INC, TAG ) \
    { \
        if( MEM_PROF == 1 ) \
        { \
            int ndx = 0; \
            while( ndx < mem_num_tags ) \
            { \
                if( strncmp( mem_tags[ndx], TAG, MAX_TAG_LEN ) == 0 ) \
                { \
                    break; \
                } \
                ndx++; \
            } \
            if(( ndx >= mem_num_tags ) && (ndx < MAX_NUM_TAGS)) \
            { \
                /* adding a new tag */ \
                strncpy( mem_tags[ ndx ], TAG, (MAX_TAG_LEN-1) ); \
                mem_allocated[ ndx ] = 0; \
                mem_freed[ ndx ] = 0; \
                mem_num_tags++; \
            } \
            if( ndx < MAX_NUM_TAGS ) \
            { \
                mem_allocated[ ndx ] += (long)(INC); \
                if ((long)(INC) > 1024 ) WARNING_message( \
                    "Incrementing memory for %s by %ldB\n", TAG, (INC)); \
            } \
            else WARNING_message("No room in mem profiler for %s\n", TAG ); \
        } \
        total_mem += (long)(INC); \
        running_mem += (long)(INC); \
        if (running_mem > peak_mem) peak_mem = running_mem; \
    }

#define DEC_MEM_STATS( DEC, TAG ) \
    { \
        if( MEM_PROF == 1 ) \
        { \
            int ndx = 0; \
            while( ndx < mem_num_tags ) \
            { \
                if( strncmp( mem_tags[ndx], TAG, MAX_TAG_LEN ) == 0 ) \
                { \
                    break; \
                } \
                else ndx++ ; \
            } \
            if(( ndx >= mem_num_tags ) && (ndx < MAX_NUM_TAGS)) \
            { \
                WARNING_message( \
                    "Could not find tag %s in mem profiler\n", TAG ); \
            } \
            else \
            { \
                mem_freed[ ndx ] += (long)(DEC); \
                if ((long)(DEC) > 1024 ) INFO_message( \
                    "Free %ldB of memory for %s\n", (DEC), TAG); \
            } \
        } \
        running_mem -= (long)(DEC); \
    }

#define PRINT_MEM_STATS( TAG ) \
        if ( MEM_STAT == 1 ) \
        { \
            INFO_message("\n======\n== Mem Stats (%s): Running %3.3fMB," \
                " Total %3.3fMB, Peak %3.3fMB\n", TAG, \
                (double)(running_mem/(1024.0*1024.0)), \
                (double)(total_mem/(1024.0*1024.0)), \
                (double)(peak_mem/(1024.0*1024.0))); \
            if( MEM_PROF ==  1 ) \
            { \
                int ndx = 0; \
                INFO_message("== Memory Profile\n"); \
                for( ndx=0; ndx < mem_num_tags; ndx++ ) \
                { \
                    INFO_message("%s: %ld allocated %ld freed\n", \
                        mem_tags[ndx], mem_allocated[ndx], \
                        mem_freed[ndx] ); \
                } \
            } \
        }


/* freeing all of the allocated mem on an error can get a little messy. instead
   we can use this macro to check what has been allocated and kill it. this of 
   course requires strict discipline for initiazing all pointers to NULL and 
   resetting them to NULL when free'd. i should be able to handle that */
#define CHECK_AND_FREE_ALL_ALLOCATED_MEM \
{ \
    /* eliminate DSETS */ \
        if ( mset != NULL ) \
        { \
            DSET_unload(mset); \
            DSET_delete(mset); \
            mset = NULL ; \
        } \
\
        if ( xset != NULL ) \
        { \
            DSET_unload(xset) ; \
            DSET_delete(xset) ; \
            xset = NULL ; \
        } \
\
        if ( dset_conformed != NULL ) \
        { \
            dset_conformed = cc_free_conformed(dset_conformed,num_infiles); \
        } \
\
        if ( cset != NULL ) \
        { \
            DSET_unload(cset); \
            DSET_delete(cset); \
            cset = NULL ; \
        } \
\
     /* free the xvectim */ \
        if ( xvectim != NULL ) \
        { \
            VECTIM_destroy(xvectim) ; \
            xvectim = NULL ; \
        } \
\
    /* free allocated mems */ \
        if( roi_mask != NULL ) \
        { \
             roi_mask = cc_free_roimask(roi_mask) ; \
        } \
    /* free allocated mems */ \
        if( data_r != NULL ) \
        { \
             free(data_r); \
             data_r = NULL; \
        } \
        if( out_r != NULL ) \
        { \
             free(out_r); \
             out_r = NULL; \
        } \
\
}

/* being good and cleaning up before erroring out can be a pain, and messy, lets
   make a variadic macro that does it for us. */
#define ERROR_EXIT_CC( ... ) \
    { \
        CHECK_AND_FREE_ALL_ALLOCATED_MEM; \
        ERROR_exit( __VA_ARGS__ ); \
    }

/*----------------------------------------------------------------------------*/
static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,",") ;
   nn++ ;
}

typedef struct _roi_mask_struct
{
    long num_tot_vox;
    long num_msk_vox;
    long num_rois;
    int *roi_map;
    int *roi_counts;
    unsigned short *roi_mask;
    byte *mask;
} roi_mask_struct;

roi_mask_struct* cc_free_roimask( roi_mask_struct* roi_mask )
{
    /* go through and free all of the memory associated with the 
     * roi mask data structure. Be careful to make sure that we 
     * are not trying to free a NULL pointer, and be sure that we
     * set the freed points to NULL to avoid double frees
     * etc */
    if( roi_mask != NULL )
    {
        if( roi_mask->roi_map != NULL )
        {
            free(roi_mask->roi_map);
            roi_mask->roi_map = NULL;

            /* update memory stats */
            DEC_MEM_STATS( roi_mask->num_rois*sizeof(int), "ROI mask roi_map" );
            PRINT_MEM_STATS( "Free ROI mask roi_map" );

        }

        if( roi_mask->roi_counts != NULL )
        {
            free(roi_mask->roi_counts);
            roi_mask->roi_counts = NULL;

            /* update memory stats */
            DEC_MEM_STATS( roi_mask->num_rois*sizeof(int),
                "ROI mask roi_count" );
            PRINT_MEM_STATS( "Free ROI mask roi_counts" );
        }

        if( roi_mask->roi_mask != NULL )
        {
            free(roi_mask->roi_mask);
            roi_mask->roi_mask = NULL;

            /* update memory stats */
            DEC_MEM_STATS( roi_mask->num_msk_vox*sizeof(unsigned int),
                "ROI mask roi_mask" );
            PRINT_MEM_STATS( "Free roi_mask" );
        }

        if( roi_mask->mask != NULL )
        {
            free(roi_mask->mask);
            roi_mask->mask = NULL;

            /* update memory stats */
            DEC_MEM_STATS( roi_mask->num_tot_vox*sizeof(byte),
                "ROI mask mask" );
            PRINT_MEM_STATS( "Free mask" );
        }

        free(roi_mask);
        roi_mask = NULL;

        /* update memory stats */
        DEC_MEM_STATS( sizeof(roi_mask_struct), "ROI mask struct" );
        PRINT_MEM_STATS( "Free ROI mask struct" );

    }
    return(roi_mask);
}

roi_mask_struct* cc_create_roimask( THD_3dim_dataset *mask_dset, long auto_mask,
    long use_all )
{

    void* mar = NULL;
    long  byte_type = 0;

    /* pay the price in memory in order to simplify tracking the rois*/
    long  rois[ SHRT_MAX+1 ];
    long  roi_count = 0;
    long  num_mask_vox = 0;
    long  ii = 0;
    long  roi_val = 0;
    long  roi_ndx = 0;
    long  zero_values = 0;
    long  neg_values = 0;
    long  oor_values = 0;
    long  ivox = 0;

    roi_mask_struct* roi_mask = NULL;

    if((auto_mask == 1) || ( use_all == 1 ))
    {
        /* allocate memory for the roi output structure */
        if( (roi_mask = (roi_mask_struct*)calloc(1,sizeof(roi_mask_struct)))
            == NULL )
        {
            WARNING_message("Could not allocate %d bytes for roi_mask_struct!",
                sizeof(roi_mask_struct));
            DSET_unload(mask_dset);
            return(NULL);
        }

        /* update memory stats */
        INC_MEM_STATS( sizeof(roi_mask_struct), "ROI mask struct" );
        PRINT_MEM_STATS( "Alloc ROI mask struct" );

        /* set the header values */
        roi_mask->num_tot_vox = DSET_NVOX(mask_dset);

        if( auto_mask == 1 )
        {
        }
        else
        {
            roi_mask->num_msk_vox = roi_mask->num_tot_vox;
        }

        /* we have no ROIs */
        roi_mask->num_rois = 0;
        roi_mask->roi_counts = NULL;
        roi_mask->roi_map = NULL;
        roi_mask->roi_mask = NULL;

        if( auto_mask == 1 )
        {
            /* use the mem allocated by automask for our mask */
            INFO_message( "Using automask to calculate mask" );

            if((roi_mask->mask  = (void*) THD_automask( mask_dset )) == NULL )
            {
                WARNING_message("Automask failed!");
                roi_mask = cc_free_roimask(roi_mask);
                DSET_unload(mask_dset);
                return(NULL);
            }

            roi_mask->num_msk_vox = THD_countmask(DSET_NVOX(mask_dset),
                roi_mask->mask);

            /* update memory stats */
            INC_MEM_STATS( mask_dset->dblk->total_bytes, "mask dset" );
            INC_MEM_STATS( DSET_NVOX(mask_dset)*sizeof(byte), "ROI mask mask" );
            PRINT_MEM_STATS( "automask" );

            /* now unload the mask dset */
            DSET_unload( mask_dset );

            /* update memory stats to indicate that we have moved the data */
            DEC_MEM_STATS( mask_dset->dblk->total_bytes, "mask dset" );
            PRINT_MEM_STATS( "unload mask dset" );
        }
        else
        {
            /* if we are doing all, just set every voxel to 1 */
            if((roi_mask->mask=(byte*)malloc(
                DSET_NVOX(mask_dset)*sizeof(byte))) == NULL )
            {
                WARNING_message("Could not allocate %d bytes for"
                    " roi_mask->mask!", DSET_NVOX(mask_dset)*sizeof(byte));
                roi_mask = cc_free_roimask(roi_mask);
                DSET_unload(mask_dset);
                return(NULL);
            }

            /* update memory stats */
            INC_MEM_STATS( DSET_NVOX(mask_dset)*sizeof(byte), "ROI mask mask" );
            PRINT_MEM_STATS( "Alloc mask" );

            for(ii=0; ii<roi_mask->num_msk_vox; ii++)
            {
                roi_mask->mask[ii]=(byte)1;
            }
        }

    } /*if((auto_mask == 1) || ( use_all == 1 ))*/
    else
    {
        /* CC now we do the ROI mask processing */

        DSET_load(mask_dset); 
        if( !DSET_LOADED(mask_dset) )
        {
            WARNING_message("Could not load mask dataset.");
            return NULL ;
        }

        /* update memory stats */
        INC_MEM_STATS( mask_dset->dblk->total_bytes, "mask dset" );
        PRINT_MEM_STATS( "mask dset load" );

        if( DSET_NVALS(mask_dset) > 1 )
        {
            WARNING_message("Mask contains %d subbriks, only the first"
                " will be used.", DSET_NVALS(mask_dset));
        }

        /* we can handle shorts bytes and floats */
        switch( DSET_BRICK_TYPE(mask_dset, 0))
        {
            case MRI_float:
                WARNING_message("Floating point numbers will be rounded to"
                    "integers.");
            case MRI_byte:
            case MRI_short:
                /* this will be executed for both byte and short */
                mar = (void*)DSET_ARRAY(mask_dset, 0);
                break;
            default:
                WARNING_message("Unsupported brick data type (%d).",
                    DSET_BRICK_TYPE(mask_dset,0));
                break;
        }

        if( mar == NULL )
        {
            ERROR_message( "Error loading mask dataset?!??" );
        }
        else
        {

            /* allocate memory for the roi output structure */
            if( (roi_mask = (roi_mask_struct*)calloc(1,sizeof(roi_mask_struct)))
                == NULL )
            {
                WARNING_message("Could not allocate %d bytes for"
                    "roi_mask_struct!", sizeof(roi_mask_struct));
                DSET_unload(mask_dset);
                return(NULL);
            }

            /* update memory stats */
            INC_MEM_STATS( sizeof(roi_mask_struct), "ROI mask struct" );
            PRINT_MEM_STATS( "Alloc ROI mask struct" );

            /* if we have a user specified mask, not created by automask,
             * it may have ROIs embedded in the mask. We interpret areas
             * that contain the same integer value as belonging to the 
             * same ROI, and therefore the number of unique numbers is the
             * number of ROIs, we look for and extract embedded ROIS here */

            /* iterate through and determine the number of ROIs from
             * the number of unique values that we encounter */
            for( ii=0; ii < DSET_NVOX(mask_dset); ii++ )
            {
                roi_val = 0;
                /* make sure tht we are interpreting the value correctly */
                if( DSET_BRICK_TYPE(mask_dset,0) == MRI_byte )
                {
                    roi_val = (long)(((byte*)mar)[ii]);
                }
                else if( DSET_BRICK_TYPE(mask_dset,0) == MRI_short )
                {
                    roi_val = (long)(((short*)mar)[ii]);
                }
                else if( DSET_BRICK_TYPE(mask_dset,0) == MRI_float )
                {
                    roi_val = (long)roundl(((float*)mar)[ii]);
                }
                
                /* 0 values are considered to be outside of the part of 
                 * the image that we are interested in, negative values
                 * are considered to be artifacts */
                if (( roi_val > 0 ) && ( roi_val < SHRT_MAX+1 ))
                {
                    if( rois[roi_val] == 0 )
                    {
                        /* we have not seen this roi value before, 
                     * count it */
                        roi_count = roi_count + 1;
                        rois[ roi_val ] = rois[ roi_val ] + 1;
                    }
                    /* count the number of "in mask" voxels */
                    num_mask_vox = num_mask_vox + 1;
                }
                else if( roi_val < 0 )
                {
                    neg_values = neg_values + 1;
                }
                else if( roi_val == 0 )
                {
                    zero_values = zero_values + 1;
                }
                else if( roi_val > SHRT_MAX )
                {
                    oor_values = oor_values + 1;
                }
            } /* for( ii=0; ii < DSET_NVOX(mask_dset); ii++ ) */

            /* make sure that we have something to work with */
            if(( num_mask_vox > 0 ) && ( roi_count > 0 ))
            {
                /* set the header values */
                roi_mask->num_msk_vox = num_mask_vox;
                roi_mask->num_tot_vox = DSET_NVOX(mask_dset);
                roi_mask->num_rois = roi_count;

                /* allocate memory for ROI mapping and counts */
                if( (roi_mask->roi_map = calloc(roi_count,
                    sizeof(int))) == NULL )
                {
                    WARNING_message("Could not allocate %d bytes for",
                        "roi_mask->roi_map!", roi_count*sizeof(int));
                    roi_mask = cc_free_roimask(roi_mask);
                    DSET_unload(mask_dset);
                    return(NULL);
                }

                /* update memory stats */
                INC_MEM_STATS( roi_count*sizeof(int), "ROI mask roi_map" );
                PRINT_MEM_STATS( "Alloc ROI mask roi_map" );

                if( (roi_mask->roi_counts = calloc(roi_count,
                    sizeof(int))) == NULL )
                {
                    WARNING_message("Could not allocate %d bytes for"
                        " roi_mask->roi_counts!", roi_count*sizeof(int));
                    roi_mask = cc_free_roimask(roi_mask);
                    DSET_unload(mask_dset);
                    return(NULL);
                }

                /* update memory stats */
                INC_MEM_STATS( roi_count*sizeof(int), "ROI mask roi_count" );
                PRINT_MEM_STATS( "Alloc ROI mask roi_count" );

                /* establish a mapping between the actual ROI numbers and
                 * those that * we will use. This way we will deal with 
                 * non-consecutive numbered ROIS */
                roi_ndx = 0;
                for( ii = 1; ii < SHRT_MAX+1; ii++ )
                {
                    /* no reason to go past the number
                     * of ROIs that we found initially */
                    if( roi_ndx >= roi_count )
                    {
                        break;
                    }

                    if( rois[ii] != 0 )
                    {
                        /* set the mapping between the roi value and the 
                         * index we will use */
                        roi_mask->roi_map[ roi_ndx ] = ii;
                        /* get the count */
                        roi_mask->roi_counts[ roi_ndx ] = rois[ii];
                        /* replace the count with the index, to make
                         * a reverse mapping */
                        rois[ii] = roi_ndx+1;
                        /* on to the next roi */
                        roi_ndx = roi_ndx + 1;
                    }
                }

                /* now that we have the mapping, we can construct our mask */
                if((roi_mask->roi_mask=(unsigned short*)calloc(
                    num_mask_vox,sizeof(unsigned short))) == NULL )
                {
                    WARNING_message("Could not allocate %d bytes for"
                        " roi_mask->roi_mask!",
                        num_mask_vox*sizeof(unsigned short));
                    roi_mask = cc_free_roimask(roi_mask);
                    DSET_unload(mask_dset);
                    return(NULL);
                }

                /* update memory stats */
                INC_MEM_STATS( num_mask_vox*sizeof(unsigned int),
                    "ROI mask roi_mask" );
                PRINT_MEM_STATS( "Alloc roi_mask" );

                /* now that we have the mapping, we can construct our mask */
                if((roi_mask->mask=(byte*)calloc(
                    DSET_NVOX(mask_dset),sizeof(byte))) == NULL )
                {
                    WARNING_message("Could not allocate %d bytes for"
                        " roi_mask->mask!",
                        DSET_NVOX(mask_dset)*sizeof(byte));
                    roi_mask = cc_free_roimask(roi_mask);
                    DSET_unload(mask_dset);
                    return(NULL);
                }

                /* update memory stats */
                INC_MEM_STATS( DSET_NVOX(mask_dset)*sizeof(byte),
                    "ROI mask mask" );
                PRINT_MEM_STATS( "Alloc mask" );

                /* now populate the mask with the newly created ROI indices*/
                ivox = 0;
                for( ii = 0; ii < DSET_NVOX(mask_dset); ii++ )
                {
                    roi_val = 0;
                    /* make sure tht we are interpreting the value correctly */
                    if( DSET_BRICK_TYPE(mask_dset,0) == MRI_byte )
                    {
                        roi_val = (long)(((byte*)mar)[ii]);
                    }
                    else if( DSET_BRICK_TYPE(mask_dset,0) == MRI_short )
                    {
                        roi_val = (long)(((short*)mar)[ii]);
                    }
                    else if( DSET_BRICK_TYPE(mask_dset,0) == MRI_float )
                    {
                        roi_val = (long)roundl(((float*)mar)[ii]);
                    }
                
                    /* again make sure we are dealing with an "in brain" and
                     * non-negative voxel */
                    if (( roi_val > 0 ) && ( roi_val < SHRT_MAX+1 ))
                    {
                        roi_mask->mask[ ii ] = 1;

                        /* make sure that we don't overrun the inverse
                         * mapping */
                        if( ivox < num_mask_vox )
                        {
                            /* set the mask value to the roi index from
                             * our reverse mapping */
                            roi_mask->roi_mask[ ivox ] = rois[ roi_val ];

                            /* increment the number of voxels */
                            ivox = ivox+1;
                        }
                        else
                        {
                            WARNING_message("Exceeded imap bound!!");
                        }
                    }
                } /* for( ii = 0; ii < DSET_NVOX(mask_dset); ii++ ) */
            } /* if(( num_mask_vox > 0 ) && ( roi_count > 0 )) */
        } /* if( mar == NULL ) ... else */

        /* print out info for the user */
        INFO_message( "%d voxels and %d ROIs found in mask dataset after"
            " excluding %d 0s, %d negative, and %d out-of-range values ",
            num_mask_vox, roi_count, zero_values, neg_values, oor_values );

        DSET_unload(mask_dset);

        /* update memory stats */
        DEC_MEM_STATS( mask_dset->dblk->total_bytes, "mask dset" );
        PRINT_MEM_STATS( "mask dset unload" );

    } /*if((auto_mask == 1) || ( use_all == 1 )) ... else */

    return( roi_mask );
}

/* data structure to hold the "conformed" data, which 
 * is the nueroimaging data that has been masked, 
 * standardized, and possibly averaged into ROI
 * time courses. */
typedef struct _conformed_data_struct
{
    long num_seeds;
    long num_targets;
    long num_observations;
    MRI_vectim* seed_data;
    MRI_vectim* target_data;

} conformed_data_struct;

conformed_data_struct** cc_free_conformed( conformed_data_struct** data, long ndsets )
{
    long ii=0;
    if( data != NULL )
    {
        for(ii=0;ii<ndsets;ii++)
        {
            if( data[ii]->seed_data != NULL )
            {
                if( data[ii]->seed_data == data[ii]->target_data )
                {
                    /* -- set to NULL to avoid double freeing */
                    data[ii]->target_data = NULL;
                }

                /*-- CC update our memory stats to reflect vectim -- */
                DEC_MEM_STATS((data[ii]->seed_data->nvec*sizeof(int)) +
                              ((data[ii]->seed_data->nvec)*
                               (data[ii]->seed_data->nvals))*
                               sizeof(float) +
                               sizeof(MRI_vectim), "seed vectim");
                PRINT_MEM_STATS( "seed vectim destroy" );

                VECTIM_destroy(data[ii]->seed_data);
                data[ii]->seed_data = NULL;
            }

            if( data[ii]->target_data != NULL )
            {
                /*-- CC update our memory stats to reflect vectim -- */
                DEC_MEM_STATS((data[ii]->target_data->nvec*sizeof(int)) +
                              ((data[ii]->target_data->nvec)*
                               (data[ii]->target_data->nvals))*
                               sizeof(float) +
                               sizeof(MRI_vectim), "target vectim");
                PRINT_MEM_STATS( "target vectim destroy" );

                VECTIM_destroy(data[ii]->target_data);
                data[ii]->target_data = NULL;
            }

            free(data[ii]);

            /*-- CC update our memory stats to reflect freeing 
             * conformed data structure -- */
            DEC_MEM_STATS( sizeof(conformed_data_struct), "conformed_data");
            PRINT_MEM_STATS( "free conformed data struct" );
        }

        free(data);
        DEC_MEM_STATS( ndsets*sizeof(conformed_data_struct*), "conformed_data");
        PRINT_MEM_STATS( "free conformed data struct" );
        data = NULL;
    }

    return(data);
}
/* extract data from neuroimages and conform so that we can use
 * it for CWAS calculations. This involves removing out of brain
 * data, detrending the data, normalizing the variance, and
 * averaging the data into ROI time series if so desired */
conformed_data_struct* cc_mask_std_avg( THD_3dim_dataset* inset,
    roi_mask_struct* roi_mask, long do_seed_roi, long do_target_roi,
    long polort )
{

    long roi_val = 0;
    long ii = 0;
    long jj = 0;
    float* tar = NULL;
    float* rar = NULL;
    double frac = 0.0;
    MRI_vectim *xvectim = NULL;
    MRI_vectim *roi_vectim = NULL;
    conformed_data_struct* conformed_data = NULL;

    DSET_load(inset); 
    CHECK_LOAD_ERROR(inset);

    /* update memory stats */
    INC_MEM_STATS( inset->dblk->total_bytes, "in dset" );
    PRINT_MEM_STATS( "input dset load" );

    /*-- create vectim from input dataset --*/
    INFO_message("vectim-izing input dataset") ;

    /*-- CC added in mask to reduce the size of xvectim -- */
    xvectim = THD_dset_to_vectim( inset , roi_mask->mask , 0 ) ;
    if( xvectim == NULL )
    {
        WARNING_message("Can't create xvectim?!");
        DSET_unload(inset) ;
        return(NULL);
    }

    /*-- CC update our memory stats to reflect vectim -- */
    INC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                    ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                    sizeof(MRI_vectim), "xvectim");
    PRINT_MEM_STATS( "vectim" );

    /* -- CC unloading the dataset to reduce memory usage  -- 
     * THD_dset_to_vectim doesn't appear to unload the data
     * although it does load it */
    DEC_MEM_STATS( inset->dblk->total_bytes, "in dset" );
    PRINT_MEM_STATS( "input dset unload" );
    DSET_unload(inset) ;

    /* -- CC perform detrending -- */
    if( polort >= 0 )
    {
        INFO_message( "Detrending with polort = %d\n", polort );
        for( ii=0 ; ii < xvectim->nvec ; ii++ )
        {  
            /* remove polynomial trend */
            DETREND_polort(polort,xvectim->nvals,VECTIM_PTR(xvectim,ii));
        }
    }

    /* -- CC normalize input data to zero mean and unit variance
          this procedure does not change time series that 
          have zero variance -- */
    THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

    /* calculate ROI averages if specified */
    if( (do_seed_roi == 1) || (do_target_roi == 1) )
    {
        /* allocated a vectim to hold the ROI averages */
        MAKE_VECTIM( roi_vectim, roi_mask->num_rois, xvectim->nvals );
        if( !ISVALID_VECTIM(roi_vectim) )
        {
            WARNING_message("Could not allocate roi_vectim!");
            return(NULL);
        }

        /*-- CC update our memory stats to reflect vectim -- */
        INC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float)+
                        sizeof(MRI_vectim), "ROI vectim");
        PRINT_MEM_STATS( "ROI vectim" );

        /* iterate over voxel time courses and calculate ROI averages */
        for( ii=0; ii < xvectim->nvec; ii++ )
        {
            tar = VECTIM_PTR(xvectim,ii);

            /* get the index for the ROI corresponding to this voxel.
             * remember to subtract 1 to avoid overflow */
            roi_val = (roi_mask->roi_mask[ii] - 1);
            if(( roi_val >= 0 ) && (roi_val < roi_mask->num_rois ))
            {
                frac = (double)(1.0 / roi_mask->roi_counts[roi_val]);
                rar = VECTIM_PTR(roi_vectim,roi_val);
            }
            else
            {
                WARNING_message("ROI number %d is out of bounds (%d).",
                    roi_val, roi_mask->num_rois);
            }

            for(jj=0;jj<xvectim->nvals;jj++)
            {
                rar[jj] = rar[jj] + (frac * tar[jj]);
            }
        }

        /* normalize the ROI_vectim */
        THD_vectim_normalize(roi_vectim) ;  /* L2 norm = 1 */
    }

    if((conformed_data = (conformed_data_struct*)calloc(1,
        sizeof(conformed_data_struct))) == NULL)
    {
        WARNING_message("Could not allocate conformed data structure!");
        if(xvectim!=NULL) VECTIM_destroy(xvectim);
        if(roi_vectim!=NULL) VECTIM_destroy(roi_vectim);
        return(NULL);
    }

    /*-- CC update our memory stats to reflect vectim -- */
    INC_MEM_STATS(sizeof(conformed_data_struct), "conformed_data");
    PRINT_MEM_STATS( "alloc conformed data" );

    /* populate the conformed data structure */
    conformed_data->num_observations = xvectim->nvals;

    /* if we are not using ROIs, free the voxel data */
    if(( do_seed_roi == 1 ) && ( do_target_roi == 1 ))
    {
        conformed_data->seed_data = roi_vectim;
        conformed_data->num_seeds = roi_vectim->nvec;

        conformed_data->target_data = roi_vectim;
        conformed_data->num_targets = roi_vectim->nvec;

        /*-- CC update our memory stats to reflect moving ROI vectim
         *   to seed vectim -- */
        DEC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "ROI vectim");
        INC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "seed vectim");
        PRINT_MEM_STATS( "move roi vectim to seed vectim" );

        /*-- CC update our memory stats to reflect vectim -- */
        DEC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "xvectim");
        PRINT_MEM_STATS( "xvectim destroy" );

        VECTIM_destroy(xvectim);
    }
    else if(( do_seed_roi == 0 ) && ( do_target_roi == 0 ))
    {
        conformed_data->seed_data = xvectim;
        conformed_data->num_seeds = xvectim->nvec;

        conformed_data->target_data = xvectim;
        conformed_data->num_targets = xvectim->nvec;

        /*-- CC update our memory stats to reflect moving ROI vectim
         *   to seed vectim -- */
        DEC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "xvectim");
        INC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "seed vectim");
        PRINT_MEM_STATS( "move xvectim to seed vectim" );
    }
    /* if we are not using ROIs, free the voxel data */
    else if(( do_seed_roi == 1 ) && ( do_target_roi == 0 ))
    {
        conformed_data->seed_data = roi_vectim;
        conformed_data->num_seeds = roi_vectim->nvec;

        /*-- CC update our memory stats to reflect moving ROI vectim
         *   to seed vectim -- */
        DEC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "ROI vectim");
        INC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "seed vectim");
        PRINT_MEM_STATS( "move roi vectim to seed vectim" );

        conformed_data->target_data = xvectim;
        conformed_data->num_targets = xvectim->nvec;

        /*-- CC update our memory stats to reflect vectim -- */
        DEC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "xvectim");
        INC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "target vectim");
        PRINT_MEM_STATS( "move xvectim to target vectim" );
    }
    /* if we are not using ROIs, free the voxel data */
    else if(( do_seed_roi == 0 ) && ( do_target_roi == 1 ))
    {
        conformed_data->seed_data = xvectim;
        conformed_data->num_seeds = xvectim->nvec;

        /*-- CC update our memory stats to reflect vectim -- */
        DEC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "xvectim");
        INC_MEM_STATS((xvectim->nvec*sizeof(int)) +
                        ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "seed vectim");
        PRINT_MEM_STATS( "move xvectim to seed vectim" );

        conformed_data->target_data = roi_vectim;
        conformed_data->num_targets = roi_vectim->nvec;

        /*-- CC update our memory stats to reflect moving ROI vectim
         *   to seed vectim -- */
        DEC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "ROI vectim");
        INC_MEM_STATS((roi_vectim->nvec*sizeof(int)) +
                        ((roi_vectim->nvec)*(roi_vectim->nvals))*sizeof(float) +
                        sizeof(MRI_vectim), "target vectim");
        PRINT_MEM_STATS( "move roi vectim to target vectim" );
    }

    return(conformed_data);
}

/* 3dCWAS was created from 3dAutoTCorrelate by
   R. Cameron Craddock */

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
    THD_3dim_dataset *xset = NULL;
    THD_3dim_dataset *yset = NULL;
    THD_3dim_dataset *cset = NULL;
    THD_3dim_dataset *mset = NULL ;
    int nopt=1 , method=PEARSON , do_autoclip=0 ;
    int nvox , nvals , polort=1 ;
    char *prefix = "ECM" ;
    int   nmask , abuc=1 ;
    char str[STRLEN] , *cpt = NULL;
    int *mask_ndx_to_vol_ndx = NULL;
    MRI_vectim *xvectim = NULL ;

    roi_mask_struct* roi_mask = NULL;
    /* CC - flag to specify that the 
     * mask contains ROIs */
    int do_maskROIs = 0;

    /* CC - flags that control options */
    double  scale = 1.0;
    double  shift = 0.0;
    double  thresh = -1.2;
    double  sparsity = 100.0;
    long    mem_bytes = (long)2147483648;

    /* CC - flags to control behaviour */
    long do_scale = 0;
    long do_shift = 0;
    long do_sparsity = 0;
    long do_thresh = 0;
    long do_write_mask = 0;
    long do_write_intermediate = 1;
    long do_seed_roi = 0;
    long do_target_roi = 0;

    /* CC - iteration stopping criteria */
    long max_iter = 1000;
    double eps = 0.0001;

    /* CC - vectors to hold the results (bin/wght) */
    double* eigen_vec[2];

    /* CC - we will have two subbricks: binarized and weighted */
    int nsubbriks = 2;
    int subbrik = 0;
    short * wodset;
    float * fodset;

    /* covariates */
    NI_element   *covnel=NULL;       
    NI_str_array *covlab=NULL;
    int num_covset_col = 0;
    int mcov = 0;

    /* voxel/ROI indices */
    long ii = 0;
    long jj = 0;
    long kk = 0;
    long ll = 0;
    long ivox = 0;
    long iroi = 0;
    long num_calc = 0;

    /* time indices */
    long m = 0;
    long n = 0;

    int nbad = 0;
    char **infiles=NULL;
    int num_infiles = 0;

    float* xseed = NULL;
    float* xtarget = NULL;
    float* yseed = NULL;
    float* ytarget = NULL;

    double* out_r = NULL;
    double* data_r = NULL;
    double r_temp;
    double r_std;
    double r_mean;
    double r_sum;
    double r_sum_sq;

   /*----*/
   conformed_data_struct** dset_conformed=NULL;

/*
   AFNI_SETUP_OMP(0) ;*/  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dCWAS [options] dset\n"
"  Computes voxelwise local functional connectivity density and\n"
"  stores the result in a new 3D bucket dataset as floats to\n"
"  preserve their values. ECM reflects the strength and\n"
"  extent of a voxel's global connectivity as well as the\n"
"  importance of the voxels that it is directly connected to.\n\n"
"  Conceptually the process involves: \n"
"      1. Calculating the correlation between voxel time series for\n"
"         every pair of voxels in the brain (as determined by masking)\n"
"      2. Calculate the eigenvector corresponding to the largest\n"
"         eigenvalue of the similarity matrix.\n\n" 
"  Guaranteeing that this eigenvector is unique and all positive\n"
"  requires that the similarity matrix is strictly positive. This\n"
"  is enforced by either adding one to the correlations (Lohmann \n"
"  et. al. 2010), or by adding one and dividing by two (Wink et al.\n"
"  2012).\n\n" 
"  Practically the power iteration algorithm described in Wink et\n"
"  al. 2012) is used to optimize for computational time and memory\n"
"  usage.\n\n"
"  Lohmann G, Margulies DS, Horstmann A, Pleger B, Lepsien J, et al.\n"
"      (2010) Eigenvector Centrality Mapping for Analyzing\n"
"      Connectivity Patterns in fMRI Data of the Human Brain. PLoS\n"
"      ONE 5(4): e10232. doi: 10.1371/journal.pone.0010232\n\n"
"  Wink, A. M., de Munck, J. C., van der Werf, Y. D., van den Heuvel,\n"
"      O. A., & Barkhof, F. (2012). Fast Eigenvector Centrality\n"
"      Mapping of Voxel-Wise Connectivity in Functional Magnetic\n"
"      Resonance Imaging: Implementation, Validation, and\n"
"      Interpretation. Brain Connectivity, 2(5), 265–274.\n"
"      doi:10.1089/brain.2012.0087\n\n"
"\n"
"Options:\n"
"  -full       = uses the full power method (Lohmann et. al. 2010).\n"
"                Enables the use of thresholding and calculating\n"
"                thresholded centrality. Uses sparse array to reduce \n"
"                memory requirement. Automatically selected if \n"
"                -thresh, or -sparsity are used.\n"
"  -fecm       = uses a shortcut that substantially speeds up \n"
"                computation, but is less flexibile in what can be\n"
"                done the similarity matrix. i.e. does not allow \n"
"                thresholding correlation coefficients. based on \n" 
"                fast eigenvector centrality mapping (Wink et. al\n"
"                2012). Default when -thresh, or -sparsity\n"
"                are NOT used.\n"
"  -thresh r   = exclude connections with correlation < r. cannot be\n"
"                used with FECM\n"
"  -sparsity p = only include the top p%% connectoins in the calculation\n"
"                cannot be used with FECM method. (default = 100)\n"
"  -shift s    = value that should be added to correlation coeffs to\n"
"                enforce non-negativity, s >= 0. [default = 0.0, unless\n"
"                -fecm is specified in which case the default is 1.0\n"
"                (e.g. Wink et al 2012)].\n"
"  -scale x    = value that correlation coeffs should be multiplied by\n"
"                after shifting, x >= 0 [default = 1.0, unless -fecm is\n"
"                specified in which case the default is 0.5 (e.g. Wink et\n"
"                al 2012)].\n"
"  -eps p      = sets the stopping criterion for the power iteration\n"
"                l2|v_old - v_new| < eps*|v_old|. default = .001 (0.1%%)\n"
"  -max_iter i = sets the maximum number of iterations to use in\n"
"                in the power iteration. default = 1000\n"
"\n"
"  -polort m   = Remove polynomical trend of order 'm', for m=0..3.\n"
"                [default is m=1; removal is by least squares].\n"
"                Using m=0 means that just the mean is removed.\n"
"\n"
"  -autoclip   = Clip off low-intensity regions in the dataset,\n"
"  -automask   = so that the correlation is only computed between\n"
"                high-intensity (presumably brain) voxels.  The\n"
"                mask is determined the same way that 3dAutomask works.\n"
"\n"
"  -mask mmm   = Mask to define 'in-brain' voxels. Reducing the number\n"
"                the number of voxels included in the calculation will\n"
"                significantly speedup the calculation. Consider using\n"
"                a mask to constrain the calculations to the grey matter\n"
"                rather than the whole brain. This is also preferrable\n"
"                to using -autoclip or -automask.\n"
"\n"
"  -prefix p   = Save output into dataset with prefix 'p'\n"
"                [default prefix is 'ecm'].\n"
"\n"
"  -memory G   = Calculating eignevector centrality can consume alot\n"
"                of memory. If unchecked this can crash a computer\n"
"                or cause it to hang. If the memory hits this limit\n"
"                the tool will error out, rather than affecting the\n"
"                system [default is 2G].\n"
"\n"
"Notes:\n"
" * The output dataset is a bucket type of floats.\n"
" * The program prints out an estimate of its memory used\n"
"    when it ends.  It also prints out a progress 'meter'\n"
"    to keep you pacified.\n"
"\n"
"-- RWCox - 31 Jan 2002 and 16 Jul 2010\n"
"-- Cameron Craddock - 13 Nov 2015 \n"
"-- Daniel Clark - 14 March 2016\n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dECM",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

    mainENTRY("3dCWAS main"); machdep(); PRINT_VERSION("3dCWAS");
    AFNI_logger("3dCWAS",argc,argv);

    /*-- option processing --*/
    while( nopt < argc && argv[nopt][0] == '-' )
    {

        if( strcmp(argv[nopt],"-time") == 0 )
        {
            abuc = 0 ; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-autoclip") == 0 ||
            strcmp(argv[nopt],"-automask") == 0   )
        {
            do_autoclip = 1 ; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-mask") == 0 )
        {
            /* mset is opened here, but not loaded? */
            mset = THD_open_dataset(argv[++nopt]);
            CHECK_OPEN_ERROR(mset,argv[nopt]);
            nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-rois") == 0 )
        {
            do_maskROIs = 1;
            nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-writemask") == 0 )
        {
            do_write_mask = 1;
            nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-sparsity") == 0 )
        {
            double val = (double)strtod(argv[++nopt],&cpt) ;
            if( *cpt != '\0' || val < 0 || val > 100 )
            {
                ERROR_EXIT_CC("Illegal value after -sparsity!") ;
            }
            sparsity = val ; do_sparsity = 1; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-thresh") == 0 )
        {
            double val = (double)strtod(argv[++nopt],&cpt) ;
            if( *cpt != '\0' || val < -1.1 || val > 1 )
            {
                ERROR_EXIT_CC("Illegal value after -thresh!") ;
            }
            thresh = val ; do_thresh = 1; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-memory") == 0 )
        {
            double val = (double)strtod(argv[++nopt],&cpt) ;
            if( *cpt != '\0' || val < 0 )
            {
                ERROR_EXIT_CC("Illegal value after -memory!") ;
            }
            mem_bytes = (long)(ceil(val * (double)1024)*1024*1024);
            nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-scale") == 0 )
        {
            double val = (double)strtod(argv[++nopt],&cpt) ;
            if( *cpt != '\0' || val < 0 )
            {
                ERROR_EXIT_CC("Illegal value after -scale!") ;
            }
            scale = val ; do_scale = 1; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-shift") == 0 )
        {
            double val = (double)strtod(argv[++nopt],&cpt) ;
            if( *cpt != '\0' || val < 0 )
            {
                ERROR_EXIT_CC("Illegal value after -shift!") ;
            }
            shift = val ; do_shift = 1; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-prefix") == 0 )
        {
            prefix = strdup(argv[++nopt]) ;
            if( !THD_filename_ok(prefix) )
            {
                ERROR_EXIT_CC("Illegal value after -prefix!") ;
            }
            nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-polort") == 0 )
        {
            int val = (int)strtod(argv[++nopt],&cpt) ;
            if( *cpt != '\0' || val < 0 || val > 3 )
            {
                ERROR_EXIT_CC("Illegal value after -polort!") ;
            }
            polort = val ; nopt++ ; continue ;
        }

        if( strcmp(argv[nopt],"-mem_stat") == 0 )
        {
            MEM_STAT = 1 ; nopt++ ; continue ;
        }

        if( strncmp(argv[nopt],"-mem_profile",8) == 0 )
        {
            MEM_PROF = 1 ; nopt++ ; continue ;
        }

        if( strncmp(argv[nopt],"-seed_roi",8) == 0 )
        {
            do_seed_roi = 1 ; nopt++ ; continue ;
        }

        if( strncmp(argv[nopt],"-target_roi",8) == 0 )
        {
            do_target_roi = 1 ; nopt++ ; continue ;
        }

        /* this code was "borrowed" (stolen) from 3dttest++.c */
        if( strcasecmp(argv[nopt],"-config") == 0 )
        {
            /* temporary variables used for processing the
             * -config command line arguement */
            char *lab;
            float sig;
            float men;
            char nlab[2048];
            char dlab[2048];

            if( ++nopt >= argc )
            {
                ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
            }

            if( covnel != NULL )
            {
                ERROR_exit("can't use -config twice!");
            }

            if((covnel = THD_mixed_table_read( argv[nopt] )) == NULL )
            {
                ERROR_message("Can't read table from -config file '%s'",
                    argv[nopt]);
                ERROR_message("Try re-running this program with the extra"
                    "option -DAFNI_DEBUG_TABLE=YES");
                ERROR_exit(   "Can't continue after the above error!") ;
            }

            INFO_message("Covariates file: %d columns, each with %d rows",
                         covnel->vec_num , covnel->vec_len ) ;

            /* count and validate the number of covariates */
            mcov = covnel->vec_num - 1;
            if( mcov < 1 )
            {
                ERROR_exit("Need at least 2 columns in -config file!");
            }
            else if( mcov > MAXCOV )
            {
                ERROR_exit("%d covariates in file, more than max allowed (%d)",
                    mcov,MAXCOV);
            }
 
            /* decode and validate labels from the covariates file */
            lab = NI_get_attribute( covnel , "Labels" );
            if( lab != NULL )
            {
                ININFO_message("Covariate column labels: %s",lab);
                covlab = NI_decode_string_list( lab , ";," );
                if( covlab == NULL || covlab->num < mcov+1 )
                {
                    ERROR_exit("can't decode labels properly?!");
                }
            }
            else
            {
                ERROR_exit("Can't get labels from -config file '%s'",
                    argv[nopt]);
            }

            /* go through and validate the columns of covariate file */
            nbad=0;

            /* first check that the files exist and are accessible */
            if( covnel->vec_typ[0] == NI_STRING )
            {
                /* get a pointer to the colum */
                infiles = (char **)covnel->vec[jj];

                /* search through the dataset labels and count ones that 
                 * are the same */
                for( kk=0 ; kk < covnel->vec_len ; kk++ )
                {
                    if( access(infiles[kk],R_OK) == -1 )
                    {
                        printf("Could not access file %ld, %s\n", kk,
                            infiles[kk]);
                        perror("Error");
                        nbad = nbad+1;
                    }

                    for( jj=kk+1 ; jj < covnel->vec_len ; jj++ )
                    {
                        if( strcmp(infiles[kk],infiles[jj]) == 0 )
                        {
                            ERROR_message(
                                "Files %d and %d: are duplicates (%s)",
                                jj,kk,infiles[kk]);
                            nbad=nbad+1;
                        }
                    }
                }

                num_infiles=covnel->vec_len;
                if( num_infiles < 3 )
                {
                    ERROR_message("Need at least 3 input files, only found %d",
                        num_infiles);
                    nbad=nbad+1;
                }
            }

            /* make strings from the column labels, we will use strcat which
             * requires the strings be terminated by '\0', so make sure that
             * the output strings are so terminated */
            nlab[0] = '\0'; /* will hold string columns of covariate labels */
            dlab[0] = '\0'; /* will hold list of datasets to be used */

            for(jj=1; jj <= mcov; jj++ )
            {
                if( covnel->vec_typ[jj] == NI_FLOAT )
                {  
                    /* numeric column */

                    /* verify that the column has non-zero variance 
                     * (i.e. is not a constant) */
                    meansigma_float(covnel->vec_len,(float *)covnel->vec[jj],
                        &men,&sig) ;
                    if( sig <= 0.0f )
                    {
                        ERROR_message( "Numeric covariate column '%s'"
                            " is constant '%f'; can't be used!",
                            covlab->str[jj], men ); 
                        /* keep count of the errors */
                        nbad++;
                    }

                    /* aggregate the column labels */
                    strcat(nlab,covlab->str[jj]);
                    strcat(nlab," "); /* add in a space to make it pretty */
                }
                else if( covnel->vec_typ[0] == NI_STRING )
                {
                    /* string column: */
                    /* first check that the files exist and are accessible */

                    /* get a pointer to the colum */
                    char **qpt = (char **)covnel->vec[jj];
                    int nsame = 1;
                    for( kk=0 ; kk < covnel->vec_len ; kk++ )
                    {
                        if( access(qpt[kk],R_OK) == -1 )
                        {
                            printf("Could not access file %ld, %s\n", kk,
                                qpt[kk]);
                            perror("Error");
                            nbad = nbad+1;
                        }

                    }
                    for( kk=1 ; kk < covnel->vec_len ; kk++ )
                    {
                         if( strcmp(qpt[0],qpt[kk]) == 0 )
                         {
                             nsame++;
                         }
                    }

                    /* see if they are all the same */
                    if( nsame == covnel->vec_len )
                    {
                         ERROR_message( "Dataset covariate column '%s' is"
                             " constant '%s'; can't be used!", covlab->str[jj],
                             qpt[0] );
                         nbad++;
                    }

                    /* count number of covariance columns */
                    num_covset_col++; 

                    strcat(dlab,covlab->str[jj]) ; strcat(dlab," ") ;
                } /* if( covnel->vec_typ[jj] == NI_FLOAT ); else */

                /* left truncate string?? */
                LTRUNC(covlab->str[jj]) ;
            } /*for(nbad=0,jj=1; jj <= mcov; jj++ )*/

            /* we cannot tolerate any errors */
            if( nbad > 0 )
            {
                ERROR_exit("Cannot continue past above ERROR%s :-(",
                                      (nbad==1) ? "\0" : "s" );
            }

            /* provide the user feedback on whate we found in the covariate
             * file */
            ININFO_message("Calculating CWAS using %d input files.",
                num_infiles);
            if( mcov-num_covset_col > 0 )
            {
                ININFO_message("  %d numeric covariate%s: %s",
                    mcov-num_covset_col, (mcov-num_covset_col==1) ? "\0" : "s",
                    nlab );
            }

            if( num_covset_col > 0 )
            {
                ININFO_message("  %d dataset covariate%s: %s",
                          num_covset_col ,
                          (num_covset_col==1) ? "\0" : "s" , dlab ) ;
            }
            nopt++ ; continue ;
        } /* if( strcasecmp(argv[nopt],"-config") == 0 ) */

        ERROR_EXIT_CC("Illegal option: %s",argv[nopt]) ;
    }

    if( covnel == NULL ) ERROR_EXIT_CC("Need a configuration"
        " (-config <filename>) on command line!?") ;

    /* CC - first thing is to take care of the mask.
     *
     * I am potentially making this more difficult than it needs to
     * be but I would like for users to be able to specify a ROI
     * file instead of the mask that would reduce the dimensionality
     * of the problem */
    /* if a mask was specified make sure it is appropriate */
    if( mset )
    {
        roi_mask  = cc_create_roimask(mset, do_autoclip, 0) ;
        nmask = roi_mask->num_msk_vox;
    } 
    /* if automasking is requested, handle that now */
    else if( do_autoclip )
    {
        /* if autoclip is requested and no mask is provided, we 
         * use the first input file to create a mask */
        mset = THD_open_dataset(infiles[0]);
        CHECK_OPEN_ERROR(mset,infiles[0]);
        roi_mask  = cc_create_roimask(mset, do_autoclip, 0) ;
        nmask = roi_mask->num_msk_vox;

        INFO_message("%d voxels survive -autoclip",nmask) ;
        if( nmask < 2 ) ERROR_EXIT_CC("Only %d voxels in -automask!",nmask);
    }
    /* otherwise we use all of the voxels in the image */
    else 
    {
        mset = THD_open_dataset(infiles[0]);
        CHECK_OPEN_ERROR(mset,infiles[0]);
        roi_mask  = cc_create_roimask(mset, do_autoclip, 1) ;
        nmask = DSET_NVOX(mset) ;
    }

    if( nmask < 2 )
    {
        ERROR_EXIT_CC("Only %d voxels in -mask, exiting...",nmask);
    }
    else
    {
        INFO_message("Including %d voxels in the computation",nmask);
    }

    if( do_write_mask == 1 )
    {
        /* If the user asks, write out the mask file for debugging */
        snprintf(str, STRLEN, "mask_%s", prefix);


        /*-- create output dataset --*/
        cset = EDIT_empty_copy( mset ) ;

        /*-- configure the output dataset */
        printf( "creating output bucket(%s)\n", "mask_out.nii.gz" );
        EDIT_dset_items( cset ,
            ADN_prefix    , str ,
            ADN_nvals     , 1              , /*  subbricks */
            ADN_ntt       , 0              , /* no time axis */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_BUCK_TYPE ,
            ADN_datum_all , MRI_short     ,
            ADN_none ) ;

        /* add history information to the hearder */
        tross_Make_History( "3dCWAS" , argc,argv , cset ) ;

        ININFO_message("creating output dataset in memory") ;
    
        subbrik = 0;
        EDIT_BRICK_TO_NOSTAT(cset,subbrik); /* stat params  */
        EDIT_BRICK_FACTOR(cset,subbrik,1.0);/* scale factor */
        snprintf(str,STRLEN,"mask");

        EDIT_BRICK_LABEL(cset,subbrik,str) ;
        EDIT_substitute_brick(cset,subbrik,MRI_short,NULL) ;   /* make array  */

        /* copy measure data into the subbrik */
        wodset = (short*)DSET_ARRAY(cset,subbrik);
        ININFO_message("copying mask") ;

        /* write out the mask data, if we didn't create a mask or receive a mask
         * in the input arguements, we put a 1 in every voxel */
        if (roi_mask != NULL )
        {
            for(ii=0;ii<DSET_NVOX(mset);ii++)
            {
                wodset[ii] = (short)roi_mask->mask[ii];
            }
        }
        else
        {
            for(ii=0;ii<DSET_NVOX(mset);ii++)
            {
                wodset[ii] = (short)1;
            }
        }
        ININFO_message("finished copying mask") ;

        /* write the dataset */
        DSET_write(cset);
        WROTE_DSET(cset);

        /* free up the output dataset memory */
        DSET_unload(cset);
        DSET_delete(cset) ;
    }

    /* we are now done with the mask dataset and can free it */
    DSET_unload(mset);
    DSET_delete(mset);
    mset=NULL;

    /* allocate space for input datasets */
    if(( dset_conformed = (conformed_data_struct**)calloc( num_infiles,
        sizeof(conformed_data_struct*))) == NULL )
    {
        ERROR_EXIT_CC("Could not allocate space for conformed datasets.");
    }

    INC_MEM_STATS( num_infiles*sizeof(conformed_data_struct*), "dset_conformed");
    PRINT_MEM_STATS( "allocate dset_conformed" );

    ININFO_message("Loading datasets");

    /* load in the data */
    for( ii = 0; ii < num_infiles; ii++)
    {
        /* open the dataset */
        xset = THD_open_dataset(infiles[ii]);
        CHECK_OPEN_ERROR(xset,infiles[ii]);

        /* basic (weak) check to make sure the input data is
         * consistent with the mask */
        if( DSET_NVOX(xset) != roi_mask->num_tot_vox)
        {
            ERROR_message("(%d != %d)", DSET_NVOX(xset), roi_mask->num_tot_vox);
            ERROR_EXIT_CC("Input and mask dataset differ in number of voxels!");
        }

        ININFO_message("masking, standardizing and averaging %s", infiles[ii]);

        /* conform the dataset */
        dset_conformed[ii] = cc_mask_std_avg( xset, roi_mask, do_seed_roi, 
            do_target_roi, polort );

        /* we are now done with the mask dataset and can free it */
        DSET_unload(xset);
        DSET_delete(xset);
        xset=NULL;
    }

    /* allocate space for x correlations */
    if(( data_r = (double*)calloc( num_infiles * dset_conformed[0]->num_targets,
        sizeof(double))) == NULL )
    {
        ERROR_EXIT_CC("Could not allocate space for FC correlations.");
    }

    INC_MEM_STATS( num_infiles*dset_conformed[0]->num_targets*sizeof(double), "data_r");
    PRINT_MEM_STATS( "allocate data correlation vector" );

    /* num calculations */
    num_calc = (long)round(.5 * (double)num_infiles * (double)(num_infiles - 1));

    /* allocate output space for the correlations */
    if(( out_r = (double*)calloc( num_calc*dset_conformed[0]->num_seeds, 
        sizeof(double))) == NULL )
    {
        ERROR_EXIT_CC("Could not allocate space for output correlations.");
    }

    INC_MEM_STATS( num_calc*dset_conformed[0]->num_seeds*sizeof(double), "out_r");
    PRINT_MEM_STATS( "allocate out correlation vector" );

    /* tell the user what we are up to */
    ININFO_message( "Calculating CWAS with %d seeds and %d targets",
        do_seed_roi == 1 ? roi_mask->num_rois : roi_mask->num_msk_vox,
        do_target_roi == 1 ? roi_mask->num_rois : roi_mask->num_msk_vox);

    for( kk = 0; kk < dset_conformed[0]->num_seeds; kk++ )
    {
        /* start by calculating the fc maps */
        for( ii = 0; ii < num_infiles; ii++)
        {
            xseed = VECTIM_PTR(dset_conformed[ii]->seed_data,kk);

            r_mean = (double)0.0;
            r_sum = (double)0.0;
            r_sum_sq = (double)0.0;

            for( ll = 0; ll < dset_conformed[ii]->num_targets; ll++ )
            {
                xtarget = VECTIM_PTR(dset_conformed[ii]->target_data,ll);

                r_temp = (double)0.0;

                /* iterate over the xset observations, this will be the rows
                 * of the inner mult matrix */
                for( m = 0; m < dset_conformed[ii]->num_observations; m++ )
                {
                    r_temp += xseed[m] * xtarget[m];
                }

                /* fisher transform the result */
                if( r_temp >= 1.0 )
                {
                    r_temp = 20.0;
                }
                else
                {
                    r_temp =  .5 * (log(1+r_temp) - log(1-r_temp));
                }

                r_sum += r_temp;
                r_sum_sq += r_temp * r_temp;
                
                data_r[ii*dset_conformed[ii]->num_targets+ll] = r_temp;
            }

            r_mean = (double)r_sum / (double)dset_conformed[ii]->num_targets;
            r_std = sqrt((double)r_sum_sq - ((double)r_sum)*r_mean);

            for( ll = 0; ll < dset_conformed[ii]->num_targets; ll++ )
            {
                data_r[ii*dset_conformed[ii]->num_targets+ll] = 1.0 / r_std *
                    ( data_r[ii*dset_conformed[ii]->num_targets+ll] - r_mean );
            }
        } /* for( ii = 0; ii < infiles; ii++) */

        for( ii = 0; ii < num_infiles; ii++)
        {
            for( jj = ii+1; jj < num_infiles; jj++)
            {
                out_r[kk*num_calc+ii*num_infiles+jj]=0.0;
                /* calculate the outer mults using seeds */
                for( ll = 0; ll < dset_conformed[jj]->num_targets; ll++ )
                {
                    out_r[kk*num_calc+ii*num_infiles+jj] += 
                        data_r[ii*dset_conformed[ii]->num_targets+ll]*
                        data_r[jj*dset_conformed[jj]->num_targets+ll];
                }
            }
        }

        if(kk % 1000 == 0 ) printf("."); fflush(stdout);
    }

    /* free mem used in calculations */
    /*cc_mask_std_avg(*/
#if 0
        if( do_write_intermediate == 1 )
        {
            /* If the user asks, write out the mask file for debugging */
            snprintf( str, STRLEN, "spat_sim_%s_%ld.nii.gz", prefix, ii );

            /*-- create output dataset --*/
            cset = EDIT_empty_copy( xset ) ;

            /*-- configure the output dataset */
            printf( "creating output bucket(%s)\n", str );
            EDIT_dset_items( cset ,
                ADN_prefix    , str ,
                ADN_nvals     , num_infiles - ii - 1, /*  subbricks */
                ADN_ntt       , 0              , /* no time axis */
                ADN_type      , HEAD_ANAT_TYPE ,
                ADN_func_type , ANAT_BUCK_TYPE ,
                ADN_datum_all , MRI_float     ,
                ADN_none ) ;

            /* add history information to the hearder */
            tross_Make_History( "3dCWAS" , argc,argv , cset ) ;

            ININFO_message("creating output dataset in memory") ;

            /* initialize the subbrik index */
            subbrik = 0;
        } 
            if( do_write_intermediate == 1 )
            {
                EDIT_BRICK_TO_NOSTAT(cset,subbrik); /* stat params  */
                EDIT_BRICK_FACTOR(cset,subbrik,1.0);/* scale factor */
                snprintf(str,STRLEN,"fc_spatial_sim");

                EDIT_BRICK_LABEL(cset,subbrik,str) ;
                EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array  */

                /* copy measure data into the subbrik */
                fodset = (float*)DSET_ARRAY(cset,subbrik);
                ININFO_message("copying sim matrix") ;

                /* write out the mask data, if we didn't create a mask or receive a mask
                 * in the input arguements, we put a 1 in every voxel */
		ivox = 0;
                for(ii=0;ii<roi_mask->num_tot_vox;ii++)
		{
		    if( roi_mask->mask[ii] == 1 )
		    {
			if( ivox < roi_mask->num_msk_vox )
			{
                            if( do_target_roi == 1 )
                            {
                                iroi = roi_mask->roi_mask[ivox] - 1;
                                if( iroi < roi_mask->num_rois )
                                {
                                    fodset[ii]=(float)out_r[iroi];    
                                }
                                else
                                {
                                    ININFO_message("exceeded number of rois %ld > %ld",
                                        iroi, roi_mask->num_rois);
                                }
                            }
                            else
                            {
                                fodset[ii]=(float)out_r[ivox];    
                            }
                            ivox = ivox + 1;
			}
                        else
                        {
                            ININFO_message("exceeded number of mask voxels %ld > %ld",
                                ivox, roi_mask->num_msk_vox);
                        }
		    }
		    else
		    {
		        fodset[ii]=(float)0.0;
		    }
		}

                ININFO_message("finished copying sim matrix") ;

                /* iterate to the next subbrik */
                subbrik = subbrik + 1;
            }

    /* write out the fule and clean up */
    if( do_write_intermediate == 1 )
    {
        /* write the dataset */
        DSET_write(cset);
        WROTE_DSET(cset);

        /* free up the output dataset memory */
        DSET_unload(cset);
        DSET_delete(cset);
        cset = NULL;
    }



    /* update the user so that they know what we are up to */

      if( DSET_NVOX(mset) != nvox )
         ERROR_EXIT_CC("Input and mask dataset differ in number of voxels!") ;

   /*-- open dataset, check for legality --*/

    if( nopt >= argc ) ERROR_EXIT_CC("Need a dataset on command line!?") ;
    xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);

   /* -- Load in the input dataset and make sure it is suitable -- */
   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;

   if( nvox * nvals * sizeof(float) > mem_bytes )
   {
     ERROR_EXIT_CC("Size of input dataset %s ( %s Bytes) exceeds the"
         " memory budget ( %s Bytes ). Either increase memory budget\n"
         "or use a smaller dataset.", argv[nopt],
         commaized_integer_string(nvox*nvals*sizeof(float)),
         commaized_integer_string(mem_bytes)) ;
   }

   if( nvals < 3 )
     ERROR_EXIT_CC("Input dataset %s does not have 3 or more sub-bricks!",
        argv[nopt]) ;

   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   INC_MEM_STATS((nvox * nvals * sizeof(double)), "input dset");
   PRINT_MEM_STATS("inset");

   if( !EQUIV_GRIDS(mask_dset,input_dset) )
     WARNING_message("Input dataset %s grid mismatch from mask.\n"
             "Try the following command for grid comparison:\n"
             " 3dinfo -header_line -prefix -same_all_grid %s %s\n"
             ,argv[narg], 
             DSET_HEADNAME(mask_dset), DSET_HEADNAME(input_dset)) ;


    /*-- create output dataset --*/
    cset = EDIT_empty_copy( xset ) ;

    /*-- configure the output dataset */
    if( abuc )
    {
        printf( "creating output bucket(%s)\n", prefix );
        EDIT_dset_items( cset ,
            ADN_prefix    , prefix         ,
            ADN_nvals     , nsubbriks      , /*  subbricks */
            ADN_ntt       , 0              , /* no time axis */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_BUCK_TYPE ,
            ADN_datum_all , MRI_float     ,
            ADN_none ) ;
    } 
    else 
    {
        EDIT_dset_items( cset ,
            ADN_prefix    , prefix         ,
            ADN_nvals     , nsubbriks      , /*  subbricks */
            ADN_ntt       , nsubbriks      ,  /* num times */
            ADN_ttdel     , 1.0            ,  /* fake TR */
            ADN_nsl       , 0              ,  /* no slice offsets */
            ADN_type      , HEAD_ANAT_TYPE ,
            ADN_func_type , ANAT_EPI_TYPE  ,
            ADN_datum_all , MRI_float      ,
            ADN_none ) ;
    }

    /* add history information to the hearder */
    tross_Make_History( "3dECM" , argc,argv , cset ) ;

    ININFO_message("creating output dataset in memory") ;

    for (subbrik = 0; subbrik < 2; subbrik++) {
        /* -- Configure the subbriks -- */

        EDIT_BRICK_TO_NOSTAT(cset,subbrik) ;                     /* stat params  */
        /* CC this sets the subbrik scaling factor, which we will probably want
           to do again after we calculate the voxel values */
        EDIT_BRICK_FACTOR(cset,subbrik,1.0) ;                 /* scale factor */
//
//        if( do_binary == 1 )
//        {
//            sprintf(str,"Binary ECM");
//        }
//        else
//        {
//            sprintf(str,"Weighted ECM");
//        }

        EDIT_BRICK_LABEL(cset,subbrik,str) ;
        EDIT_substitute_brick(cset,subbrik,MRI_float,NULL) ;   /* make array   */

        /* copy measure data into the subbrik */
        wodset = DSET_ARRAY(cset,subbrik);

        /* increment memory stats */
        INC_MEM_STATS( (DSET_NVOX(cset)*DSET_NVALS(cset)*sizeof(float)),
            "output dset");
        PRINT_MEM_STATS( "outset" );

        /* set all of the voxels in the output image to zero */
        bzero(wodset, DSET_NVOX(cset)*sizeof(float));

        /* output the eigenvector, scaling it by sqrt(2) */
        for (ii = 0; ii < xvectim->nvec; ii++ )
        {
           wodset[ mask_ndx_to_vol_ndx[ ii ] ] = (float)(SQRT_2 * eigen_vec[subbrik][ ii ]);
        }

        /* we have copied out v_prev, now we can kill it */
        if( eigen_vec[subbrik] != NULL )
        {
            free(eigen_vec[subbrik]);
            eigen_vec[subbrik] = NULL;

            /* update running memory statistics to reflect freeing the vectim */
            DEC_MEM_STATS(xvectim->nvec*sizeof(double), "eigen_vec");
        }
    }

    /*-- tell the user what we are about to do --*/
    INFO_message("Done..\n") ;

    /* update running memory statistics to reflect freeing the vectim */
    DEC_MEM_STATS(((xvectim->nvec*sizeof(int)) +
                       ((xvectim->nvec)*(xvectim->nvals))*sizeof(float) +
                       sizeof(MRI_vectim)), "vectim");

    /* toss some trash */
    VECTIM_destroy(xvectim) ;
    DSET_delete(xset) ;

    PRINT_MEM_STATS( "Vectim Unload" );

    /* finito */
    INFO_message("Writing output dataset to disk [%s bytes]",
                commaized_integer_string(cset->dblk->total_bytes)) ;

    /* write the dataset */
    DSET_write(cset);
    WROTE_DSET(cset);

    /* increment our memory stats, since we are relying on the header for this
       information, we update the stats before actually freeing the memory */
    DEC_MEM_STATS( (DSET_NVOX(cset)*DSET_NVALS(cset)*sizeof(float)), "output dset");

    /* free up the output dataset memory */
    DSET_unload(cset);
    DSET_delete(cset);

    PRINT_MEM_STATS( "Unload Output Dset" );
#endif

    roi_mask = cc_free_roimask( roi_mask );

    /* DSET_delete(mset) ;
     * mset = NULL ;
     */

    CHECK_AND_FREE_ALL_ALLOCATED_MEM;

    /* force a print */
    MEM_STAT = 1;
    PRINT_MEM_STATS( "Fin" );


    exit(0) ;
}
