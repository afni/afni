/*
afni/src/sparse_array.c
*/


// Look for OpenMP macro
#ifdef USE_OMP
#include <omp.h>
#endif

// Include libraries
#include "mrilib.h"
#include "sparse_array.h"
#include <sys/mman.h>
#include <sys/types.h>

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,",") ;
   nn++ ;
}


sparse_array_node* free_sparse_list( sparse_array_node* list )
{
    sparse_array_node* tptr = NULL;
    while( list != NULL )
    {
       tptr = list;
       list = list->next;
       free(tptr); 
    }
    return( NULL );
}

/* function for freeing a sparse array */
sparse_array_head_node* free_sparse_array( sparse_array_head_node* sparray )
{
    sparse_array_node* tptr = NULL;
    sparse_array_node* sptr = NULL;

    if( sparray != NULL )
    {
        sptr = sparray->nodes;

        while( sptr != NULL )
        {
            tptr = sptr;
            sptr = sptr->next;
            if( tptr != NULL) free(tptr);
        }

        /* now free the head node */
        free(sparray);
    }
    return(NULL);
}

/* we will use a histogram to sort the values, define that here*/
typedef struct _hist_node_head
{
    double bin_low;
    double bin_high;
    long nbin;
    sparse_array_node* nodes;
    sparse_array_node* tail;
} hist_node_head;

/* function to simplify free histogram */
hist_node_head* free_histogram(hist_node_head * histogram, int nhistbins)
{
    long kout = 0;

    /* only try to free the histogram if we have reason
       to beleive that it exists */
    if (histogram != NULL )
    {
        /* iterate through the histogram bins */
        for( kout = 0; kout < nhistbins; kout++ )
        {
            /* if there is a linked list for this
               bin, iterate over it and free the list */
            if( histogram[kout].nodes != NULL )
            {
                /* free the list of hist_nodes */
                free_sparse_list( histogram[kout].nodes );
                histogram[kout].nodes = NULL; 
            }
        } 

        /* all of the linked lists should be empty,
           now free the bin array */
        if (histogram != NULL) {
            free(histogram);
        }
    }

    return(NULL);
}

/* function for creating a sparse array from the thresholded
   correlation matrix of a vectim.

   This approach uses a histogram approach to implement a sparsity threshold if
   it is so desired.

   inputs:
       xvectim: the input time courses that will be correlated
       sparsity: the fraction of the top correlations that should be retained
       threshold: a threshold that should be applied to determine if a correlation 
           should be retained. For sparsity thresholding this value will be used
           as an inititial guess to speed calculation and a higher threshold may
           ultimately be calculated through the adaptive process.

    output:
        sparse_array_node: the list of remaining correlation values, or NULL if there
             was an error

    note:

        this function can use a _lot_ of memory if you the sparsity is too high, we tell
        the user how much memory we anticipate using, but this doesn't work for threshold only!*/
sparse_array_head_node* create_sparse_corr_array( MRI_vectim* xvectim, double sparsity, double thresh,
     double (*corfun)(long,float*,float*), long mem_allowance  )
{

    /* random counters etc... */
    long kout = 0;

    /* variables for histogram */
    hist_node_head* histogram=NULL;
    sparse_array_head_node* sparse_array=NULL;
    sparse_array_node* recycled_nodes=NULL;
    long bottom_node_idx = 0;
    long totNumCor = 0;
    long totPosCor = 0;
    long ngoal = 0;
    long nretain = 0;
    float binwidth = 0.0;
    long nhistbins = 100;
    long mem_budget = 0;

    /* retain the original threshold*/
    double othresh = thresh;

    /* set the memory budget from the allowance */
    mem_budget = mem_allowance;

    INFO_message( "Starting create_sparse_corr_array with a memory allowance of %ld", mem_budget);
 
    /* calculate the total number of possible correlations */
    totPosCor = .5 * ( xvectim->nvec -1 ) * ( xvectim->nvec );

    /* create a head node for the sparse array */
    sparse_array = (sparse_array_head_node*)calloc(1,sizeof(sparse_array_head_node));

    if( sparse_array == NULL )
    {
        ERROR_message( "Could not allocate header for sparse array\n" );
        return(NULL);
    }

    /* decrement the memory budget to account for the sparse array header */
    mem_budget = mem_budget - sizeof(sparse_array_head_node);

    /* check if we can do what is asked of us with the budget provided */
    if( sparsity < 100.0 )
    {
        /* figure the cost of the histogram into the memory budget */
        mem_budget = mem_budget - nhistbins*sizeof(hist_node_head);

        /* and the number of desired correlations */
        ngoal = nretain = (long)ceil(((double)totPosCor)*((double)sparsity) / 100.0);

        /* check to see if we want to use more memory than would be used by the 
           full correlation matrix, if so, the we should probably just use full
           correlation - or min memory func */
        if((ngoal * sizeof( sparse_array_node )) > mem_budget)
        {
           WARNING_message( "The sparse array with %3.2lf%% of the %ld total"
                            " would exceed the memory budget (%3.2lf MB) refusing to proceed\n",
                            sparsity, totPosCor,((double)mem_budget)/(1024.0*1024.0));
           return( NULL );
        }
        else
        {
           INFO_message( "The sparse array with %ld values will take %3.2lf"
                         " MB of memory (budget = %3.2lf MB)\n", ngoal,
                         (double)(ngoal * sizeof(sparse_array_node))
                         /(1024.0*1024.0), ((double)mem_budget)/(1024.0*1024.0));
        }
    }
    else
    {
        WARNING_message( "Cannot pre-calculate the memory required for a sparse"
                         " matrix when only a correlation threshold is used. "
                         "Instead the mem is tracked and if we exceed what "
                         "would be used by the non-sparse array, the operation"
                         " will be aborted.");
    }

    INFO_message( "Extracting sparse correlation array with threshold = %f and"
                  " sparsity = %3.2f%% (%d)\n", thresh, sparsity, nretain);

    /* if we are using a sparsity threshold, setup the histogram to sort the values */
    if ( sparsity < 100.0 )
    {

        /* make sure that there is a bin for correlation values that == 1.0 */
        binwidth = (1.005-thresh)/nhistbins;

        /* allocate memory for the histogram bins */
        if(( histogram = (hist_node_head*)malloc(nhistbins*sizeof(hist_node_head))) == NULL )
        {
            /* if the allocation fails, free all memory and exit */
            ERROR_message( "Could not allocate %d byte array for histogram\n",
                nhistbins*sizeof(hist_node_head)); 
            return( NULL );
        }

        /* initialize history bins */
        for( kout = 0; kout < nhistbins; kout++ )
        {
            histogram[ kout ].bin_low = thresh+kout*binwidth;
            histogram[ kout ].bin_high = histogram[ kout ].bin_low+binwidth;
            histogram[ kout ].nbin = 0;
            histogram[ kout ].nodes = NULL; 
            histogram[ kout ].tail = NULL; 
            /*
                INFO_message("Hist bin %d [%3.3f, %3.3f) [%d, %p]\n",
                    kout, histogram[ kout ].bin_low, histogram[ kout ].bin_high,
                    histogram[ kout ].nbin, histogram[ kout ].nodes );
            */
        }

    }

    /*---------- loop over mask voxels, correlate ----------*/
    AFNI_OMP_START ;
#pragma omp parallel if( xvectim->nvec > 999 )
    {
        int lii,ljj,lin,lout,ithr,nthr,vstep,vii ;
        float *xsar , *ysar ;
        sparse_array_node* new_node = NULL ;
        int new_node_idx = 0;
        double car = 0.0 ; 

        /*-- get information about who we are --*/
#ifdef USE_OMP
        ithr = omp_get_thread_num() ;
        nthr = omp_get_num_threads() ;
        if( ithr == 0 ) INFO_message("%d OpenMP threads started",nthr) ;
#else
        ithr = 0 ; nthr = 1 ;
#endif

        /*-- For the progress tracker, we want to print out 50 numbers,
             figure out a number of loop iterations that will make this easy */
        vstep = (int)( xvectim->nvec / (nthr*50.0f) + 0.901f ) ; vii = 0 ;
        if(ithr == 0 ) fprintf(stderr,"Looping:") ;

#pragma omp for schedule(static, 1)
        for( lout=0 ; lout < xvectim->nvec ; lout++ )
        {  /*----- outer voxel loop -----*/

            if( ithr == 0 && vstep > 2 ) /* allow small dsets 16 Jun 2011 [rickr] */
            {
                vii++;
                if( vii%vstep == vstep/2 )
                {
                    vstep_print();
                }
            }

            /* if the amount of memory exceeds budget, dont do anything more */
            if ( mem_budget >= 0 )
            {
                /* get ref time series from this voxel */
                xsar = VECTIM_PTR(xvectim,lout);

                /* try to make calculation more efficient by only calculating the unique 
                   correlations */
                for( lin=(lout+1) ; lin < xvectim->nvec ; lin++ )
                {  /*----- inner loop over voxels -----*/

                    if ( mem_budget >= 0 )
                    {
                        /* extract the voxel time series */
                        ysar = VECTIM_PTR(xvectim,lin);

                        /* now correlate the time series */
                        car = (double)(corfun(xvectim->nvals,xsar,ysar));

                        if ( car < thresh )
                        {
                            continue;
                        }

#pragma omp critical(dataupdate)
                        {
                            /* the threshold might have changed while we were waiting,
                               so check it again */
                            if (car >= thresh )
                            {
                                /* create a node to add to the histogram, try to use a 
                                   recycled node to save time and memory */
                                if ( recycled_nodes == NULL )
                                {
                                    mem_budget = mem_budget - sizeof(sparse_array_node);
                                    if( mem_budget >= 0 )
                                    {
                                        new_node = (sparse_array_node*)calloc(1,sizeof(sparse_array_node));
                                    }
                                    else
                                    {
                                        new_node = NULL; 
                                    }
                                }
                                else
                                {
                                    new_node = recycled_nodes;
                                    new_node->next = NULL;
                                    recycled_nodes = recycled_nodes->next;
                                }
                                if( new_node == NULL )
                                {
                                    /* allocate memory for this node, rather than fiddling with 
                                       error handling here, lets just move on */
                                    WARNING_message("Could not allocate a new node!");
                                }
                                else
                                {
                                    new_node->weight = car;
                                    new_node->row = lout;
                                    new_node->column = lin;

                                    totNumCor += 1;
               
                                    if ( sparsity >= 100.0 )
                                    {
                                        new_node->next = sparse_array->nodes;
                                        sparse_array->nodes = new_node;
                                        sparse_array->num_nodes = sparse_array->num_nodes + 1;
                                        new_node = NULL; 
                                    }
                                    else
                                    {
                                        /* determine the index in the histogram to add the node */
                                        new_node_idx = (int)floor((double)(car-othresh)/(double)binwidth);
                                        if ((new_node_idx > nhistbins) || (new_node_idx < bottom_node_idx))
                                        {
                                            /* this error should indicate a programming error and should not happen */
                                            WARNING_message("Node index %d (%3.4lf >= %3.4lf) is out of range [%d,%d)"
                                                " {[%3.4lf, %3.4lf)}!",new_node_idx, car, thresh, bottom_node_idx,
                                                nhistbins, histogram[bottom_node_idx].bin_low,
                                                histogram[bottom_node_idx].bin_high );
                                        }
                                        else
                                        {
                                            /* populate histogram node */
                                            new_node->row = lout; 
                                            new_node->column = lin;
                                            new_node->weight = car;
                                            new_node->next = NULL;
        
                                            /* populate histogram */
                                            new_node->next = histogram[new_node_idx].nodes;
                                            histogram[new_node_idx].nodes = new_node;
                                            if (histogram[new_node_idx].tail == NULL)
                                            {
                                                histogram[new_node_idx].tail = new_node;
                                            }
                                            histogram[new_node_idx].nbin++; 
                
                                            /* see if there are enough correlations in the histogram
                                               for the sparsity */
                                            while ((totNumCor - histogram[bottom_node_idx].nbin) > nretain)
                                            { 
                                                /* push the histogram nodes onto the list of recycled nodes, it could be
                                                   that this hist bin is empty, in which case we have nothing to add */
                                                if( histogram[bottom_node_idx].tail != NULL )
                                                {
                                                    histogram[bottom_node_idx].tail->next = recycled_nodes;
                                                    recycled_nodes = histogram[bottom_node_idx].nodes;
                                                }
                                                else
                                                {
                                                    if( histogram[bottom_node_idx].nbin != 0 )
                                                    {
                                                         WARNING_message("Trying to remove histogram bin that contains"
                                                                         " %d values, but whose tail pointer is NULL\n",
                                                                         histogram[bottom_node_idx].nbin);
                                                    }
                                                }
        
                                                /* bookkeeping */ 
                                                histogram[bottom_node_idx].nodes = NULL;
                                                histogram[bottom_node_idx].tail = NULL;
                                                totNumCor -= histogram[bottom_node_idx].nbin;
                                                histogram[bottom_node_idx].nbin = 0;
             
                                                /* get the new threshold */
                                                thresh = (double)histogram[++bottom_node_idx].bin_low;
                                                /*INFO_message("Increasing threshold to %3.2f (%d)\n",
                                                    thresh,bottom_node_idx); */
                                            } /* while */
                                        } /* else, new_node_idx in range */
                                    } /* else, sparsity >= 100.0 */
                                } /* else, new_node != NULL */
                            } /* if (car >= thresh ) */
                        } /* this is the end of the critical section */
                    } /* if ( mem_budget >= 0 ) */
                } /* end of inner loop over voxels */
            } /* if ( mem_budget >= 0 ) */
        } /* end of outer loop over ref voxels */

        if( ithr == 0 ) fprintf(stderr,".\n") ;

    } /* end OpenMP */
    AFNI_OMP_END ;


    /* check to see if we exceeded memory or didn't get any
       correlations > threshold */
    if (( mem_budget < 0 ) || ( totNumCor == 0 ))
    {
        if ( mem_budget < 0 )
        {
            ERROR_message( "Memory budget (%lf MB) exceeded, consider using a"
                "higher correlation or lower sparsity threshold",
                ((double)mem_allowance/(1024.0*1024.0)));
        }
        else
        {
            ERROR_message( "No correlations exceeded threshold, consider using"
                           " a lower correlation threshold");
        }
        sparse_array = free_sparse_array( sparse_array ); 
    }
    else
    {
        /* if using sparsity threshold, construct sparse array from
           the histogram */
        if ( sparsity < 100.0 )
        {

            /* pull the requested number of nodes off of the histogram */
            for ( kout = (nhistbins-1); kout >= bottom_node_idx; kout-- )
            { 
                if((histogram[ kout ].nodes != NULL ) &&
                   (histogram[ kout ].nbin > 0))
                {
                    if( histogram[ kout ].tail == NULL )
                    {
                        ERROR_message("Head is not null, but tail is?? (%ld)\n",
                            kout);
                    }
                    /* push the list onto sparse array */
                    histogram[ kout ].tail->next = sparse_array->nodes;
                    sparse_array->nodes = histogram[ kout ].nodes;

                    /* increment the number of nodes */
                    sparse_array->num_nodes = sparse_array->num_nodes +
                        histogram[ kout ].nbin;
      
                    /* remove the references from the histogram,
                       this is super important considering we don't want
                       to accidently free any nodes that are on the sparse_array
                       when we free the histogram later */
                    histogram[ kout ].nodes = NULL;
                    histogram[ kout ].tail = NULL;
                    histogram[ kout ].nbin = 0;
                } 
                /* dont take more than we want */
                if ( sparse_array->num_nodes > nretain ) break;
            }

            INFO_message( "Sparsity requested %ld and received %ld correlations"
                          " (%3.2lf%% sparsity) final threshold = %3.4lf.\n",
                nretain, sparse_array->num_nodes,
                100.0*((double)sparse_array->num_nodes)/((double)totPosCor),
                thresh);

            if( sparse_array->num_nodes < nretain )
            {
                INFO_message( "Consider lowering the initial correlation"
                    "threshold (%3.2lf) to retain more correlations.\n",
                    othresh);
            }
        }
        else
        {
            INFO_message( "Correlation threshold (%3.2lf) resulted in %ld"
                " correlations (%3.2lf%% sparsity).\n", thresh, 
                sparse_array->num_nodes,
                100.0*((double)sparse_array->num_nodes)/((double)totPosCor));
        }
    }

    /* free residual mem */
    histogram = free_histogram( histogram, nhistbins );
    recycled_nodes = free_sparse_list( recycled_nodes );    

    return( sparse_array );
}

