#ifndef _sparse_array_h

#define _sparse_array_h

#include "mrilib.h"

/* define sparse array node data structure */
typedef struct _sparse_array_node sparse_array_node;

struct _sparse_array_node
{
   long row;
   long column;
   double weight;
   sparse_array_node* next; 
};

typedef struct _sparse_array_head_node
{
   long num_nodes;
   sparse_array_node* nodes; 
} sparse_array_head_node;


/* function for freeing a sparse array */
sparse_array_head_node* free_sparse_array( sparse_array_head_node* sparray );

/* function for creating a sparse array from the thresholded
   correlation matrix of a vectim.

   This approach uses a histogram approach to implement a sparsity threshold if
   it is so desired.

   inputs:
       xvectim: the input time courses that will be correlated
       sparsity: the fraction of the top correlations that should be retained
       threshold: a threshold that should be applied to determine if a
           correlation should be retained. For sparsity thresholding this value
           will be used as an inititial guess to speed calculation and a higher
           threshold may ultimately be calculated through the adaptive process.

    output:
        sparse_array_node: the list of remaining correlation values, or NULL
        if there was an error

    note:
        this function can use a _lot_ of memory if you the sparsity is too
        high, we implement a crude check to try and avoid killing the system,
        but it may not always work!*/
sparse_array_head_node* create_sparse_corr_array( MRI_vectim* xvectim,
    double sparsity, double threshold, double (*corfun)(long,float*,float*),
    long memory_allowance  );
#endif
