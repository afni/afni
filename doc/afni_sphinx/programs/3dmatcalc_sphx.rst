*********
3dmatcalc
*********

.. _3dmatcalc:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dmatcalc [options]
    Apply a matrix to a dataset, voxel-by-voxel, to produce a new
    dataset.
    
    * If the input dataset has 'N' sub-bricks, and the input matrix
       is 'MxN', then the output dataset will have 'M' sub-bricks; the
       results in each voxel will be the result of extracting the N
       values from the input at that voxel, multiplying the resulting
       N-vector by the matrix, and output the resulting M-vector.
    
    * If the input matrix has 'N+1' columns, then it will be applied
       to an (N+1)-vector whose first N elements are from the dataset
       and the last value is 1.  This convention allows the addition
       of a constant vector (the last row of the matrix) to each voxel.
    * The output dataset is always stored in float format.
    * Useful applications are left to your imagination.  The example
       below is pretty fracking hopeless.  Something more useful might
       be to project a 3D+time dataset onto some subspace, then run
       3dpc on the results.
    
    
    OPTIONS:
    -------
     -input ddd  = read in dataset 'ddd'  [required option]
     -matrix eee = specify matrix, which can be done as a .1D file
                    or as an expression in the syntax of 1dmatcalc
                    [required option]
     -prefix ppp = write to dataset with prefix 'ppp'
     -mask mmm   = only apply to voxels in the mask; other voxels
                    will be set to all zeroes
    
    EXAMPLE:
    -------
    Assume dataset 'v+orig' has 50 sub-bricks:
     3dmatcalc -input v+orig -matrix '&read(1D:50@1,\,50@0.02) &transp' -prefix w
    The -matrix option computes a 2x50 matrix, whose first row is all 1's
    and whose second row is all 0.02's.  Thus, the output dataset w+orig has
    2 sub-bricks, the first of which is the voxel-wise sum of all 50 inputs,
    and the second is the voxel-wise average (since 0.02=1/50).
    
    -- Zhark, Emperor -- April 2006
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
