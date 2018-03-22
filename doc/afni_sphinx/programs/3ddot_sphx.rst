*****
3ddot
*****

.. _ahelp_3ddot:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3ddot [options] dset1 [dset2 dset3 ...]
    Output = correlation coefficient between sub-brick pairs
             All datasets on the command line will get catenated
             at loading time and should all be on the same grid.
             - you can use sub-brick selectors on the dsets
             - the result is a number printed to stdout
    Options:
      -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be averaged from 'dataset'.  Note
                     that the mask dataset and the input dataset
                     must have the same number of voxels.
      -mrange a b  Means to further restrict the voxels from
                     'mset' so that only those mask values
                     between 'a' and 'b' (inclusive) will
                     be used.  If this option is not given,
                     all nonzero values from 'mset' are used.
                     Note that if a voxel is zero in 'mset', then
                     it won't be included, even if a < 0 < b.
      -demean      Means to remove the mean from each volume
                     prior to computing the correlation.
      -docor       Return the correlation coefficient (default).
      -dodot       Return the dot product (unscaled).
      -docoef      Return the least square fit coefficients
                     {a,b} so that dset2 is approximately a + b*dset1
      -dosums      Return the 6 numbers xbar=<x> ybar=<y>
                     <(x-xbar)^2> <(y-ybar)^2> <(x-xbar)(y-ybar)> 
                     and the correlation coefficient.
      -doeta2      Return eta-squared (Cohen, NeuroImage 2008).
      -dodice      Return the Dice coefficient (the Sorensen-Dice index).
      -show_labels Print sub-brick labels to help identify what 
                   is being correlated. This option is useful when
                   you have more than 2 sub-bricks at input.
      -upper       Compute upper triangular matrix
      -full        Compute the whole matrix. A waste of time, but handy
                   for parsing.
      -1D          Comment headings in order to read in 1D format.
                   This is only useful with -full.
      -NIML        Write output in NIML 1D format. Nicer for plotting.
                   -full and -show_labels are automatically turned on with -NIML.
                   For example: 
                        3ddot -NIML anat.001.sc7z.sigset+orig"[0,1,2,3,4]" \
                                                                    > corrmat.1D
                        1dRplot corrmat.1D 
               or
                        1dRplot -save somecorr.jpg -i corrmat.1D
    
      Note: This program is not efficient when more than two subbricks are input.
    
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
