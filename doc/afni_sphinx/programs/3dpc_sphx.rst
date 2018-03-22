****
3dpc
****

.. _ahelp_3dpc:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Principal Component Analysis of 3D Datasets
    Usage: 3dpc [options] dataset dataset ...
    
    Each input dataset may have a sub-brick selector list.
    Otherwise, all sub-bricks from a dataset will be used.
    
    OPTIONS:
      -dmean        = remove the mean from each input brick (across space)
      -vmean        = remove the mean from each input voxel (across bricks)
                        [N.B.: -dmean and -vmean are mutually exclusive]
                        [default: don't remove either mean]
      -vnorm        = L2 normalize each input voxel time series
                        [occurs after the de-mean operations above,]
                        [and before the brick normalization below. ]
      -normalize    = L2 normalize each input brick (after mean subtraction)
                        [default: don't normalize]
      -nscale       = Scale the covariance matrix by the number of samples
                      This is not done by default for backward compatibility.
                      You probably want this option on. 
      -pcsave sss   = 'sss' is the number of components to save in the output;
                        it can't be more than the number of input bricks
                        [default = none of them]
                      * To get all components, set 'sss' to a very large
                        number (more than the time series length), like 99999
                        You can also use the key word ALL, as in -pcsave ALL
                        to save all the components.
      -reduce r pp  = Compute a 'dimensionally reduced' dataset with the top
                        'r' eigenvalues and write to disk in dataset 'pp'
                        [default = don't compute this at all]
                      * If '-vmean' is given, then each voxel's mean will
                        be added back into the reduced time series.  If you
                        don't want this behaviour, you could remove the mean
                        with 3dDetrend before running 3dpc.
                      * On the other hand, the effects of '-vnorm' and '-dmean'
                        and '-normalize' are not reversed in this output
                        (at least at present -- send some cookies and we'll talk).
      -prefix pname = Name for output dataset (will be a bucket type);
                      * Also, the eigen-timeseries will be in 'pname'_vec.1D
                        (all of them) and in 'pnameNN.1D' for eigenvalue
                        #NN individually (NN=00 .. 'sss'-1, corresponding
                        to the brick index in the output dataset)
                      * The eigenvalues will be printed to file 'pname'_eig.1D
                        All eigenvalues are printed, regardless of '-pcsave'.
                        [default value of pname = 'pc']
      -1ddum ddd    = Add 'ddd' dummy lines to the top of each *.1D file.
                        These lines will have the value 999999, and can
                        be used to align the files appropriately.
                        [default value of ddd = 0]
      -verbose      = Print progress reports during the computations
      -quiet        = Don't print progress reports [the default]
      -eigonly      = Only compute eigenvalues, then
                        write them to 'pname'_eig.1D, and stop.
      -float        = Save eigen-bricks as floats
                        [default = shorts, scaled so that |max|=10000]
      -mask mset    = Use the 0 sub-brick of dataset 'mset' as a mask
                        to indicate which voxels to analyze (a sub-brick
                        selector is allowed) [default = use all voxels]
    
    Example using 1D data a input, with each column being the equivalent
    of a sub-brick:
     3dpc -prefix mmm -dmean -nscale -pcsave ALL datafile.1D
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
