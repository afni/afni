*********
3dZeropad
*********

.. _ahelp_3dZeropad:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dZeropad [options] dataset
    Adds planes of zeros to a dataset (i.e., pads it out).
    
    Options:
      -I n = adds 'n' planes of zero at the Inferior edge
      -S n = adds 'n' planes of zero at the Superior edge
      -A n = adds 'n' planes of zero at the Anterior edge
      -P n = adds 'n' planes of zero at the Posterior edge
      -L n = adds 'n' planes of zero at the Left edge
      -R n = adds 'n' planes of zero at the Right edge
      -z n = adds 'n' planes of zeros on EACH of the
              dataset z-axis (slice-direction) faces
    
     -RL a = These options specify that planes should be added/cut
     -AP b = symmetrically to make the resulting volume have
     -IS c = 'a', 'b', and 'c' slices in the respective directions.
    
     -mm   = pad counts 'n' are in mm instead of slices:
             * each 'n' is an integer
             * at least 'n' mm of slices will be added/removed:
                n =  3 and slice thickness = 2.5 mm ==> 2 slices added
                n = -6 and slice thickness = 2.5 mm ==> 3 slices removed
    
     -master mset = match the volume described in dataset 'mset':
                    * mset must have the same orientation and grid
                       spacing as dataset to be padded
                    * the goal of -master is to make the output dataset
                       from 3dZeropad match the spatial 'extents' of
                       mset (cf. 3dinfo output) as much as possible,
                       by adding/subtracting slices as needed.
                    * you can't use -I,-S,..., or -mm with -master
    
     -prefix ppp = write result into dataset with prefix 'ppp'
                     [default = 'zeropad']
    
    Nota Bene:
     * You can use negative values of n to cut planes off the edges
         of a dataset.  At least one plane must be added/removed
         or the program won't do anything.
     * Anat parent and Talairach markers are NOT preserved in the
         new dataset.
     * If the old dataset has z-slice-dependent time offsets, and
         if new (zero filled) z-planes are added, the time offsets
         of the new slices will be set to zero.
     * You can use program '3dinfo' to find out how many planes
         a dataset has in each direction.
     * Program works for byte-, short-, float-, and complex-valued
         datasets.
     * You can use a sub-brick selector on the input dataset.
    
     Author: RWCox - July 2000
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
