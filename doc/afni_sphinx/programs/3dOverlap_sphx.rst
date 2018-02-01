*********
3dOverlap
*********

.. _3dOverlap:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dOverlap [options] dset1 dset2 ...
    Output = count of number of voxels that are nonzero in ALL
             of the input dataset sub-bricks
    The result is simply a number printed to stdout.  (If a single
    brick was input, this is just the count of the number of nonzero
    voxels in that brick.)
    Options:
      -save ppp = Save the count of overlaps at each voxel into a
                  dataset with prefix 'ppp' (properly thresholded,
                  this could be used as a mask dataset).
    Example:
      3dOverlap -save abcnum a+orig b+orig c+orig
      3dmaskave -mask 'abcnum+orig<3..3>' a+orig
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
