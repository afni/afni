*****
3ddup
*****

.. _3ddup:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3ddup [options] dataset
     'Duplicates' a 3D dataset by making a warp-on-demand copy.
     Applications:
       - allows AFNI to resample a dataset to a new grid without
           destroying an existing data .BRIK
       - change a functional dataset to anatomical, or vice-versa
       - THIS PROGRAM IS BASICALLY OBSOLETE !!
    
    OPTIONS:
      -'type'           = Convert to the given 'type', which must be
                           chosen from the same list as in to3d
      -session dirname  = Write output into given directory (default=./)
      -prefix  pname    = Use 'pname' for the output directory prefix
                           (default=dup)
    N.B.: Even if the new dataset is anatomical, it will not contain
          any markers, duplicated from the original, or otherwise.
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
