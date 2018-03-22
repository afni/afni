*********
3dmatmult
*********

.. _ahelp_3dmatmult:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
    Multiply AFNI datasets slice-by-slice as matrices.
    
    If dataset A has Ra rows and Ca columns (per slice), and dataset B has
    Rb rows and Cb columns (per slice), multiply each slice pair as matrices
    to obtain a dataset with Ra rows and Cb columns.  Here Ca must equal Rb
    and the number of slices must be equal.
    
    In practice the first dataset will probably be a transformation matrix
    (or a sequence of them) while the second dataset might just be an image.
    For this reason, the output dataset will be based on inputB.
    
    ----------------------------------------
    examples:
    
        3dmatmult -inputA matrix+orig -inputB image+orig -prefix transformed
    
        3dmatmult -inputA matrix+orig -inputB image+orig  \
                  -prefix transformed -datum float -verb 2
    
    ----------------------------------------
    informational command arguments (execute option and quit):
    
        -help                   : show this help
        -hist                   : show program history
        -ver                    : show program version
    
    ----------------------------------------
    required command arguments:
    
        -inputA DSET_A          : specify first (matrix) dataset
    
            The slices of this dataset might be transformation matrices.
    
        -inputB DSET_B          : specify second (matrix) dataset
    
            This dataset might be any image.
    
        -prefix PREFIX          : specify output dataset prefix
    
            This will be the name of the product (output) dataset.
    
    ----------------------------------------
    optional command arguments:
    
        -datum TYPE             : specify output data type
    
            Valid TYPEs are 'byte', 'short' and 'float'.  The default is
            that of the inputB dataset.
    
        -verb LEVEL             : specify verbosity level
    
            The default level is 1, while 0 is considered 'quiet'.
    
    ----------------------------------------
    * If you need to re-orient a 3D dataset so that the rows, columns
      and slices are correct for 3dmatmult, you can use the one of the
      programs 3daxialize or 3dresample for this purpose.
    
    * To multiply a constant matrix into a vector at each voxel, the
      program 3dmatcalc is the proper tool.
    
    ----------------------------------------------------------------------
    R. Reynolds    (requested by W. Gaggl)
    
    3dmatmult version 0.0, 29 September 2008
    compiled: Mar  7 2018
