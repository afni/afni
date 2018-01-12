.. contents:: 
    :depth: 4 

************
3dThreetoRGB
************

.. code-block:: none

    Usage #1: 3dThreetoRGB [options] dataset
    Usage #2: 3dThreetoRGB [options] dataset1 dataset2 dataset3
    
    Converts 3 sub-bricks of input to an RGB-valued dataset.
    * If you have 1 input dataset, then sub-bricks [0..2] are
       used to form the RGB components of the output.
    * If you have 3 input datasets, then the [0] sub-brick of
       each is used to form the RGB components, respectively.
    * RGB datasets have 3 bytes per voxel, with values ranging
       from 0..255.
    
    Options:
      -prefix ppp = Write output into dataset with prefix 'ppp'.
                     [default='rgb']
      -scale fac  = Multiply input values by 'fac' before using
                     as RGB [default=1].  If you have floating
                     point inputs in range 0..1, then using
                     '-scale 255' would make a lot of sense.
      -mask mset  = Only output nonzero values where the mask
                     dataset 'mset' is nonzero.
      -fim        = Write result as a 'fim' type dataset.
                     [this is the default]
      -anat       = Write result as a anatomical type dataset.
    Notes:
    * Input datasets must be byte-, short-, or float-valued.
    * You might calculate the component datasets using 3dcalc.
    * You can also create RGB-valued datasets in to3d, using
       2D raw PPM image files as input, or the 3Dr: format.
    * RGB fim overlays are transparent in AFNI in voxels where all
       3 bytes are zero - that is, it won't overlay solid black.
    * At present, there is limited support for RGB datasets.
       About the only thing you can do is display them in 2D
       slice windows in AFNI.
    
    -- RWCox - April 2002
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
