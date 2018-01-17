*******
3dedge3
*******

.. _3dedge3:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dedge3 [options] dset dset ...
    Does 3D Edge detection using the library 3DEdge by;
    by Gregoire Malandain (gregoire.malandain@sophia.inria.fr)
    
    Options :
      -input iii  = Input dataset
      -verbose    = Print out some information along the way.
      -prefix ppp = Sets the prefix of the output dataset.
      -datum ddd  = Sets the datum of the output dataset.
      -fscale     = Force scaling of the output to the maximum integer range.
      -gscale     = Same as '-fscale', but also forces each output sub-brick to
                      to get the same scaling factor.
      -nscale     = Don't do any scaling on output to byte or short datasets.
      -scale_floats VAL = Multiply input by VAL, but only if the input datum is
                          float. This is needed when the input dataset
                          has a small range, like 0 to 2.0 for instance.
                          With such a range, very few edges are detected due to
                          what I suspect to be truncation problems.
                          Multiplying such a dataset by 10000 fixes the problem
                          and the scaling is undone at the output.
    
    
    References for the algorithms:
     -  Optimal edge detection using recursive filtering
        R. Deriche, International Journal of Computer Vision,
        pp 167-187, 1987.
     -  Recursive filtering and edge tracking: two primary tools
        for 3-D edge detection, O. Monga, R. Deriche, G. Malandain
        and J.-P. Cocquerez, Image and Vision Computing 4:9, 
        pp 203-214, August 1991.
    
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
