*********
3dDTtoDWI
*********

.. _ahelp_3dDTtoDWI:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dDTtoDWI [options] gradient-file I0-dataset DT-dataset
    
    Computes multiple gradient images from 6 principle direction tensors and
        corresponding gradient vector coordinates applied to the I0-dataset.
     The program takes three parameters as input :  
        a 1D file of the gradient vectors with lines of ASCII floats Gxi,Gyi,Gzi.
        Only the non-zero gradient vectors are included in this file (no G0 line).
     The I0 dataset is a volume without any gradient applied.
     The DT dataset is the 6-sub-brick dataset containing the diffusion tensor data,
        Dxx, Dxy, Dyy, Dxz, Dyz, Dzz (lower triangular row-wise order)
    
     Options:
       -prefix pname    = Use 'pname' for the output dataset prefix name.
                          [default='DWI']
       -automask        = mask dataset so that the gradient images
                          are computed only for high-intensity (presumably
                          brain) voxels.  The intensity level is determined
                          the same way that 3dClipLevel works.
    
       -datum type      = output dataset type [float/short/byte] 
                          (default is float).
       -help            = show this help screen.
       -scale_out_1000  = matches with 3dDWItoDT's '-scale_out_1000'
                          functionality.  If the option was used
                          there, then use it here, too.
    
     Example:
    
        3dDTtoDWI -prefix DWI -automask tensor25.1D 'DT+orig[26]' DT+orig.
    
    
     The output is a n sub-brick bucket dataset containing computed DWI images.
        where n is the number of vectors in the gradient file + 1
    
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
