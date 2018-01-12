.. contents:: 
    :depth: 4 

************
3dNwarpFuncs
************

.. code-block:: none

    Usage: 3dNwarpFuncs [options]
    
    This program reads in a nonlinear 3D warp (from 3dQwarp, etc.) and
    computes some functions of the displacements.  See the OPTIONS below
    for information on what can be computed.
    
    OPTIONS:
    --------
     -nwarp  www  = 'www' is the name of the 3D warp dataset
                    (this is a mandatory option!)
                   ++ This can be computed on the fly, as in 3dNwarpApply.
    
     -prefix ppp  = 'ppp' is the name of the new output dataset
    
     -bulk        = Compute the (fractional) bulk volume change.
                   ++ e.g., Jacobian determinant minus 1.
     -shear       = Compute the shear energy.
     -vorticity   = Compute the vorticity enerty.
    
    If none of '-bulk', '-shear', or '-vorticity' are given, then '-bulk'
    will be assumed.
    
    NOTES:
    ------
    Denote the displacement vector field (warp) by
       [ p(x,y,z) , q(x,y,z) , r(x,y,z) ]
    Define the Jacobian matrix by
    
          [ 1+dp/dx   dp/dy    dp/dz  ]   [ Jxx Jxy Jxz ]
      J = [  dq/dx   1+dq/dy   dq/dz  ] = [ Jyx Jyy Jyz ]
          [  dr/dx    dr/dy   1+dr/dz ]   [ Jzx Jzy Jzz ]
    
    * The '-bulk' output is the determinant of this matrix (det[J]), minus 1.
    * It measures the amount of volume distortion.
    
    * The '-shear' output is the sum of squares of the J matrix elements --
      which equals the sum of squares of its eigenvalues -- divided by
      det[J]^(2/3), then minus 3.
    * It measures the amount of shearing distortion (normalized by the amount
      of volume distortion).
    
    * The '-vorticity' output is the sum of squares of the skew part of
      the J matrix = [ Jxy-Jyx , Jxz-Jzx , Jyz-Jzy ], divided by det[J]^(2/3).
    * It measures the amount of twisting distortion (also normalized).
    
    * All 3 of these functions are dimensionless.
    
    * The penalty used in 3dQwarp is a combination of the bulk, shear,
      and vorticity functions.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
