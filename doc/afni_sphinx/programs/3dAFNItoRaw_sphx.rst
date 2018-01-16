***********
3dAFNItoRaw
***********

.. _3dAFNItoRaw:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dAFNItoRaw [options] dataset
    Convert an AFNI brik file with multiple sub-briks to a raw file with
      each sub-brik voxel concatenated voxel-wise.
    For example, a dataset with 3 sub-briks X,Y,Z with elements x1,x2,x3,...,xn,
      y1,y2,y3,...,yn and z1,z2,z3,...,zn will be converted to a raw dataset with
      elements x1,y1,z1, x2,y2,z2, x3,y3,z3, ..., xn,yn,zn 
    The dataset is kept in the original data format (float/short/int)
    Options:
      -output / -prefix = name of the output file (not an AFNI dataset prefix)
        the default output name will be rawxyz.dat
    
      -datum float = force floating point output. Floating point forced if any
        sub-brik scale factors not equal to 1.
    
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
