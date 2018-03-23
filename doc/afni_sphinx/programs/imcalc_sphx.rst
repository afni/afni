.. _ahelp_imcalc:

******
imcalc
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Do arithmetic on 2D images, pixel-by-pixel.
    Usage: imcalc options
    where the options are:
      -datum type = Coerce the output data to be stored as the given type,
                      which may be byte, short, or float.
                      [default = datum of first input image on command line]
      -a dname    = Read image 'dname' and call the voxel values 'a'
                      in the expression.  'a' may be any letter from 'a' to 'z'.
                   ** If some letter name is used in the expression, but not
                      present in one of the image options here, then that
                      variable is set to 0.
      -expr "expression"
                    Apply the expression within quotes to the input images,
                      one voxel at a time, to produce the output image.
                      ("sqrt(a*b)" to compute the geometric mean, for example)
      -output name = Use 'name' for the output image filename.
                      [default='imcalc.out']
    
    See the output of '3dcalc -help' for details on what kinds of expressions
    are possible.  Note that complex-valued images cannot be processed (byte,
