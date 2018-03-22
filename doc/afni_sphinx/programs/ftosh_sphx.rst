*****
ftosh
*****

.. _ahelp_ftosh:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ftosh: convert float images to shorts, by RW Cox
    Usage: ftosh [options] image_files ...
    
     where the image_files are in the same format to3d accepts
     and where the options are
    
      -prefix pname:  The output files will be named in the format
      -suffix sname:  'pname.index.sname' where 'pname' and 'sname'
      -start  si:     are strings given by the first 2 options.
      -step   ss:     'index' is a number, given by 'si+(i-1)*ss'
                      for the i-th output file, for i=1,2,...
                  *** Default pname = 'sh'
                  *** Default sname = nothing at all
                  *** Default si    = 1
                  *** Default ss    = 1
    
      -nsize:         Enforce the 'normal size' option, to make
                      the output images 64x64, 128x128, or 256x256.
    
      -scale sval:    'sval' and 'bval' are numeric values; if
      -base  bval:    sval is given, then the output images are
      -top   tval:    formed by scaling the inputs by the formula
                      'output = sval*(input-bval)'.
                  *** Default sval is determined by finding
                      V = largest abs(input-bval) in all the input
                      images and then sval = tval / V.
                  *** Default tval is 32000; note that tval is only
                      used if sval is not given on the command line.
