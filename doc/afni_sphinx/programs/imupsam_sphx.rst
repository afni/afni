.. _ahelp_imupsam:

*******
imupsam
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: imupsam [-A] n input_image output_image
    
    *** Consider using the newer imcat for resampling.
        byte and rgb images
    
    * Upsamples the input 2D image by a factor of n and
        writes result into output_image; n must be an
        integer in the range 2..30.
    * 7th order polynomial interpolation is used in each
        direction.
    * Inputs can be complex, float, short, PGM, PPM, or JPG.
    * If input_image is in color (PPM or JPG), output will
        be PPM unless output_image ends in '.jpg'.
    * If output_image is '-', the result will be written
        to stdout (so you could pipe it into something else).
    * The '-A' option means to write the result in ASCII
        format: all the numbers for the file are output,
        and nothing else (no header info).
