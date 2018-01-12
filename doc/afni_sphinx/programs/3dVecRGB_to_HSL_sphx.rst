.. contents:: 
    :depth: 4 

***************
3dVecRGB_to_HSL
***************

.. code-block:: none

    
      Convert a 3-brick RGB (red, green, blue) data set to an HSL (hue,
      saturation, luminance) one.
    
      Written by PA Taylor (Jan 2016), as part of FATCAT.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + USAGE: 
        Convert an RGB (red, green, blue) vector set to an HSL (hue, saturation,
        luminance) one. The input brick must have 3 bricks, one per component.
        The output HSL data set will have 3 (or 4, see below) bricks.
    
        For viewing the HSL set, one might want to use the AFNI/SUMA colorbar
        'Color_circle_AJJ' with the [0]th (Hue) brick. In SUMA, one might also
        set the brightness 'B' to be the [2]nd (Lum) brick.  Additionally, one
        can concatenate a fourth brick to the HSL output, and use *that* for
        setting the brightness value;  this feature was specifically added for
        the DTI tract volume viewing in SUMA, with the through of appending the
        FA values to the HSL information (see the ***to-be-named*** tract volume
        colorization script for more details).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND:
          3dVecRGBtoHSL -prefix PREFIX -in_vec FILE_V {-mask MASK}    \
                {-in_scal FILE_S}
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + RUNNING, need to provide:
        -prefix  PREFIX  :output file name part.
        -in_vec  FILE_V  :input RGB vector file of three bricks, presumably each
                          having values in the interval [0,1].
        -mask    MASK    :can include a whole brain mask within which to
                          calculate things. Otherwise, data should be masked
                          already.
        -in_scal FILE_S  :can input scalar a file (single brick), which will be
                          appended to the output file, with the utility of
                          being an extra set of 'brightness' values (mainly
                          aimed at loading in an FA data set for tract volume
                          coloration).  This input is not required.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE (such as prepping for tract volume viewing):
    
        3dVecRGB_to_HSL  -in_vec DT_V1+orig.  -in_scal DT_FA+orig     \
                         -mask mask+orig.  -prefix HSL
    
    ____________________________________________________________________________
    
