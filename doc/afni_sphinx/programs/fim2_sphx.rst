.. _ahelp_fim2:

****
fim2
****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

     Usage: fim2 [options] image_files ...
     where 'image_files ...' is a sequence of MRI filenames,
      
     options are:
     -pcnt #         correlation coeff. threshold will be 1 - 0.01 * #
     -pcthresh #     correlation coeff. threshold will be #
     -im1 #          index of image file to use as first in time series;
                       default is 1; previous images are filled with this
                       image to synchronize with the reference time series
     -num #          number of images to actually use, if more than this
                       many are specified on the command line;  default is
                       to use all images
     -non            this option turns off the default normalization of
                       the output activation image;  the user should provide
                       a scaling factor via '-coef #', or '1' will be used
     -coef #         the scaling factor used to convert the activation output
                       from floats to short ints (if -non is also present)
      
     -ort fname      fname = filename of a time series to which the image data
                       will be orthogonalized before correlations are computed;
                       any number of -ort options (from 0 on up) may be used
     -ideal fname    fname = filename of a time series to which the image data
                       is to be correlated; exactly one such time series is
                       required;  if the -ideal option is not used, then the
                       first filename after all the options will be used
           N.B.: This version of fim2 allows the specification of more than
                 one ideal time series file.  Each one is separately correlated
                 with the image time series and the one most highly correlated
                 is selected for each pixel.  Multiple ideals are specified
                 using more than one '-ideal fname' option, or by using the
                 form '-ideal [ fname1 fname2 ... ]' -- this latter method
                 allows the use of wildcarded ideal filenames.
                 The '[' character that indicates the start of a group of
                 ideals can actually be any ONE of these: [{/%
                 and the ']' that ends the group can be:  ]}/%
      
           [Format of ort and ideal time series files:
            ASCII; one number per line;
            Same number of lines as images in the time series;
            Value over 33333 --> don't use this image in the analysis]
      
     -polref #       use polynomials of order 0..# as extra 'orts';
     [or -polort #]    default is 0 (yielding a constant vector).
                       Use # = -1 to suppress this feature.
      
     -fimfile fname  fname = filename to save activation magnitudes in;
                       if not given, the last name on the command line will
                       be used
     -corr           if present, indicates to write correlation output to
                       image file 'fimfile.CORR' (next option is better)
     -corfile fname  fname = filename to save correlation image in;
                       if not present, and -corr is not present, correlation
                       image is not saved.
     -cnrfile fname  fname = filename to save contrast-to-noise image in;
                       if not present, will not be computed or saved;
                       CNR is scaled by 100 if images are output as shorts
                       and is written 'as-is' if output as floats (see -flim).
                       [CNR is defined here to be alpha/sigma, where
                        alpha = amplitude of normalized ideal in a pixel
                        sigma = standard deviation of pixel after removal
                                of orts and ideal
                        normalized ideal = ideal scaled so that trough-to-peak
                          height is one.]
     -sigfile fname  fname = filename to save standard deviation image in;
                       the standard deviation is of what is left after the
                       least squares removal of the -orts, -polrefs, and -ideal.
                      N.B.: This is always output in the -flim format!
     -fitfile fname  Image files of the least squares fit coefficients of
                       all the -ort and -polref time series that
                       are projected out of the data time series before
                       the -ideal is fit.  The actual filenames will
                       be fname.01 fname.02 ....
                       Their order is -orts, then -polrefs, and last -ideal.
                      N.B.: These are always output in the -flim format!
     -subort fname   A new timeseries of images is written to disk, with
                       names of the form 'fname.0001', etc.  These images
                       have the orts and polrefs (but not ideals) subtracted out.
                      N.B.: These are always output in the -flim format!
     -flim           If present, write outputs in mrilib 'float' format,
                       rather than scale+convert to integers
                       [The 'ftosh' program can later convert to short integers]
     -clean          if present, then output images won't have the +/- 10000
                       values forced into their corners for scaling purposes.
     -clip           if present, output correlations, etc., will be set to
                       zero in regions of low intensity.
     -q              if present, indicates 'quiet' operation.
     -dfspace[:0]    Indicates to use the 'dfspace' filter (a la imreg) to
                       register the images spatially before filtering.
     -regbase fname   Indicates to read image in file 'fname' as the base
                       image for registration.  If not given, the first image
                       in the time series that is used in the correlation
                       computations will be used.  This is also the image
                       that is used to define 'low intensity' for the -clip option.
