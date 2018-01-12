.. contents:: 
    :depth: 4 

*****
imreg
*****

.. code-block:: none

    Usage: imreg [options] base_image image_sequence ...
     * Registers each 2D image in 'image_sequence' to 'base_image'.
     * If 'base_image' = '+AVER', will compute the base image as
       the average of the images in 'image_sequence'.
     * If 'base_image' = '+count', will use the count-th image in the
       sequence as the base image.  Here, count is 1,2,3, ....
    
    OUTPUT OPTIONS:
      -nowrite        Don't write outputs, just print progress reports.
      -prefix pname   The output files will be named in the format
      -suffix sname   'pname.index.sname' where 'pname' and 'sname'
      -start  si      are strings given by the first 2 options.
      -step   ss      'index' is a number, given by 'si+(i-1)*ss'
                      for the i-th output file, for i=1,2,...
                    *** Default pname = 'reg.'
                    *** Default sname = nothing at all
                    *** Default si    = 1
                    *** Default ss    = 1
    
      -flim           Write output in mrilib floating point format
                      (which can be converted to shorts using program ftosh).
                    *** Default is to write images in format of first
                        input file in the image_sequence.
      -keepsize       Preserve the original image size on output.
                      Default is without this option, and results
                      in images that are padded to be square.
    
      -quiet          Don't write progress report messages.
      -debug          Write lots of debugging output!
    
      -dprefix dname  Write files 'dname'.dx, 'dname'.dy, 'dname'.phi
                        for use in time series analysis.
    
    ALIGNMENT ALGORITHMS:
      -bilinear       Uses bilinear interpolation during the iterative
                        adjustment procedure, rather than the default
                        bicubic interpolation. NOT RECOMMENDED!
      -modes c f r    Uses interpolation modes 'c', 'f', and 'r' during
                        the coarse, fine, and registration phases of the
                        algorithm, respectively.  The modes can be selected
                        from 'bilinear', 'bicubic', and 'Fourier'.  The
                        default is '-modes bicubic bicubic bicubic'.
      -mlcF           Equivalent to '-modes bilinear bicubic Fourier'.
    
      -wtim filename  Uses the image in 'filename' as a weighting factor
                        for each voxel (the larger the value the more
                        importance is given to that voxel).
    
      -dfspace[:0]    Uses the 'iterated diffential spatial' method to
                        align the images.  The optional :0 indicates to
                        skip the iteration of the method, and to use the
                        simpler linear differential spatial alignment method.
                        ACCURACY: displacments of at most a few pixels.
                    *** This is the default method (without the :0).
    
      -cmass            Initialize the translation estimate by aligning
                        the centers of mass of the images.
                  N.B.: The reported shifts from the registration algorithm
                        do NOT include the shifts due to this initial step.
    
    The new two options are used to play with the -dfspace algorithm,
    which has a 'coarse' fit phase and a 'fine' fit phase:
    
      -fine blur dxy dphi  Set the 3 'fine' fit parameters:
                             blur = FWHM of image blur prior to registration,
                                      in pixels [must be > 0];
                             dxy  = convergence tolerance for translations,
                                      in pixels;
                             dphi = convergence tolerance for rotations,
                                      in degrees.
    
      -nofine              Turn off the 'fine' fit algorithm. By default, the
