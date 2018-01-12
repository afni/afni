.. contents:: 
    :depth: 4 

****
tfim
****

.. code-block:: none

    MCW TFIM: t-tests on sets of functional images, by RW Cox
    
    Usage 1: tfim [options] -set1 image_files ... -set2 image_files ...
    Usage 2: tfim [options] -base1 bval -set2 image_files ...
    
    In usage 1, the collection of images files after '-set1' and the
    collection after '-set2' are averaged and differenced, and the
    difference is tested for significance with a 2 sample Student t-test.
    
    In usage 2, the collection of image files after '-set2' is averaged
    and then has the constant numerical value 'bval' subtracted, and the
    difference is tested for significance with a 1 sample Student t-test.
    
    N.B.: The input images can be in the usual 'short' or 'byte'
          formats, or in the floating point 'flim' format.
    N.B.: If in either set of images, a given pixel has zero variance
          (i.e., is constant), then the t-test is not performed.
          In that pixel, the .tspm file will be zero.
    
    Options are:
    
     -prefix pname: 'pname' is used as the prefix for the output
                      filenames.  The output image files are
                       + pname.diff = average of set2 minus average of set1
                                      (or minus 'bval')
                       + pname.tspm = t-statistic of difference
                      Output images are in the 'flim' (floating pt. image)
                      format, and may be converted to 16 bit shorts using
                      the program 'ftosh'.
                  *** The default 'pname' is 'tfim', if -prefix isn't used.
     -pthresh pval: 'pval' is a numeric value between 0 and 1, giving
                      the significance level (per voxel) to threshold the
                      output with; voxels with (2-sided) t-statistic
                      less significant than 'pval' will have their diff
                      output zeroed.
                  *** The default is no threshold, if -pthresh isn't used.
     -eqcorr dval:  If present, this option means to write out the file
                       pname.corr = equivalent correlation statistic
                                  =  t/sqrt(dof+t^2)
                      The number 'dval' is the value to use for 'dof' if
                      dval is positive.  This would typically be the total
                      number of data images used in forming the image sets,
                      if the image sets are from sfim or fim.
                      If dval is zero, then dof is computed from the number
                      of images in -set1 and -set2; if these are averages
                      from program sfim, then dof will be smallish, which in
                      turn means that significant corr values will be higher
                      than you may be used to from using program fim.
                  *** The default is not to write, if -eqcorr isn't used.
     -paired:       If present, this means that -set1 and -set2 should be
                      compared using a paired sample t-test.  This option is
                      illegal with the -base1 option.  The number of samples
                      in the two sets of images must be equal.
                      [This test is implemented by subtracting -set1 images
                       from the -set2 images, then testing as in '-base1 0'.]
                  *** The default is to do an unpaired test, if -paired isn't
                      used.  In that case, -set1 and -set2 don't need to have
