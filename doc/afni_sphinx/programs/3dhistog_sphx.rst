********
3dhistog
********

.. _ahelp_3dhistog:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Compute histogram of 3D Dataset
    Usage: 3dhistog [editing options] [histogram options] dataset
    
    The editing options are the same as in 3dmerge
     (i.e., the options starting with '-1').
    
    The histogram options are:
      -nbin #   Means to use '#' bins [default=100]
      -dind i   Means to take data from sub-brick #i, rather than #0
      -omit x   Means to omit the value 'x' from the count;
                  -omit can be used more than once to skip multiple values.
      -mask m   Means to use dataset 'm' to determine which voxels to use
      -roi_mask r Means to create a histogram for each non-zero value in 
                  dataset 'r'. If -mask option is also used, dataset 'r' is 
                  masked by 'm' before creating the histograms.
      -doall    Means to include all sub-bricks in the calculation;
                  otherwise, only sub-brick #0 (or that from -dind) is used.
      -noempty  Only output bins that are not empty.
                This does not apply to NIML output via -prefix.
      -notitle  Means to leave the title line off the output.
      -log10    Output log10() of the counts, instead of the count values.
                This option cannot be used with -pdf or with -prefix
      -pdf      Output the counts divided by the number of samples.
                This option is only valid with -prefix
      -min x    Means specify minimum (inclusive) of histogram.
      -max x    Means specify maximum (inclusive) of histogram.
      -igfac    Means to ignore sub-brick scale factors and histogram-ize
                  the 'raw' data in each volume.
    
      Output options for integer and floating point data
      By default, the program will determine if the data is integer or float
       even if the data is stored as shorts with a scale factor.
      Integer data will be binned by default to be 100 or the maximum number of
       integers in the range, whichever is less. For example, data with the range
       (0..20) gives 21 bins for each integer, and non-integral bin boundaries
       will be raised to the next integer (2.3 will be changed to 3, for instance).
      If the number of bins is higher than the number of integers in the range,
       the bins will be labeled with floating point values, and multiple bins
       may be zero between the integer values
      Float data will be binned by default to 100 bins with absolute limits for
       the min and max if these are specified as inclusive. For example,
       float data ranging from (0.0 to 20.0) will be binned into bins that
       are 0.2 large  (0..0.199999, 0.2..0.399999,...,19.8..20.0)
      To have bins divided at 1.0 instead, specify the number of bins as 20
       Bin 0 is 0..0.9999, Bin 1 is 1.0 to 1.9999, ..., Bin 20 is 19 to 20.0000
       giving a slight bias to the last bin
    
      -int      Treat data and output as integers
      -float    Treat data and output as floats
      -unq U.1D Writes out the sorted unique values to file U.1D.
                This option is not allowed for float data
                If you have a problem with this, write
                Ziad S. Saad (saadz@mail.nih.gov)
      -prefix HOUT: Write a copy of the histogram into file HOUT.1D
                    you can plot the file with:
                 1dplot -hist -sepscl -x HOUT.1D'[0]' HOUT.1D'[1,2]' 
            or   
                 1dRplot -input HOUT.1D
    
    Without -prefix, the histogram is written to stdout.  
    Use redirection '>' if you want to save it to a file.
    The format is a title line, then three numbers printed per line:
      bottom-of-interval  count-in-interval  cumulative-count
    
    There is no 1dhistog program, for the simple reason that you can use
    this program for the same purpose, as in this example:
      3dhistog -nbin 50 -notitle -min 0 -max .01 err.1D > ehist.1D
      1dplot -hist -x ehist.1D'[0]' -xlabel 'err.1D' -ylabel 'histo' ehist.1D'[1]'
    
    -- by RW Cox, V Roopchansingh, and ZS Saad
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
