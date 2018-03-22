***************
3dNormalityTest
***************

.. _ahelp_3dNormalityTest:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Program: 3dNormalityTest
    
    * This program tests the input values at each voxel for normality,
      using the Anderson-Darling method:
        http://en.wikipedia.org/wiki/Anderson-Darling_test
    
    * Each voxel must have at least 5 values (sub-bricks).
    
    * The resulting dataset has the Anderson-Darling statistic converted
      to an exponentially distributed variable, so it can be thresholded
      with the AFNI slider and display a nominal p-value below.  If you
      want the A-D statistic un-converted, use the '-noexp' option.
    
    * Conversion of the A-D statistic to a p-value is done via simulation
      of the null distribution.
    
    OPTIONS:
    --------
     -input dset  = Specifies the input dataset.
                    Alternatively, the input dataset can be given as the
                    last argument on the command line, after all other
                    options.
    
     -prefix ppp  = Specifies the name for the output dataset.
    
     -noexp       = Do not convert the A-D statistic to an exponentially
                    distributed value -- just leave the raw A-D score in
                    the output dataset.
     -pval        = Output the results as a pure (estimated) p-value.
    
    EXAMPLES:
    ---------
    (1) Simulate a 2D square dataset with the values being normal on one
    edge and exponentially distributed on the other, and mixed in-between.
    
      3dUndump -dimen 101 101 1 -prefix UUU
      3dcalc -datum float -a UUU+orig -b '1D: 0 0 0 0 0 0 0 0 0 0' -prefix NNN \
             -expr 'i*gran(0,1.4)+(100-i)*eran(4)'
      rm -f UUU+orig.*
      3dNormalityTest -prefix Ntest -input NNN+orig
      afni -com 'OPEN_WINDOW axialimage' Ntest+orig
    
    In the above script, the UUU+orig dataset is created just to provide a spatial
    template for 3dcalc.  The '1D: 0 ... 0' input to 3dcalc is a time template
    to create a dataset with 10 time points.  The values are random deviates,
    ranging from pure Gaussian where i=100 to pure exponential at i=0.
    
    (2) Simulate a single logistic random variable into a 1D file and compute
    the A-D nominal p-value:
    
      1deval -num 200 -expr 'lran(2)' > logg.1D
      3dNormalityTest -input logg.1D\' -prefix stdout: -pval
    
    Note the necessity to transpose the logg.1D file (with the \' operator),
    since 3D programs interpret each 1D file row as a voxel time series.
    
    ++ March 2012 -- by The Ghost of Carl Friedrich Gauss
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
