*****
3dfim
*****

.. _ahelp_3dfim:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

     Program:   3dfim 
    
    Purpose:   Calculate functional image from 3d+time data file. 
    Usage:     3dfim  [-im1 num]  -input fname  -prefix name 
                  -ideal fname  [-ideal fname] [-ort fname] 
     
     options are:
     -im1 num        num   = index of first image to be used in time series 
                             correlation; default is 1  
      
     -input fname    fname = filename of 3d + time data file for input
      
     -prefix name    name  = prefix of filename for saving functional data
      
     -ideal fname    fname = filename of a time series to which the image data
                             is to be correlated. 
      
     -percent p      Calculate percentage change due to the ideal time series 
                     p     = maximum allowed percentage change from baseline 
                             Note: values greater than p are set equal to p. 
      
     -ort fname      fname = filename of a time series to which the image data
                             is to be orthogonalized 
      
                 N.B.: It is possible to specify more than
                 one ideal time series file. Each one is separately correlated
                 with the image time series and the one most highly correlated
                 is selected for each pixel.  Multiple ideals are specified
                 using more than one '-ideal fname' option, or by using the
                 form '-ideal [ fname1 fname2 ... ]' -- this latter method
                 allows the use of wildcarded ideal filenames.
                 The '[' character that indicates the start of a group of
                 ideals can actually be any ONE of these: [{/%
                 and the ']' that ends the group can be:  ]}/%
      
                 [Format of ideal time series files:
                 ASCII; one number per line;
                 Same number of lines as images in the time series;
                 Value over 33333 --> don't use this image in the analysis]
      
                 N.B.: It is also possible to specify more than
                 one ort time series file.  The image time series is  
                 orthogonalized to each ort time series.  Multiple orts are 
                 specified by using more than one '-ort fname' option, 
                 or by using the form '-ort [ fname1 fname2 ... ]'.  This 
                 latter method allows the use of wildcarded ort filenames.
                 The '[' character that indicates the start of a group of
                 ideals can actually be any ONE of these: [{/%
                 and the ']' that ends the group can be:  ]}/%
      
                 [Format of ort time series files:
                 ASCII; one number per line;
                 At least same number of lines as images in the time series]
      
      
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
