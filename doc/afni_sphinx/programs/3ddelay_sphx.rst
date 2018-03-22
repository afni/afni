*******
3ddelay
*******

.. _ahelp_3ddelay:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    The program estimates the time delay between each voxel time series    
    in a 3D+time dataset and a reference time series[1][2].                
    The estimated delays are relative to the reference time series.
    For example, a delay of 4 seconds means that the voxel time series 
    is delayed by 4 seconds with respect to the reference time series.
    
                                                                           
    Usage:                                                                 
    3ddelay                                                                 
    -input fname       fname = filename of input 3d+time dataset           
                       DO NOT USE CATENATED timeseries! Time axis is assumed
                       to be continuous and not evil.
    -ideal_file rname  rname = input ideal time series file name           
       The length of the reference time series should be equal to           
         that of the 3d+time data set. 
         The reference time series vector is stored in an ascii file.        
         The programs assumes that there is one value per line and that all  
         values in the file are part of the reference vector.                
         PS: Unlike with 3dfim, and FIM in AFNI, values over 33333 are treated
         as part of the time series.                                          
    -fs fs             Sampling frequency in Hz. of data time series (1/TR). 
    -T  Tstim          Stimulus period in seconds. 
                       If the stimulus is not periodic, you can set Tstim to 0.
    [-prefix bucket]   The prefix for the results Brick.
                       The first subbrick is for Delay.
                       The second subbrick is for Covariance, which is an 
                       estimate of the power in voxel time series at the
                       frequencies present in the reference time series.
                       The third subbrick is for the Cross Correlation 
                       Coefficients between FMRI time series and reference time
                       series. The fourth subbrick contains estimates of the
                       Variance of voxel time series. 
                       The default prefix is the prefix of the input dset 
                       with a '.DEL' extension appended to it.
    
    [-polort order]    Detrend input time series with polynomial of order
                       'order'. If you use -1 for order then the program will
                       suggest an order for you (about 1 for each 150 seconds)
                       The minimum recommended is 1. The default is -1 for auto
                       selection. This is the same as option Nort in the plugin
                       version.
    [-nodtrnd]         Equivalent to polort 0, whereby only the mean is removed.
               NOTE:   Regardless of these detrending options, No detrending is 
                       done to the reference time series.
    
    [-uS/-uD/-uR]      Units for delay estimates. (Seconds/Degrees/Radians)
                       You can't use Degrees or Radians as units unless 
                       you specify a value for Tstim > 0.
    [-phzwrp]          Delay (or phase) wrap.
                       This switch maps delays from: 
                       (Seconds) 0->T/2 to 0->T/2 and T/2->T to -T/2->0
                       (Degrees) 0->180 to 0->180 and 180->360 to -180->0
                       (Radians) 0->pi to 0->pi and pi->2pi to -pi->0
                       You can't use this option unless you specify a 
                       value for Tstim > 0.
    [-nophzwrp]        Do not wrap phase (default).
    [-phzreverse]      Reverse phase such that phase -> (T-phase)
    [-phzscale SC]     Scale phase: phase -> phase*SC (default no scaling)
    
    [-bias]            Do not correct for the bias in the estimates [1][2]
    [-nobias | -correct_bias] Do correct for the bias in the estimates
                              (default).
    
    [-dsamp]           Correct for slice timing differences        (default).
    [-nodsamp ]        Do not correct for slice timing differences .
    
    [-mask mname]      mname = filename of 3d mask dataset                 
                       only voxels with non-zero values in the mask would be 
                       considered.                                           
    
    [-nfirst fnum]     fnum = number of first dataset image to use in      
                         the delay estimate. (default = 0)                 
    [-nlast  lnum]     lnum = number of last dataset image to use in       
                         the delay estimate. (default = last)              
    
    [-co CCT]          Cross Correlation Coefficient threshold value.
                       This is only used to limit the ascii output (see below).
    
    [-asc [out]]       Write the results to an ascii file for voxels with 
    [-ascts [out]]     cross correlation coefficients larger than CCT.
                       If 'out' is not specified, a default name similar 
                       to the default output prefix is used.
                       -asc, only files 'out' and 'out.log' are written to disk
                       (see ahead)
                       -ascts, an additional file, 'out.ts', is written to disk
                       (see ahead)
                       There a 9 columns in 'out' which hold the following
                       values:
                        1- Voxel Index (VI) : Each voxel in an AFNI brick has a
                              unique index.
                              Indices map directly to XYZ coordinates.
                              See AFNI plugin documentations for more info.
                        2..4- Voxel coordinates (X Y Z): Those are the voxel 
                              slice coordinates. You can see these coordinates
                              in the upper left side of the AFNI window.
                              To do so, you must first switch the voxel 
                              coordinate units from mm to slice coordinates. 
                              Define Datamode -> Misc -> Voxel Coords ?
                              PS: The coords that show up in the graph window
                                  may be different from those in the upper left
                                  side of AFNI's main window.
                        5- Duff : A value of no interest to you. It is preserved
                                  for backward compatibility.
                        6- Delay (Del) : The estimated voxel delay.
                        7- Covariance (Cov) : Covariance estimate.
                        8- Cross Correlation Coefficient (xCorCoef) : 
                              Cross Correlation Coefficient.
                        9- Variance (VTS) : Variance of voxel's time series.
    
                       The file 'out' can be used as an input to two plugins:
                         '4Ddump' and '3D+t Extract'
    
                       The log file 'out.log' contains all parameter settings 
                       used for generating the output brick. 
                       It also holds any warnings generated by the plugin.
                       Some warnings, such as 'null time series ...' , or 
                       'Could not find zero crossing ...' are harmless. '
                       I might remove them in future versions.
    
                       A line (L) in the file 'out.ts' contains the time series 
                       of the voxel whose results are written on line (L) in the
                       file 'out'.
                       The time series written to 'out.ts' do not contain the
                       ignored samples, they are detrended and have zero mean.
    
                                                                          
    Random Comments/Advice:
       The longer you time series, the better. It is generally recomended that
       the largest delay be less than N/10, N being time series' length.
       The algorithm does go all the way to N/2.
    
       If you have/find questions/comments/bugs about the plugin, 
       send me an E-mail: saadz@mail.nih.gov
    
                              Ziad Saad Dec 8 00.
    
       [1] : Bendat, J. S. (1985). The Hilbert transform and applications 
             to correlation measurements, Bruel and Kjaer Instruments Inc.
              
       [2] : Bendat, J. S. and G. A. Piersol (1986). Random Data analysis and
             measurement procedures, John Wiley & Sons.
       Author's publications on delay estimation using the Hilbert Transform:
       [3] : Saad, Z.S., et al., Analysis and use of FMRI response delays. 
             Hum Brain Mapp, 2001. 13(2): p. 74-93.
       [4] : Saad, Z.S., E.A. DeYoe, and K.M. Ropella, Estimation of FMRI 
             Response Delays.  Neuroimage, 2003. 18(2): p. 494-504.
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
