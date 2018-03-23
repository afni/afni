.. _ahelp_3dRetinoPhase:

*************
3dRetinoPhase
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dRetinoPhase [-prefix ppp]  dataset
       where dataset is a time series from a retinotpy stimulus
    
     -exp EXP: These four options specify the type of retinotpy 
     -con CON: stimulus. EXP and CON are for expanding and 
     -clw CLW : contracting rings, respectively. CLW and CCW are
     -ccw CCW: for clockwise and counter clockwise moving polar
               polar angle mapping stimuli. You can specify one, 
               or all stimuli in one command. When all are specified
               polar angle stimuli, and eccentricity stimuli of 
               opposite directions are combined.
     -prefix PREF: Prefix of output datasets. 
               PREF is suffixed with the following:
               .ecc+ for positive (expanding) eccentricity (EXP)
               .ecc- for negative (contracting) eccentricity (CON)
               .pol+ for clockwise polar angle mapping (CLW)
               .pol- for counterclockwise polar angle mapping (CCW)
      At a minimum each input gets a phase dataset output. It contains
         response phase (or delay) in degrees.
         If both directions are given for polar and/or eccentricity
         then a visual field angle data set is created.
         The visual field angle is obtained by averaging phases of opposite
         direction stimuli. The hemodynamic offset is half the phase difference.
    
      Each output also contains a thresholding sub-brick. Its type 
         depends on the phase estimation method (-phase_estimate).
    
                     Note on the thresholding sub-bricks
                     -----------------------------------
      Both FFT and DELAY values of -phase_estimate produce thresholding 
         sub-bricks with the phase estimates. Those thresholds have associated 
         significance levels, but they should be taken with a grain of 
         salt. There is no correction for autocorrelation, so the DOFs 
         are generous.
      The program also attaches a thresholding sub-brick to the
         visual field angle datasets which are estimated by averaging the phase
         estimates in order to remove the hemodynamic offset. This composite 
         thresholding sub-brick contains at each voxel/node, the maximum
         threshold from the datasets of stimli of opposite direction.
      This thresholding sub-brick is for convenience, allowing you to
         threshold with a mask that is the union of the individual
         thresholded maps. Significance levels are purposefully not
         attached. I don't know how to compute them properly.
    
     -spectra: Output amplitude and phase spectra datasets.
     -Tstim T: Period of stimulus in seconds. This parameter does
               not depend on the number of wedges or rings (Nr/Nw).
               It is the duration of a full cycle of the stimulus.
               Use -Tpol TPOL, and -Tecc TECC, to specify periods
               for each stimulus type separately. -Tstim sets both 
               periods to T.
     -nrings Nr: Nr is the number of rings in the stimulus. 
                  The default is 1.
     -nwedges Nw: Nw is the number of wedges in the stimulus. 
                  The default is 1.
     -ort_adjust: Number of DOF lost in detrending outside of this 
                  program.
     -pre_stim PRE: Blank period, in seconds, before stimulus began 
     -sum_adjust y/n: Adjust sum of angles for wrapping based on the
                      angle difference. Default is 'y'
     -phase_estimate METH: Select method of phase estimation
           METH == FFT  uses the phase of the fundamental frequency.
           METH == DELAY uses the 3ddelay approach for estimating
                         the phase. This requires the use of option
                         -ref_ts . See references [3] and [4] below. 
           The DELAY option appears to be good as the FFT for high SNR
              and high duty cycle. See results produced by @Proc.PK.All_D
              in the demo archive AfniRetinoDemo.tgz.
           However,the DELAY option seems much better for low duty cycle stimuli.
           It is not set as the default for backward compatibility. Positive and 
              negative feedback about this option are welcome.
    
         Thanks to Ikuko Mukai and Masaki Fukunaga for making the case 
            for DELAY's addition; they were right. 
    
     -ref_ts REF_TS: 0 lag reference time series of response. This is
                     needed for the DELAY phase estimation method.
          With the DELAY method, the phase results are comparable to 
            what you'd get with the following 3ddelay command:
            For illustration, say you have stimuli of 32 second periods
            with the polar stimuli having two wedges. After creating 
            the reference time series with waver (32 sec. block period 
            eccentricity, 32/2=16 sec. block period for polar), run 
            4 3ddelay commands as such:
                          for an expanding ring of 32 second period:
               3ddelay  -input exp.niml.dset \
                        -ideal_file ECC.1D   \
                        -fs 0.5  -T 32 \
                        -uD -nodsamp \
                        -phzreverse -phzscale 1.0 \
                        -prefix ecc+.del.niml.dset\n
                  Repeat for contracting ring, remove -phzreverse 
    
                           for clockwise two wedge of 32 second period:
               3ddelay  -input clw.niml.dset \
                        -ideal_file POL.1D   \
                        -fs 0.5  -T 16 \
                        -uD -nodsamp \
                        -phzreverse -phzscale 0.5 \
                        -prefix pol+.del.niml.dset\n
                  Repeat for counterclockwise remove -phzreverse 
         Instead of the 3ddelay mess, all you do is run 3dRetinoPhase with the 
            following extra options:               -phase_estimate DELAY -ref_ts ECC.1D
            or    -phase_estimate DELAY -ref_ts POL.1D
    
         If you are not familiar with the use of program 'waver' for creating
         reference time series, take a look at demo script @Proc.PK.All_D in
         AfniRetinoDemo.tgz.
    
     -multi_ref_ts MULTI_REF_TS: Multiple 0 lag reference time series. 
                                 This allows you to test multiple regressors.
                                 The program will run a separate analysis for 
                                 each regressor (column), and combine the results
                                 in the output dataset this way:
           ([.] denotes output sub-brick)
           [0]: Phase from regressor that yields the highest correlation coeff.
           [1]: Maximum correlation coefficient.
           [2]: Number of regressor that yields the highest correlation coeff.
                Counting begins at 1 (not 0)
           [3]: Phase from regressor 1
           [4]: Correlation coefficient from regressor 1
           [5]: Phase from regressor 2
           [6]: Correlation coefficient from regressor 2
           ... etc.
           In general, for regressor k (k starts at 1)
              [2*k+1] contains the Phase and [2*k+2] the Correlation coefficient
    
      N.B: If MULTI_REF_TS has only one timeseries, -multi_ref_ts produces
           an output identical to that of -ref_ts. 
    
      See usage in @RetinoProc and demo data in
      https://afni.nimh.nih.gov/pub/dist/tgz/AfniRetinoDemo.tgz 
    
    References for this program:
       [1] RW Cox.  AFNI: Software for analysis and visualization of functional
                          magnetic resonance neuroimages.  
                          Computers and Biomedical Research, 29: 162-173, 1996.
       [2] Saad Z.S., et al.  SUMA: An Interface For Surface-Based Intra- And
                          Inter-Subject Analysis With AFNI.
         Proc. 2004 IEEE International Symposium on Biomedical Imaging, 1510-1513
       If you use the DELAY method:
       [3] Saad, Z.S., et al. Analysis and use of FMRI response delays. 
             Hum Brain Mapp, 2001. 13(2): p. 74-93.
       [4] Saad, Z.S., E.A. DeYoe, and K.M. Ropella, Estimation of FMRI 
             Response Delays.  Neuroimage, 2003. 18(2): p. 494-504.
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
