***********
3dAmpToRSFC
***********

.. _3dAmpToRSFC:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
      This program is for converting spectral amplitudes into standard RSFC
      parameters.  This function is made to work directly with the outputs of
      3dLombScargle, but you could use other inputs that have similar 
      formatting. (3dLombScargle's main algorithm is special because it
      calculates spectra from time series with nonconstant sampling, such as if
      some time points have been censored during processing-- check it out!.)
    
      At present, 6 RSFC parameters get returned in separate volumes:
         ALFF, mALFF, fALFF, RSFA, mRSFA and fRSFA.
      For more information about each RSFC parameter, see, e.g.:   
         ALFF/mALFF -- Zang et al. (2007),
         fALFF --      Zou et al. (2008),
         RSFA --       Kannurpatti & Biswal (2008).
      You can also see the help of 3dRSFC, as well as the Appendix of 
      Taylor, Gohel, Di, Walter and Biswal (2012) for a mathematical
      description and set of relations.
    
      NB: *if* you want to input an unbandpassed time series and do some
      filtering/other processing at the same time as estimating RSFC parameters,
      then you would want to use 3dRSFC, instead.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND: 
            3dAmpToRSFC { -in_amp AMPS | -in_pow POWS } -prefix PREFIX \
                -band FBOT FTOP  { -mask MASK } { -nifti }
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + RUNNING:
    
       -in_amp AMPS   :input file of one-sided spectral amplitudes, such as
                        output by 3dLombScargle.  It is also assumed that the
                        the frequencies are uniformly spaced with a single DF
                        ('delta f'), and that the zeroth brick is at 1*DF (i.e.
                        that the zeroth/baseline frequency is not present in the
             or         spectrum.
       -in_pow POWS    :input file of a one-sided power spectrum, such as
                        output by 3dLombScargle.  Similar freq assumptions
                        as in '-in_amp ...'.
    
       -band FBOT FTOP :lower and upper boundaries, respectively, of the low
                        frequency fluctuations (LFFs), which will be in the
                        inclusive interval [FBOT, FTOP], within the provided
                        input file's frequency range.
       -prefix PREFIX  :output file prefix; file names will be: PREFIX_ALFF*,
                        PREFIX_FALFF*, etc.
    
       -mask MASK      :volume mask of voxels to include for calculations; if
                        no mask is included, values are calculated for voxels
                        whose values are not identically zero across time.
       -nifti          :output files as *.nii.gz (default is BRIK/HEAD).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      + OUTPUT: 
           Currently, 6 volumes of common RSFC parameters, briefly:
              PREFIX_ALFF+orig    :amplitude of low freq fluctuations
                                   (L1 sum).
              PREFIX_MALFF+orig   :ALFF divided by the mean value within
                                   the input/estimated whole brain mask
                                   (a.k.a. 'mean-scaled ALFF').
              PREFIX_FALFF+orig   :ALFF divided by sum of full amplitude
                                   spectrum (-> 'fractional ALFF').
              PREFIX_RSFA+orig    :square-root of summed square of low freq
                                   fluctuations (L2 sum).
              PREFIX_MRSFA+orig   :RSFA divided by the mean value within
                                   the input/estimated whole brain mask
                                   (a.k.a. 'mean-scaled RSFA').
              PREFIX_FRSFA+orig   :ALFF divided by sum of full amplitude
                                   spectrum (a.k.a. 'fractional RSFA').
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
            3dAmpToRSFC                         \
                -in_amp SUBJ_01_amp.nii.gz     \
                -prefix  SUBJ_01                \
                -mask    mask_WB.nii.gz         \
                -band    0.01  0.1              \
                -nifti 
    
