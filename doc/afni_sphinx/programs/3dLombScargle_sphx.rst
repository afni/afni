.. _ahelp_3dLombScargle:

*************
3dLombScargle
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
      Make a periodogram or amplitude-spectrum of a time series that has a
      non-constant sampling rate. The spectra output by this program are 
      'one-sided', so that they represent the half-amplitude or power
      associated with a frequency, and they would require a factor of 2 to 
      account for both the the right- and left-traveling frequency solutions 
      of the Fourier transform (see below 'OUTPUT' and 'NOTE').
    
      Of particular interest is the application of this functionality to 
      resting state time series that may have been censored.  The theory behind
      the mathematics and algorithms of this is due to separate groups, mainly
      in the realm of astrophysical applications: Vaníček (1969, 1971), 
      Lomb (1976), Scargle (1982), and Press & Rybicki (1989). Shoutout to them.
    
      This particular implementation is due to Press & Rybicki (1989), by
      essentially translating their published Fortran implementation into C,
      while using GSL for the FFT, instead of NR's realft(), and making
      several adjustments based on that. 
    
      The Lomb-Scargle adaption was done with fairly minimal changes here by
      PA Taylor (v1.4, June, 2016). 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
      + USAGE: 
          Input a 4D volumetric time series (BRIK/HEAD or NIFTI data set)
          as well as an optional 1D file of 0s and 1s that defines which points
          to censor out (i.e., each 0 represents a point/volume to censor out);
          if no 1D file is input, the program will check for volumes that are
          uniformly zero and consider those to be censored.
    
          The output is a LS periodogram, describing spectral magnitudes
          up to some 'maximum frequency'-- the default max here is what
          the Nyquist frequency of the time series *would have been* without
          any censoring.  (Interestingly, this analysis can actually be
          legitimately applied in cases to estimate frequency content >Nyquist.
          Wow!)
    
          The frequency spectrum will be in the range [df, f_N], where:
            df = 1/T, and T is the total duration of the uncensored time series;
            f_N = 1/dt, and dt is the sampling time (i.e., TR);
            and the interval of frequencies is also df.
          These ranges and step sizes should be *independent* of the censoring
          which is a nice property of the Lomb-Scargle-iness.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      + OUTPUT: 
        1) PREFIX_time.1D    :a 1D file of the sampled time points (in units of
                              seconds) of the analyzed (and possibly censored)
                              data set.
        2) PREFIX_freq.1D    :a 1D file of the frequency sample points (in units
                              of 1/seconds) of the output periodogram/spectrum
                              data set.
        3) PREFIX_amp+orig   :volumetric data set containing a LS-derived
                 or           amplitude spectrum (by default, named 'amp') or a
           PREFIX_pow+orig    power spectrum (see '-out_pow_spec', named 'pow')
                              one per voxel. 
                              Please note that the output amplitude and power
                              spectra are 'one-sided', to represent the 
                              *half* amplitude or power of a given frequency
                              (see the following note).
    
      + A NOTE ABOUT Fourier+Parseval matters (please forgive the awkward
       formatting):
          In the formulation used here, for a time series x[n] of length N, 
          the periodogram value S[k] is related to the amplitude value |X[k]|:
           (1)     S[k] = (|X[k]|)**2,
          for each k-th harmonic.
    
          Parseval's theorem relates time fluctuations to spectral amplitudes,
          stating that (for real time series with zero mean):
           (2)     sum_n{ x[n]**2 } = (1/N) * sum_k{ |X[k]|**2 }, 
                                    = (1/N) * sum_k{ S[k] }, 
          where n=0,1,..,N-1 and k=0,1,..,N-1 (NB: A[0]=0, for zero mean 
          series). The LHS is essentially the variance of the time series 
          (times N-1).  The above is derived from Fourier transform maths, and
          the Lomb-Scargle spectra are approximations to Fourier, so the above
          can be expected to approximately hold, if all goes well.
    
          Another Fourier-related result is that for real, discrete time series,
          the spectral amplitudes/power values are symmetric and periodic in N.
          Therefore, |X[k]| = |X[-k]| = |X[N-k-1]| (in zero-base array 
          counting);
          the distinction between positive- and negative-indexed frequencies
          can be thought of as signifying right- and left-traveling waves, which
          both contribute to the total power of a specific frequency.
          The upshot is that one could write the Parseval formula as:
           (3)     sum_n{ x[n]**2 } = (2/N) * sum_l{ |X[l]|**2 }, 
                                    = (2/N) * sum_l{ S[l] }, 
          where n=0,1,..,N-1 and l=0,1,..,(N/2)-1 (note the factor of 2 now
          appearing on the RHS relations). These symmetries/considerations
          are the reason why ~N/2 frequency values are output here (we assume 
          that only real-valued time series are input), without any loss of
          information.
    
          Additionally, with a view toward expressing the overall amplitude
          or power of a given frequency, which many people might want to use to 
          estimate spectral 'functional connectivity' parameters such as ALFF,
          fALFF, RSFA, etc. (using, for example, 3dAmptoRSFC), we therefore 
          note that the *total* amplitude or power of a given frequency would
          be:
                A[k] = 2*|X[k]|                 
                P[k] = 2*S[k] = 2*|X[k]|**2 = 0.5*A[k]**2    
          instead of just that of the left/right traveling part. These types of
          quantities (A and P) are also referred to as 'two-sided' spectra. The
          resulting Parseval relation could then be written:
           (4)     sum_n{ x[n]**2 } = (1/(2N)) * sum_l{ A[l]**2 }, 
                                    = (1/N) * sum_l{ P[l] }, 
          where n=0,1,..,N-1 and l=0,1,..,(N/2)-1. Somehow, it just seems easier
          to output the one-sided values, X and S, so that the Parsevalian
          summation rules look more similar.
    
          With all of that in mind, the 3dLombScargle results are output as
          follows. For amplitudes, the following approx. Parsevellian relation
          should hold between the 'holey' time series x[m] of M points and
          the frequency series Y[l] of L~M/2 points (where {|Y[l]|} approaches
          the Fourier amplitudes {|X[l]|} as the number of censored points 
          decreases and M->N):
           (5)     sum_m{ x[m]**2 } = (1/L) * sum_l{ Y[l]**2 }, 
          where m=0,1,..,M-1 and l=0,1,..,L-1. For the power spectrum T[l]
          of L~M/2 values, then:
           (6)     sum_m{ x[m]**2 } = (1/L) * sum_l{ T[l] } 
          for the same ranges of summations.
    
          So, please consider that when using the outputs of here. 3dAmpToRSFC
          is prepared for this when calculating spectral parameters (from 
          amplitudes).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND:  3dLombScargle -prefix PREFIX -inset FILE \
                      {-censor_1D C1D} {-censor_str CSTR} \
                      {-mask MASK} {-out_pow_spec}  \
                      {-nyq_mult N2}  {-nifti}  
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + RUNNING:
      -prefix PREFIX   :output prefix name for data volume, time point 1D file
                        and frequency 1D file.
      -inset FILE      :time series of volumes, a 4D volumetric data set.
    
      -censor_1D C1D   :single row or column of 1s (keep) and 0s (censored)
                        describing which volumes of FILE are kept in the
                        sampling and which are censored out, respectively. The
                        length of the list of numbers must be of the
                        same length as the number of volumes in FILE.
                        If not entered, then the program will look for subbricks
                        of all-zeros and assume those are censored out.
      -censor_str CSTR :AFNI-style selector string of volumes to *keep* in
                        the analysis.  Such as: 
                             '[0..4,7,10..$]'
                        Why we refer to it as a 'censor string' when it is
                        really the list of volumes to keep... well, it made
                        sense at the time.  Future historians can duel with
                        ink about it.
    
      -mask MASK       :optional, mask of volume to analyze; additionally, any
                        voxel with uniformly zero values across time will
                        produce a zero-spectrum.
    
      -out_pow_spec    :switch to output the amplitude spectrum of the freqs
                        instead of the periodogram.  In the formulation used
                        here, for a time series of length N, the power spectral
                        value S is related to the amplitude value X as:
                        S = (X)**2.
          NB --> You can both normalize and amplitude-ize the output values,
                if you wish. Or do neither. Or just do one of them. Your choice.
    
      -nyq_mult N2     :L-S periodograms can include frequencies above what
                        would typically be considered Nyquist (here defined
                        as:
                         f_N = 0.5*(number of samples)/(total time interval)
                        By default, the maximum frequency will be what
                        f_N *would* have been if no censoring of points had
                        occured. (This makes it easier to compare L-S spectra
                        across a group with the same scan protocol, even if
                        there are slight differences in censoring, per subject.)
                        Acceptable values are >0. (For those reading the 
                        algorithm papers, this sets the 'hifac' parameter.)
                        If you don't have a good reason for changing this,
                        dooon't change it!
      -nifti           :switch to output *.nii.gz volume file
                        (default format is BRIK/HEAD).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
            3dLombScargle -prefix LSout -inset TimeSeries.nii.gz \
                 -mask mask.nii.gz -censor_1D censor_list.txt
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     
     
