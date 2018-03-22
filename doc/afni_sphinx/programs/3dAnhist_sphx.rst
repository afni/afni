********
3dAnhist
********

.. _3dAnhist:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAnhist [options] dataset
    Input dataset is a T1-weighted high-res of the brain (shorts only).
    Output is a list of peaks in the histogram, to stdout, in the form
      ( datasetname #peaks peak1 peak2 ... )
    In the C-shell, for example, you could do
      set anhist = `3dAnhist -q -w1 dset+orig`
    Then the number of peaks found is in the shell variable $anhist[2].
    
    Options:
      -q  = be quiet (don't print progress reports)
      -h  = dump histogram data to Anhist.1D and plot to Anhist.ps
      -F  = DON'T fit histogram with stupid curves.
      -w  = apply a Winsorizing filter prior to histogram scan
             (or -w7 to Winsorize 7 times, etc.)
      -2  = Analyze top 2 peaks only, for overlap etc.
    
      -label xxx = Use 'xxx' for a label on the Anhist.ps plot file
                    instead of the input dataset filename.
      -fname fff = Use 'fff' for the filename instead of 'Anhist'.
    
    If the '-2' option is used, AND if 2 peaks are detected, AND if
    the -h option is also given, then stdout will be of the form
      ( datasetname 2 peak1 peak2 thresh CER CJV count1 count2 count1/count2)
    where 2      = number of peaks
          thresh = threshold between peak1 and peak2 for decision-making
          CER    = classification error rate of thresh
          CJV    = coefficient of joint variation
          count1 = area under fitted PDF for peak1
          count2 = area under fitted PDF for peak2
          count1/count2 = ratio of the above quantities
    NOTA BENE
    ---------
    * If the input is a T1-weighted MRI dataset (the usual case), then
       peak 1 should be the gray matter (GM) peak and peak 2 the white
       matter (WM) peak.
    * For the definitions of CER and CJV, see the paper
       Method for Bias Field Correction of Brain T1-Weighted Magnetic
       Resonance Images Minimizing Segmentation Error
       JD Gispert, S Reig, J Pascau, JJ Vaquero, P Garcia-Barreno,
       and M Desco, Human Brain Mapping 22:133-144 (2004).
    * Roughly speaking, CER is the ratio of the overlapping area of the
       2 peak fitted PDFs to the total area of the fitted PDFS.  CJV is
       (sigma_GM+sigma_WM)/(mean_WM-mean_GM), and is a different, ad hoc,
       measurement of how much the two PDF overlap.
    * The fitted PDFs are NOT Gaussians.  They are of the form
       f(x) = b((x-p)/w,a), where p=location of peak, w=width, 'a' is
       a skewness parameter between -1 and 1; the basic distribution
       is defined by b(x)=(1-x^2)^2*(1+a*x*abs(x)) for -1 < x < 1.
    
    -- RWCox - November 2004
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
