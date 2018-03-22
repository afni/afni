***********
1dCorrelate
***********

.. _ahelp_1dCorrelate:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dCorrelate [options] 1Dfile 1Dfile ...
    ------
     * Each input 1D column is a collection of data points.
     * The correlation coefficient between each column pair is computed, along
       with its confidence interval (via a bias-corrected bootstrap procedure).
     * The minimum sensible column length is 7.
     * At least 2 columns are needed [in 1 or more .1D files].
     * If there are N input columns, there will be N*(N-1)/2 output rows.
     * Output appears on stdout; redirect ('>' or '>>') as needed.
     * Only one correlation method can be used in one run of this program.
     * This program is basically the basterd offspring of program 1ddot.
     * Also see http://en.wikipedia.org/wiki/Confidence_interval
    
    -------
    Methods   [actually, only the first letter is needed to choose a method]
    -------   [and the case doesn't matter: '-P' and '-p' both = '-Pearson']
    
     -Pearson  = Pearson correlation                    [the default method]
     -Spearman = Spearman (rank) correlation      [more robust vs. outliers]
     -Quadrant = Quadrant (binarized) correlation  [most robust, but weaker]
     -Ktaub    = Kendall's tau_b 'correlation'    [popular somewhere, maybe]
    
    -------------
    Other Options  [these options cannot be abbreviated!]
    -------------
    
     -nboot B  = Set the number of bootstrap replicates to 'B'.
                 * The default value of B is 4000.
                 * A larger number will give somewhat more accurate
                   confidence intervals, at the cost of more CPU time.
    
     -alpha A  = Set the 2-sided confidence interval width to '100-A' percent.
                 * The default value of A is 5, giving the 2.5..97.5% interval.
                 * The smallest allowed A is 1 (0.5%..99.5%) and the largest
                   allowed value of A is 20 (10%..90%).
                 * If you are interested assessing if the 'p-value' of a
                   correlation is smaller than 5% (say), then you should use
                   '-alpha 10' and see if the confidence interval includes 0.
    
     -block    = Attempt to allow for serial correlation in the data by doing
       *OR*      variable-length block resampling, rather than completely
     -blk        random resampling as in the usual bootstrap.
                 * You should NOT do this unless you believe that serial
                   correlation (along each column) is present and significant.
                 * Block resampling requires at least 20 data points in each
                   input column.  Fewer than 20 will turn off this option.
    -----
    Notes
    -----
    * For each pair of columns, the output include the correlation value
      as directly calculated, plus the bias-corrected bootstrap value, and
      the desired (100-A)% confidence interval [also via bootstrap].
    
    * The primary purpose of this program is to provide an easy way to get
      the bootstrap confidence intervals, since people almost always seem to use
      the asymptotic normal theory to decide if a correlation is 'significant',
      and this often seems misleading to me [especially for short columns].
    
    * Bootstrapping confidence intervals for the inverse correlations matrix
      (i.e., partial correlations) would be interesting -- anyone out there
      need this ability?
    
    -------------
    Sample output  [command was '1dCorrelate -alpha 10 A2.1D B2.1D']
    -------------
    # Pearson correlation [n=12 #col=2]
    # Name      Name       Value   BiasCorr   5.00%   95.00%  N: 5.00% N:95.00%
    # --------  --------  -------- -------- -------- -------- -------- --------
      A2.1D[0]  B2.1D[0]  +0.57254 +0.57225 -0.03826 +0.86306 +0.10265 +0.83353
    
    * Bias correction of the correlation had little effect; this is very common.
      ++ To be clear, the bootstrap bias correction is to allow for potential bias
         in the statistical estimate of correlation when the sample size is small.
      ++ It cannot correct for biases that result from faulty data (or faulty
         assumptions about the data).
    
    * The correlation is NOT significant at this level, since the CI (confidence
      interval) includes 0 in its range.
    
    * For the Pearson method ONLY, the last two columns ('N:', as above) also
      show the widely used asymptotic normal theory confidence interval.  As in
      the example, the bootstrap interval is often (but not always) wider than
      the theoretical interval.
    
    * In the example, the normal theory might indicate that the correlation is
      significant (less than a 5% chance that the CI includes 0), but the
      bootstrap CI shows that is not a reasonable statistical conclusion.
      ++ The principal reason that I wrote this program was to make it easy
         to check if the normal (Gaussian) theory for correlation significance
         testing is reasonable in any given case -- for small samples, it often
         is NOT reasonable!
    
    * Using the same data with the '-S' option gives the table below, again
      indicating that there is no significant correlation between the columns
      (note also the lack of the 'N:' results for Spearman correlation):
    
    # Spearman correlation [n=12 #col=2]
    # Name      Name       Value   BiasCorr   5.00%   95.00%
    # --------  --------  -------- -------- -------- --------
      A2.1D[0]  B2.1D[0]  +0.46154 +0.42756 -0.23063 +0.86078
    
    -------------
    SAMPLE SCRIPT
    -------------
    This script generates random data and correlates it until it is
    statistically significant at some level (default=2%).  Then it
    plots the data that looks correlated.  The point is to show what
    purely random stuff that appears correlated can look like.
    (Like most AFNI scripts, this is written in tcsh, not bash.)
    
    #!/bin/tcsh
    set npt = 20
    set alp = 2
    foreach fred ( `count -dig 1 1 1000` )
      1dcat jrandom1D:${npt},2 > qqq.1D
      set aabb = ( `1dCorrelate -spearman -alpha $alp qqq.1D  | grep qqq.1D | colrm 1 42` )
      set ab = `ccalc -form rint "1000 * $aabb[1] * $aabb[2]"`
      echo $fred $ab
      if( $ab > 1 )then
        1dplot -one -noline -x qqq.1D'[0]' -xaxis -1:1:20:5 -yaxis -1:1:20:5            \
               -DAFNI_1DPLOT_BOXSIZE=0.012                                              \
               -plabel "N=$npt trial#=$fred \alpha=${alp}% => r\in[$aabb[1],$aabb[2]]"  \
               qqq.1D'[1]'
        break
      endif
    end
    \rm qqq.1D
    
    ----------------------------------------------------------------------
    *** Written by RWCox (AKA Zhark the Mad Correlator) -- 19 May 2011 ***
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
