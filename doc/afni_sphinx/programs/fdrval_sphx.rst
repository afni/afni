******
fdrval
******

.. _ahelp_fdrval:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: fdrval [options] dset sub val [val ...]
    
    Reads FDR curve data from the header of dset for sub-brick
    #sub and computes the q-value when the sub-brick statistical
    threshold is set to val.
    
    OPTIONS
    -------
     -pval   = also output the p-value (on the same line, after q)
     -ponly  = don't output q-values, just p-values
     -qonly  = don't output p-values, just q-values [the default]
    
     -qinput = The 'val' inputs are taken to be q-values and then the
      *OR*     outputs are the corresponding statistical thresholds.
     -inverse  This is the inverse of the usual operation.
               * With this option, all 'val' inputs must be between 0 and 1
                 (exclusive), or bad things will happen and the program will
                 send e-mail to your mother explaining how stupid you are.
               * You cannot use '-ponly' or '-pval' with this option.
               * For example, if you do
                   fdrval dset+orig 1 1.2
                 and get a q-value of 0.234, then
                   fdrval -qinput dset+orig 1 0.234
                 should return the value 1.2 -- the original threshold.
                 (There may be a small discrepancy, due to the differences)
                 (between forward interpolation and inverse interpolation.)
               * To set a (csh) variable to use in a script for thresholding
                 via 3dcalc, you could do something like
                   set tval = `fdrval -qinput dset+orig 1 0.05`
                   3dcalc -expr "step(a-$tval)" -a dset+orig'[1]' -prefix dmask
    
    NOTES
    -----
    * Output for each 'val' is written to stdout.
    * If the q-value can't be computed, then 1.0 will be output.
    * If you input an absurdly high threshold, you will get the smallest
        q-value stored in the dataset header. (This is not necessarily exactly
        the smallest q-value that was computed originally, due to the way the
        FDR curves are calculated and interpolated.)
    * If you use '-qinput' and input a q-value that is too small for the
        FDR curve in the dataset header, you will get a threshold at or above
        the largest value in that sub-brick.
    * Sample usage:
          fdrval Fred_REML+orig 0 `count -scale 0.1 10 20` | 1dplot -stdin
        Uses the 'count' program to input a sequence of values, and then
        pipes into the 1dplot program to make a graph of F vs. q.
    * See the link below for information on how AFNI computes FDR curves:
          https://afni.nimh.nih.gov/pub/dist/doc/misc/FDR/FDR_Jan2008.pdf
    * Also see the output of '3dFDR -help'
    
    -- A quick hack by RWCox -- 15 Oct 2008 -- PG Wodehouse's birthday!
    -- Quick re-hack to add '-qinput' option -- 20 Dec 2011 -- RWCox
    -- Re-re-hack to make super-small '-qinput' values work right -- 14 Mar 2014
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
