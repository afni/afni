***
cdf
***

.. _cdf:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage 1: cdf [-v] -t2p statname t params
    Usage 2: cdf [-v] -p2t statname p params
    Usage 3: cdf [-v] -t2z statname t params
    
    This program does various conversions using the cumulative distribution
    function (cdf) of certain canonical probability functions.  The optional
    '-v' indicates to be verbose -- this is for debugging purposes, mostly.
    Use this option if you get results you don't understand!
    
    Usage 1: Converts a statistic 't' to a tail probability.
    Usage 2: Converts a tail probability 'p' to a statistic.
    Usage 3: Converts a statistic 't' to a N(0,1) value (or z-score)
             that has the same tail probability.
    
    The parameter 'statname' refers to the type of distribution to be used.
    The numbers in the params list are the auxiliary parameters for the
    particular distribution.  The following table shows the available
    distribution functions and their parameters:
    
       statname  Description  PARAMETERS
       --------  -----------  ----------------------------------------
           fico  Cor          SAMPLES  FIT-PARAMETERS  ORT-PARAMETERS
           fitt  Ttest        DEGREES-of-FREEDOM
           fift  Ftest        NUMERATOR and DENOMINATOR DEGREES-of-FREEDOM
           fizt  Ztest        N/A
           fict  ChiSq        DEGREES-of-FREEDOM
           fibt  Beta         A (numerator) and B (denominator)
           fibn  Binom        NUMBER-of-TRIALS and PROBABILITY-per-TRIAL
           figt  Gamma        SHAPE and SCALE
           fipt  Poisson      MEAN
    
    EXAMPLES:
     Goal:    find p-value for t-statistic of 5.5 with 30 degrees of freedom
     COMMAND: cdf -t2p fitt 5.5 30
     OUTPUT:  p = 5.67857e-06
    
     Goal:    find F(8,200) threshold that gives a p-value of 0.001
     COMMAND: cdf -p2t fift 0.001 8 200
     OUTPUT:  t = 3.4343
    
    The same functionality is also available in 3dcalc, 1deval, and
    ccalc, using functions such as 'fift_t2p(t,a,b)'.  In particular,
    if you are scripting, ccalc is probably better to use than cdf,
    since the output of
      ccalc -expr 'fitt_t2p(3,20)'
    is the string '0.007076', while the output of
      cdf -t2p fitt 3 20
    is the string 'p = 0.0070759'.
