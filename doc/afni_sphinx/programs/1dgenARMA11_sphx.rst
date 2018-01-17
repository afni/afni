***********
1dgenARMA11
***********

.. _1dgenARMA11:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Program to generate an ARMA(1,1) time series, for simulation studies.
    Results are written to stdout.
    
    Usage: 1dgenARMA11 [options]
    
    Options:
    ========
     -num N  }  These equivalent options specify the length of the time
     -len N  }  series vector to generate.
    
     -nvec M  = The number of time series vectors to generate;
                if this option is not given, defaults to 1.
    
     -a a     = Specify ARMA(1,1) parameters 'a'.
     -b b     = Specify ARMA(1,1) parameter 'b' directly.
     -lam lam = Specify ARMA(1,1) parameter 'b' indirectly.
     -sig ss  = Set standard deviation of results [default=1].
     -norm    = Normalize time series so sum of squares is 1.
     -seed dd = Set random number seed.
    
      * The correlation coefficient r(k) of noise samples k units apart in time,
         for k >= 1, is given by r(k) = lam * a^(k-1)
         where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b)
         (N.B.: lam=a when b=0 -- AR(1) noise has r(k)=a^k for k >= 0)
         (N.B.: lam=b when a=0 -- MA(1) noise has r(k)=b for k=1, r(k)=0 for k>1)
      * lam can be bigger or smaller than a, depending on the sign of b:
         b > 0 means lam > a;  b < 0 means lam < a.
      * What I call (a,b) here is sometimes called (p,q) in the ARMA literature.
      * For a noise model which is the sum of AR(1) and white noise, 0 < lam < a
         (i.e., a > 0  and  -a < b < 0 ).
    
     -CORcut cc = The exact ARMA(1,1) correlation matrix (for a != 0)
                  has no non-zero entries.  The calculations in this
                  program set correlations below a cutoff to zero.
                  The default cutoff is 0.00010, but can be altered with
                  this option.  The usual reason to use this option is
                  to test the sensitivity of the results to the cutoff.
    
    Author: RWCox [for his own demented purposes]
    
    Examples:
      1dgenARMA11 -num 200 -a .8 -lam 0.7 | 1dplot -stdin
      1dgenARMA11 -num 2000 -a .8 -lam 0.7 | 1dfft -nodetrend stdin: stdout: | 1dplot -stdin
