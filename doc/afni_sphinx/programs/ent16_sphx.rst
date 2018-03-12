*****
ent16
*****

.. _ent16:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: ent16 [-%nn]
    Computes an estimate of the entropy of stdin.
    If the flag '-%75' is given (e.g.), then the
      exit status is 1 only if the input could be
      compressed at least 75%, otherwise the exit
      status is 0.  Legal values of 'nn' are 1..99.
    In any case, the entropy and compression estimates
      are printed to stdout, even if no '-%nn' flag is.
      given.
    
    METHOD: entropy is estimated by building a histogram
            of all 16 bit words in the input, then summing
            over -p[i]*log2(p[i]), i=0..65535.  Compression
            estimate seems to work pretty good for gzip -1
            in most cases of binary image data.
    
    SAMPLE USAGE (csh syntax):
      ent16 -%75 < fred+orig.BRIK
