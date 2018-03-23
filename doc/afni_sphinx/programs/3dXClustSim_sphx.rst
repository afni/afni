.. _ahelp_3dXClustSim:

***********
3dXClustSim
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    This program takes as input random field simulations
    (e.g., from 3dttest++) and does the ETAC processing to
    find cluster figure of merit (FOM) thresholds that are
    equitable (AKA balanced) across
      * voxel-wise p-values (-pthr option)
      * blurring cases      (-ncase option)
      * H power values      (-hpow option) -- probably not useful
    as well as being balanced across space to produce
    a False Positive Rate (FPR) that is approximately the
    same for each location and for each sub-case listed
    above. The usual goal is a global FPR of 5%.
    
    * This program can be slow and consume a LOT of memory!
      (And I mean a BIG LOT, not a small lot.)
    
    * The output is a set of multi-threshold (*.mthresh.*.nii)
      files -- one for each of the -ncase inputs.
    
    * These files can be used via program 3dMultiThresh
      to produce an 'activation' mask.
    
    * 3dXClustSim is intended to be used from 3dttest++
      (via its '-ETAC' option) or some other script.
    
    * It is not intended to be run directly by any but the most
      knowledgeable and astute users. Which is why this help is so terse.
    
    --------
    OPTIONS:
    --------
    
     -inset       mask sdata ... {MANDATORY} [from 3dtoXdataset or 3dttest++]
     -insdat                     Data files are in the '.sdat' format.
    
     -NN          1 or 2 or 3    [-NN1 or -NN2 or -NN3 will work; default = 2]
     -sid         1 or 2         [-1sid or -2sid will work; default = 2]
     -hpow        0 1 2          [or some subset of these; default = 2]
    
     -ncase       N lab1 .. labN [multiple processing cases; e.g., blurs]
                                 [default = 1 A]
                                 [example = 4 b04 b06 b08 b10]
    
     -pthr        list of values [default = 0.0100 0.0056 0.0031 0.0018 0.0010]
                                 [equiv z1= 2.326  2.536  2.731  2.911  3.090 ]
                                 [equiv z2= 2.576  2.770  2.958  3.121  3.291 ]
    
     -FPR ff      set global FPR goal to ff%, where ff is an integer
                  from 2 to 9 (inclusive). Default value is 5.
    
     -multiFPR    compute results for multiple FPR goals (2%, 3%, ... 9%)
    
     -minclust M  don't allow clusters smaller than M voxels [default M=5]
    
     -prefix      something
     -verb        be more verbose
     -quiet       silentium est aureum
    
    **-----------------------------------------------------------
    ** Authored by Lamont Cranston, also known as ... The Shadow.
    **-----------------------------------------------------------
