********
3dLRflip
********

.. _3dLRflip:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dLRflip [-LR|-AP|-IS|-X|-Y|-Z] [-prefix ppp] dset dset dset ...
    Flips the rows of a dataset along one of the three axes.
    
    * This program is intended to be used in the case where you
      (or some other loser) constructed a dataset with one of the 
      directions incorrectly labeled. 
    * That is, it is to help you patch up a mistake in the dataset.
      It has no other purpose.
    
    Optional options:
    -----------------
    
     -LR | -AP | -IS: Axis about which to flip the data
                      Default is -LR.
          or
     -X | -Y | -Z: Flip about 1st, 2nd or 3rd directions,
                   respectively. 
     Note: Only one of these 6 options can be used at a time.
            
     -prefix ppp: Prefix to use for output. If you have 
                  multiple datasets as input, you are better
                  off letting the program choose a prefix for
                  each output.
    
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
