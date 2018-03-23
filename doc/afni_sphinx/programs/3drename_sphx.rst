.. _ahelp_3drename:

********
3drename
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage 1: 3drename old_prefix new_prefix
      Will rename all datasets using the old_prefix to use the new_prefix;
        3drename fred ethel
      will change fred+orig.HEAD    to ethel+orig.HEAD
                  fred+orig.BRIK    to ethel+orig.BRIK
                  fred+tlrc.HEAD    to ethel+tlrc.HEAD
                  fred+tlrc.BRIK.gz to ethel+tlrc.BRIK.gz
    
    Usage 2: 3drename old_prefix+view new_prefix
      Will rename only the dataset with the given view (orig, acpc, tlrc).
    
    You cannot have paths in the old or the new prefix
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
