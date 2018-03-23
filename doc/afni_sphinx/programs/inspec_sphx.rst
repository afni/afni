.. _ahelp_inspec:

******
inspec
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: inspec <-spec specfile> 
                  [-detail d] [-prefix newspecname] 
                  [-LRmerge leftspec rightspec]
                  [-h/-help]
    Outputs information found from specfile.
        -spec specfile: specfile to be read
        -prefix newspecname: rewrite spec file.
        -detail d: level of output detail default is 1 in general,
                   0 with -LRmerge.  
                   Available levels are 0, 1, 2 and 3.
        -LRmerge LeftSpec RightSpec:
                 Merge two spec files in a way that makes
                 sense for viewing in SUMA
        -remove_state STATE_RM:
                 Get rid of state STATE_RM from the specfile
        -h or -help: This message here.
    
    Compile Date:
       Mar 22 2018
    
          Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov 
         Dec 2 03
