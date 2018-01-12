.. contents:: 
    :depth: 4 

***
@np
***

.. code-block:: none

    
    Usage: @np <pref>
    
     Finds an appropriate new prefix to use, given the files
     you already have in your directory. 
     Use this script to automatically create a valid prefix
     when you are repeatedly running similar commands but
     do not want to delete previous output.
    
     In addition to checking for valid AFNI prefix,
     the script will look for matching files with extensions:
        1D 1D.dset m nii asc ply 1D.coord 1D.topo coord topo srf 
    
     Script is slow, it is for lazy people.
