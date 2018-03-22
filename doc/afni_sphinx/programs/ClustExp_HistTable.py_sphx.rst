*********************
ClustExp_HistTable.py
*********************

.. _ahelp_ClustExp_HistTable.py:

.. contents:: 
    :depth: 4 

| 

    usage: /home/ptaylor/afni_src/linux_ubuntu_12_64/ClustExp_HistTable.py
           [-h] -StatDSET STATDSET [-prefix PREFIX] [-session SESSION]
           [-overwrite] [-help]
    

## Overview
===========

.. code-block:: none

    
    The purpose of this script is to extract the data table from history of
    datasets from 3dttest++, 3dMVM, or 3dLME. This program is mostly called from
    within ClustExp_StatParse.py
    

## Caveats
==========

.. code-block:: none

    Statistics dataset must be DIRECTLY from 3dttest++, 3dMVM or 3dLME.
    If you did 3dcopy or anything that wipes the history of the dataset after
    running the stats, this program has nothing to extract.
    3dttest++ must have been run with no covariates.
    
    

## Outputs
==========

.. code-block:: none

    
    Outputs files named with your -prefix and "_GroupTable.csv":
    (as example -prefix disco)
    
    disco_GroupTable.csv:
        Table with information parsed from the statistics dataset history.
        May include subject ID, any group or other variables, and input datasets.
    
    

## Options
==========

.. code-block:: none

    
    required:
      -StatDSET STATDSET  Statistics dataset.
    
    optional:
      -prefix PREFIX      Name for output (no path). [GroupOut]
      -session SESSION    Output parent folder if you don't want the current
                          working directory. [./]
      -overwrite          Remove previous folder with same PREFIX
    
    Justin Rajendra circa 08/2017
    I hope this will be useful for someone...
    Keep on keeping on!
