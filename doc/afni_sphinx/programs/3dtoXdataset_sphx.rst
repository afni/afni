************
3dtoXdataset
************

.. _3dtoXdataset:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Convert input datasets to the format needed for 3dClustSimX.
    
    Usage:
    
     3dtoXdataset -prefix PPP maskdataset inputdataset ...
    
    The output file 'PPP.sdat' will be created, if it does not exist.
    If it already exists, the input dataset value (inside the mask) will
    be appended to this output file.
