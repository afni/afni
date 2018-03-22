************
@toMNI_Awarp
************

.. _ahelp_@toMNI_Awarp:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

     
    Script to take a collection of datasets and transform them
    to 1x1x1 mm MNI space with an affine transformation.
    These datasets should already have been skull-stripped.
     
    Usage: @toMNI_Awarp dirname dataset1 dataset2 ...
     
    where 'dirname' is the name of the directory which will be created and
    then get the results, and 'dataset1 dataset2 ...' is a list of datasets
    to be transformed.
     
    The results can further be nonlinearly registered to form a template
