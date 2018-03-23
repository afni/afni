.. _ahelp_3dConformist:

************
3dConformist
************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ** Program 3dConformist reads in a collection of datasets and
       zero pads them to the same size.
    ** The output volume size is the smallest region that includes
       all datasets (i.e., the minimal covering box).
    ** If the datasets cannot be processed (e.g., different grid
       spacings), then nothing will happen except for error messages.
    ** The purpose of this program is to be used in scripts that
       process lots of datasets and needs to make them all conform
       to the same size for collective voxel-wise analyses.
    ** The input datasets ARE ALTERED (embiggened)! <<<<<<------******
