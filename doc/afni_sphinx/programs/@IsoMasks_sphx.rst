.. contents:: 
    :depth: 4 

*********
@IsoMasks
*********

.. code-block:: none

    Parsing ...
    Usage: @IsoMasks -mask DSET -isovals v1 v1 ...
    Creates isosurfaces from isovolume envelopes.
    
    For example, to create contours of TLRC regions:
     @IsoMasks -mask ~/abin/TTatlas+tlrc'[0]' -isovals  `count -digits 1 1 77` 
