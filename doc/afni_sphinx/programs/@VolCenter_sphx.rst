**********
@VolCenter
**********

.. _@VolCenter:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @VolCenter <-dset DSET> [-or ORIENT]
    
       Returns the center of volume DSET
       The default coordinate system of the center
       is the same as that of DSET, unless another
       coordinate system is specified with the 
       -or option
    
    Example:
    @VolCenter -dset Vol+orig.BRIK -or RAI
     outputs the center of Vol+orig in RAI coordinate system
