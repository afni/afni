**********
@Purify_1D
**********

.. _@Purify_1D:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: @Purify_1D [<-sub SUB_STRING>] dset1 dset2 ...
    Purifies a series of 1D files for faster I/O into matlab.
      -sub SUB_STRING: You can use the sub-brick selection
                       mode, a la AFNI, to output a select
                       number of columns. See Example below.
      -suf STRING:     STRING is attached to the output prefix
                       which is formed from the input names
    
    Example:
        @Purify_1D -sub '[0,3]' somedataset.1D.dset
    
    Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov
