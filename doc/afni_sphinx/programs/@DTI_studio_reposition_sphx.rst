.. contents:: 
    :depth: 4 

**********************
@DTI_studio_reposition
**********************

.. code-block:: none

    @DTI_studio_reposition <DTI_Studio_volume> <AFNI_reference_volume>
    This script reslices and repositions a DTI Studio Analyze format
    volume to match an AFNI volume used as input data for DTI Studio.
    Check realignment with AFNI to be sure all went well.
    
    Example:
    Fibers.hdr is an Analyze volume from DTI Studio that contains
       fiber tract volume data. The Analyze format data will have two files -
       Fibers.hdr with the header data and Fibers.img with the data
       DTI Studio allows saving the fibers as volumes in the Fiber panel,
       disk icon in the lower right
    FA+orig is an AFNI volume to which to match the Analyze volume
    To create an AFNI brick version of Fibers that is in alignment
     with FA+orig (output is Fibers+orig):
    
    @DTI_studio_reposition Fibers.hdr FA+orig
