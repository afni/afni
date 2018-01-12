.. contents:: 
    :depth: 4 

****************
@parse_afni_name
****************

.. code-block:: none

    Usage 1: A script to parse an AFNI name
    
       @parse_afni_name <name>
    
    Outputs the path, prefix, view and sub-brick selection string.
    
    If view is missing (nifti file), and sub-brick selection
    is used, view is set to '----'
