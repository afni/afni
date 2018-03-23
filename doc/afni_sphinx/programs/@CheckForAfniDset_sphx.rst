.. _ahelp_@CheckForAfniDset:

*****************
@CheckForAfniDset
*****************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: @CheckForAfniDset <Name> .....
    example: @CheckForAfniDset /Data/stuff/Hello+orig.HEAD
    returns 0 if neither .HEAD nor .BRIK(.gz)(.bz2)(.Z) exist
              OR in the case of an error
                 An error also sets the status flag
            1 if only .HEAD exists
            2 if both .HEAD and .BRIK(.gz)(.bz2)(.Z) exist
            3 if .nii dataset 
    See also 3dinfo -exists
    Ziad Saad (saadz@mail.nih.gov)
      SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
