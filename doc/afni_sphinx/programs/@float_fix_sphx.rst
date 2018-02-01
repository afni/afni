**********
@float_fix
**********

.. _@float_fix:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @float_fix File1 File2 ...
    
       Check whether the input files have any IEEE floating
       point numbers for illegal values: infinities and
       not-a-number (NaN) values.
    
     NOTE: Wildcard can be used when specifying filenames. However
           the filenames have to end up with .HEAD. For example
           @float_fix Mozart*.HEAD
    
    Gang Chen (gangchen@mail.nih.gov) and Ziad Saad (saadz@nih.gov)
    SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
    01/24/2007
