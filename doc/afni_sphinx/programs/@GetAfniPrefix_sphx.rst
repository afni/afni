.. _ahelp_@GetAfniPrefix:

**************
@GetAfniPrefix
**************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: @GetAfniPrefix <Name> [Suffix]
    example: @GetAfniPrefix /Data/stuff/Hello+orig.HEAD
    returns the afni prefix of name (Hello)
    Wildcards are treated as regular characters:
    example: @GetAfniPrefix 'AAzst1r*+orig'
    returns : AAzst1r*
    
    If a Suffix string is specified, then it is
    appended to the returned prefix.
    
    Ziad Saad (saadz@mail.nih.gov)
      LBC/NIMH/ National Institutes of Health, Bethesda Maryland
