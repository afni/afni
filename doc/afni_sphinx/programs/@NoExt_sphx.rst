.. _ahelp_@NoExt:

******
@NoExt
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: @NoExt <Name> <ext1> <ext2> .....
    example: @NoExt Hello.HEAD HEAD BRIK
    returns Hello
    @NoExt Hello.BRIK HEAD BRIK
    returns Hello
    @NoExt Hello.Jon HEAD BRIK
    returns Hello.Jon
    @NoExt Hello.JonA Jon nA
    returns Hello.Jo
    
    Ziad Saad (saadz@mail.nih.gov)
    LBC/NIMH/ National Institutes of Health, Bethesda Maryland
