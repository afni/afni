*********
@TimeDiff
*********

.. _ahelp_@TimeDiff:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @TimeDiff <FILE1> <FILE2>
    
    Returns the difference in modification time A(.) 
     between FILE1 and FILE2
    
    If FILE2 was modified after FILE1 then A(FILE2) - A(FILE1) > 0
    
    Non existent files are considered more recent than existing ones.
