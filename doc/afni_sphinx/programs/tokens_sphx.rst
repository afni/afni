.. _ahelp_tokens:

******
tokens
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    usage: tokens [-infile INFILE] [-extra C] [...]
    
       -infile : specify input file (stdin if none)
       -extra  : specify extra character to count as valid
                 - can use this more than once
                 - I do not remember why I added this
    ------------------------------
    examples:
    
       tokens -infile script.txt
       tokens -infile script.txt | grep -i anat
    ------------------------------
    R. Reynolds, circa 1994
    version 1.1, 1 Mar 2016
