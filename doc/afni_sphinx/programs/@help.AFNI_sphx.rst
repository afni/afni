**********
@help.AFNI
**********

.. _@help.AFNI:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    @help.AFNI [OPTIONS]
       A script to retrieve and search AFNI's help page for all programs
    
     Examples:
       @help.AFNI 
    
       @help.AFNI -match love
    
     Options:
       -match 'word1 [word2 word3]' : Looks for occurence of each word in 
                                      the list in the help file.
                                      For a match with multiple words, all
                                      the words must be on the same line of
                                      text in the help file.
       -lynx :  Set viewer to lynx
       -vi   :  Set viewer to vi
       -less :  Set viewer to less (default)
       -nedit:  Set viewer to nedit
       -noview: Set viewer to no view
