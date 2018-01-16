*******
3dNotes
*******

.. _3dNotes:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Program: 3dNotes 
    Author:  T. Ross 
    (c)1999 Medical College of Wisconsin 
                                                                            
    3dNotes - a program to add, delete and show notes for AFNI datasets.    
     
    ----------------------------------------------------------------------- 
                                                                            
    Usage: 3dNotes [-a "string"] [-h "string"][-d num] [-help] dataset  
     
    Examples: 
     
    3dNotes -a      "Subject sneezed in scanner, Aug 13 2004" elvis+orig     
    3dNotes -h      "Subject likes fried PB & banana sandwiches" elvis+orig  
    3dNotes -HH     "Subject has left the building" elvis+orig              
    3dNotes -d 2 -h "Subject sick of PB'n'banana sandwiches" elvis+orig  
     
    ----------------------------------------------------------------------- 
                                                                            
    Explanation of Options:
    ---------------------- 
       dataset       : AFNI compatible dataset [required].
                                                                            
       -a   "str"  : Add the string "str" to the list of notes.
                                                                            
                       Note that you can use the standard C escape codes,
                       \n for newline \t for tab, etc.
                                                                            
       -h   "str"   : Append the string "str" to the dataset's history.  This
                        can only appear once on the command line.  As this is
                        added to the history, it cannot easily be deleted. But,
                        history is propagated to the children of this dataset.
                                                                            
       -HH  "str"   : Replace any existing history note with "str".  This 
                        line cannot be used with '-h'.
                                                                            
       -d   num       : deletes note number num.
                                                                            
       -ses           : Print to stdout the expanded notes.                 
                                                                            
       -help          : Displays this screen.
                                                                            
                                                                            
    The default action, with no options, is to display the notes for the
    dataset.  If there are options, all deletions occur first and essentially
    simultaneously.  Then, notes are added in the order listed on the command
    line.  If you do something like -d 10 -d 10, it will delete both notes 10
    and 11.  Don't do that.
    
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
