**********
column_cat
**********

.. _ahelp_column_cat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
      column_cat : catenate files horizontally
    
      Output is sent to stdout, so redirection of output to
      a file may be desirable.
    
      Each line of output is the concatination of each current
      line from the input files, all on the same line, and
      separated by a space.  If different separation is desired,
      such as a tab, please use the -sep option.
    
      ** Note that using '-' or 'stdin' for an input file means
         to read from stdin.  One such stream is allowed.
    
      Optionos:
         -line LINE_NUM : print only line #LINE_NUM (1-based)
                          e.g. -line 1   (shows top line)
         -sep sep_str   : use sep_str as separation string
    
      Examples:
    
         column_cat -help
         column_cat file_a file_b
         column_cat file_a file_b file_c > output_file
         column_cat -line 17 file_a file_b file_c > output_file
         column_cat -sep : file_a file_b > output_file
         column_cat -sep '\t' file_a file_b > output_file
         column_cat -sep ' : ' file_a file_b > output_file
         cat file_a | column_cat -line 27 stdin
    
    R Reynolds    Jan, 2002 (distributed Aug, 2012)
