.. contents:: 
    :depth: 4 

*******
1dMarry
*******

.. code-block:: none

    Usage: 1dMarry [options] file1 file2 ...
    
      Joins together 2 (or more) ragged-right .1D files, for use with
        3dDeconvolve -stim_times_AM2.
     **_OR_**
      Breaks up 1 married file into 2 (or more) single-valued files.
    
    OPTIONS:
    =======
     -sep abc  == Use the first character (e.g., 'a') as the separator
                  between values 1 and 2, the second character (e.g., 'b')
                  as the separator between values 2 and 3, etc.
                * These characters CANNOT be a blank, a tab, a digit,
                  or a non-printable control character!
                * Default separator string is '*,' which will result
                  in output similar to '3*4,5,6'
    
     -divorce  == Instead of marrying the files, assume that file1
                  is already a married file: split time*value*value... tuples
                  into separate files, and name them in the pattern
                  'file2_A.1D' 'file2_B.1D' et cetera.
    
    If not divorcing, the 'married' file is written to stdout, and
    probably should be captured using a redirection such as '>'.
    
    NOTES:
    =====
    * You cannot use column [...] or row {...} selectors on
        ragged-right .1D files, so don't even think about trying!
    * The maximum number of values that can be married is 26.
        (No polygamy or polyandry jokes here, please.)
    * For debugging purposes, with '-divorce', if 'file2' is '-',
        then all the divorcees are written directly to stdout.
    
    -- RWCox -- written hastily in March 2007 -- hope I don't repent
             -- modified to deal with multiple marriages -- December 2008
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
