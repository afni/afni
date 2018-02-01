*****
1dcat
*****

.. _1dcat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dcat [options] a.1D b.1D ...
      where each file a.1D, b.1D, etc. is a 1D file.
      In the simplest form, a 1D file is an ASCII file of numbers
      arranged in rows and columns.
    
    1dcat takes as input one or more 1D files, and writes out a 1D file
    containing the side-by-side concatenation of all or a subset of the
    columns from the input files.
    
    * Output goes to stdout (the screen); redirect (e.g., '>') to save elsewhere.
    * All files MUST have the same number of rows!
    * Any header lines (i.e., lines that start with '#') will be lost.
    * For generic 1D file usage help and information, see '1dplot -help'
    
    OPTIONS:
    --------
      -nonconst: Columns that are identically constant should be omitted
                 from the output.
    
      -nonfixed: Keep only columns that are marked as 'free' in the 
                 3dAllineate header from '-1Dparam_save'.
                 If there is no such header, all columns are kept.
    
      -form FORM: Format of the numbers to be output.
                  You can also substitute -form FORM with shortcuts such 
                  as -i, -f, or -c.
                  For help on -form's usage, and its shortcut versions
                  see ccalc's help for the option of the same name. 
    
      -stack: Stack the columns of the resultant matrix in the output.
    
      -sel SEL: Apply the same column/row selection string to all filenames
                on the command line.
                For example:
                  1dcat -sel '[0,2]' f1.1D f2.1D
                is the same as: 1dcat f1.1D'[1,2]' f2.1D'[1,2]'
                The advantage of the option is that it allows wildcard use
                in file specification so that you can run something like:
                  1dcat -sel '[0,2]' f?.1D
    
    EXAMPLE:
    --------
      Input file 1:
       1
       2
       3
       4
      Input file 2:
       5
       6
       7
       8
    
      1dcat data1.1D data2.1D > catout.1D
      Output file: 
       1 5
       2 6
       3 7
       4 8
    
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
