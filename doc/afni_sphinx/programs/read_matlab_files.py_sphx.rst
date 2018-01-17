********************
read_matlab_files.py
********************

.. _read_matlab_files.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    =============================================================================
    read_matlab_files.py    - describe or convert MATLAB files (to 1D)
    
       Describe the contents of matlab files, and possibly convert them to 1D.
    
       Using only -infiles, all file objects (names not starting with '__') will
       be reported.  With the addition of -prefix, all numpy matrices will be
       converted to 1D format.
    
    ------------------------------------------
    examples:
    
       1. Describe the contents of all matlab files.
    
          read_matlab_files.py -infiles *.mat
    
       1. Convert all matlab files in the current directory to test.*.1D
    
          read_matlab_files.py -infiles *.mat -prefix test
    
    ------------------------------------------
    terminal options:
    
       -help                : show this help
       -hist                : show the revision history
       -ver                 : show the version number
    
    ------------------------------------------
    process options:
    
       -infiles             : specify input files
       -overwrite           : overwrite any output file
       -prefix PREFIX       : prefix for output file names
    
            Using -prefix, output files will have the naming format:
    
               PREFIX.INDEX.KEY.1D
    
                  PREFIX    : as specified with -prefix
                  INDEX     : 1-based index of objects found in file
                  KEY       : key (label) corresponding to the given object
    
    ------------------------------------------
    R Reynolds    January 2015
    =============================================================================
