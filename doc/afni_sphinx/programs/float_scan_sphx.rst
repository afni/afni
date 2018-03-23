.. _ahelp_float_scan:

**********
float_scan
**********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: float_scan [options] input_filename
    Scans the input file of IEEE floating point numbers for
    illegal values: infinities and not-a-number (NaN) values.
    
    Options:
      -fix     = Writes a copy of the input file to stdout (which
                   should be redirected using '>'), replacing
                   illegal values with 0.  If this option is not
                   used, the program just prints out a report.
      -v       = Verbose mode: print out index of each illegal value.
      -skip n  = Skip the first n floating point locations
                   (i.e., the first 4*n bytes) in the file
    
    N.B.: This program does NOT work on compressed files, nor does it
          work on byte-swapped files (e.g., files transferred between
          Sun/SGI/HP and Intel platforms), nor does it work on images
          stored in the 'flim' format!
    
    The program 'exit status' is 1 if any illegal values were
    found in the input file.  If no errors were found, then
    the exit status is 0. You can check the exit status by
    using the shell variable $status.  A C-shell example:
       float_scan fff
       if ( $status == 1 ) then
          float_scan -fix fff > Elvis.Aaron.Presley
          rm -f fff
          mv Elvis.Aaron.Presley fff
