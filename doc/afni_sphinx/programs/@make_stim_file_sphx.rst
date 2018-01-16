***************
@make_stim_file
***************

.. _@make_stim_file:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    @make_stim_file - create a time series file, suitable for 3dDeconvolve
    
        This script reads in column headers and stimulus times for
        each header (integers), and computes a 'binary' file (all
        0s and 1s) with column headers, suitable for use as input to
        3dDeconvolve.
    
        The user must specify an output file on the command line (using
        -outfile), and may specify a maximum repetition number for rows
        of output (using -maxreps).
    ------------------------------
      Usage: @make_stim_file [options] -outfile OUTFILE
    
      examples:
    
        @make_stim_file -outfile green_n_gold
        @make_stim_file -outfile green_n_gold < my_input_file
        @make_stim_file -maxreps 200 -outfile green_n_gold -headers
        @make_stim_file -help
        @make_stim_file -maxreps 200 -outfile green_n_gold -debug 1
    ------------------------------
      options:
    
        -help            : show this help information
    
        -debug LEVEL     : print debug information along the way
              e.g. -debug 1
              the default is 0, max is 2
    
        -outfile OUTFILE : (required) results are sent to this output file
              e.g. -outfile green.n.gold.out
    
        -maxreps REPS    : use REPS as the maximum repeptition time
              e.g. -maxreps 200
              the default is to use the maximum rep time from the input
    
              This option basically pads the output columns with 0s,
              so that each column has REPS rows (of 1s and 0s).
    
        -no_headers      : do not include headers in output file
              e.g. -no_headers
              the default is print column headers (# commented out)
    
        -zero_based      : consider stim times as zero-based numbers
              e.g. -zero_based
              the default is 1-based (probably a bad choice...)
    
    
    ------------------------------
      Notes:
    
        1. It is probably easiest to use redirection from an input file
           for execution of the program.  That way, mistakes can be more
           easily fixed and retried.  See 'Sample execution 2'.
    
        2. Since most people start off with stimulus data in colums, and
           since this program requires input in rows for each header, it
           may be easiest to go through a few initial steps:
               - make sure all data is in integer form
               - make sure all blank spaces are filled with 0
               - save the file to an ascii data file (without headers)
               - use AFNI program '1dtranspose' to convert column data
                 to row format
               - add the column headers back to the top of the ascii file
    
        3. The -maxreps option is recommended when using redirection, so
           that the user does not have to add the value to the bottom of
           the file.
    ------------------------------
      Sample execution 1: (typing input on command line)
    
        a. executing the following command:
    
           @make_stim_file -outfile red_blue_out
    
        b. and providing input data as follows:
    
           headers -> red blue
           'red' -> 2 4
           'blue' -> 2 3 5
           maxreps -> 6
    
        c. will produce 'red_blue_out', containing:
    
           red blue
           0 0
           1 1
           0 1
           1 0
           0 1
           0 0
    ------------------------------
      Sample execution 2: (using redirection)
    
        a. given input file 'my_input_file': (a text file with input data)
    
           red blue
           2 4
           2 3 5
           6
    
        b. run the script using redirection with -maxreps option
    
          @make_stim_file -maxreps 6 -outfile red_blue_out < my_input_file
    
        c. now there exists output file 'red_blue_out':
    
           red blue
           0 0
           1 1
           0 1
           1 0
           0 1
           0 0
    ------------------------------
      R. Reynolds
