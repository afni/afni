****
sfim
****

.. _ahelp_sfim:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    MCW SFIM: Stepwise Functional IMages, by RW Cox
    
    Usage: sfim [options] image_files ...
    
      + image_files are in the same format AFNI accepts
      + options are from the following:
    
      -sfint iname:   'iname' is the name of a file which has
                      the interval definitions; an example is
                        3*# 5*rest 4*A 5*rest 4*B 5*rest 4*A 5*rest
                      which says:
                        - ignore the 1st 3 images
                        - take the next 5 as being in task state 'rest'
                        - take the next 4 as being in task state 'A'
                        and so on;
                      task names that start with a nonalphabetic character
                      are like the '#' above and mean 'ignore'.
                  *** the default 'iname' is 'sfint'
    
      -base bname:    'bname' is the task state name to use as the
                      baseline; other task states will have the mean
                      baseline state subtracted; if there are no task
                      states from 'iname' that match 'bname', this
                      subtraction will not occur.
                  *** the default 'bname' is 'rest'
    
      -localbase:     if this option is present, then each non-base
                      task state interval has the mean of the two
                      nearest base intervals subtracted instead of the
                      grand mean of all the base task intervals.
    
      -prefix pname:  'pname' is the prefix for output image filenames for
                      all states:  the i'th interval with task state name
                      'fred' will be writen to file 'pname.fred.i'.
                  *** the default 'pname' is 'sfim'
    
      Output files are the base-mean-removed averages for each non-base
      task interval, and simply the mean for each base task interval.
      Output images are in the 'flim' (floating pt. image) format, and
