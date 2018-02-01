******
3dTcat
******

.. _3dTcat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Concatenate sub-bricks from input datasets into one big 3D+time dataset.
    Usage: 3dTcat options
    where the options are:
         -prefix pname = Use 'pname' for the output dataset prefix name.
     OR  -output pname     [default='tcat']
    
         -session dir  = Use 'dir' for the output dataset session directory.
                           [default='./'=current working directory]
         -glueto fname = Append bricks to the end of the 'fname' dataset.
                           This command is an alternative to the -prefix 
                           and -session commands.                        
         -dry          = Execute a 'dry run'; that is, only print out
                           what would be done.  This is useful when
                           combining sub-bricks from multiple inputs.
         -verb         = Print out some verbose output as the program
                           proceeds (-dry implies -verb).
                           Using -verb twice results in quite lengthy output.
         -rlt          = Remove linear trends in each voxel time series loaded
                           from each input dataset, SEPARATELY.  That is, the
                           data from each dataset is detrended separately.
                           At least 3 sub-bricks from a dataset must be input
                           for this option to apply.
                 Notes: (1) -rlt removes the least squares fit of 'a+b*t'
                              to each voxel time series; this means that
                              the mean is removed as well as the trend.
                              This effect makes it impractical to compute
                              the % Change using AFNI's internal FIM.
                        (2) To have the mean of each dataset time series added
                              back in, use this option in the form '-rlt+'.
                              In this case, only the slope 'b*t' is removed.
                        (3) To have the overall mean of all dataset time
                              series added back in, use this option in the
                              form '-rlt++'.  In this case, 'a+b*t' is removed
                              from each input dataset separately, and the
                              mean of all input datasets is added back in at
                              the end.  (This option will work properly only
                              if all input datasets use at least 3 sub-bricks!)
                        (4) -rlt can be used on datasets that contain shorts
                              or floats, but not on complex- or byte-valued
                              datasets.
         -relabel      = Replace any sub-brick labels in an input dataset
                           with the input dataset name -- this might help
                           identify the sub-bricks in the output dataset.
    
         -tpattern PATTERN = Specify the timing pattern for the output
                           dataset, using patterns described in the
                           'to3d -help' output (alt+z, seq, alt-z2, etc).
    
         -tr TR      = Specify the TR (in seconds) for the output dataset.
    
         -DAFNI_GLOB_SELECTORS=YES
                         Setting the environment variable AFNI_GLOB_SELECTORS
                         to YES (as done temporarily with this option) means
                         that sub-brick selectors '[..]' will not be used
                         as wildcards.  For example:
     3dTcat -DAFNI_GLOB_SELECTORS=YES -relabel -prefix EPIzero 'rest_*+tlrc.HEAD[0]'
                         will work to make a dataset with the #0 sub-brick
                         from each of a number of 3D+time datasets.
                      ** Note that the entire dataset specification is in quotes
                         to prevent the shell from doing the '*' wildcard expansion
                         -- it will be done inside the program itself, after the
                         sub-brick selector is temporarily detached from the string
                         -- and then a copy of the selector is re-attached to each
                         expanded filename.
                      ** Very few other AFNI '3d' programs do internal
                         wildcard expansion -- most of them rely on the shell.
    
    Command line arguments after the above are taken as input datasets.
    A dataset is specified using one of these forms:
       prefix+view
       prefix+view.HEAD
       prefix+view.BRIK
       prefix.nii
       prefix.nii.gz
    
    SUB-BRICK SELECTION:
    --------------------
    You can also add a sub-brick selection list after the end of the
    dataset name.  This allows only a subset of the sub-bricks to be
    included into the output (by default, all of the input dataset
    is copied into the output).  A sub-brick selection list looks like
    one of the following forms:
      fred+orig[5]                     ==> use only sub-brick #5
      fred+orig[5,9,17]                ==> use #5, #9, and #17
      fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8
      fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13
    Sub-brick indexes start at 0.  You can use the character '$'
    to indicate the last sub-brick in a dataset; for example, you
    can select every third sub-brick by using the selection list
      fred+orig[0..$(3)]
    You can reverse the order of sub-bricks with a list like
      fred+origh[$..0(-1)]
    (Exactly WHY you might want to time-reverse a dataset is a mystery.)
    
    You can also use a syntax based on the usage of the program count.
    This would be most useful when randomizing (shuffling) the order of
    the sub-bricks. Example:
      fred+orig[count -seed 2 5 11 s] is equivalent to something like:
      fred+orig[ 6, 5, 11, 10, 9, 8, 7] 
    You could also do: fred+orig[`count -seed 2 -digits 1 -suffix ',' 5 11 s`]
    but if you have lots of numbers, the command line would get too
    long for the shell to process it properly. Omit the seed option if
    you want the code to generate a seed automatically.
    You cannot mix and match count syntax with other selection gimmicks.
    
    If you have a lot of bricks to select in a particular order, you will
    also run into name length problems. One solution is to put the indices
    in a .1D file then use the following syntax. For example, say you have
    the selection in file reorder.1D. You can extract the sub-bricks with:
       fred+orig'[1dcat reorder.1D]' 
    As with count, you cannot mix and match 1dcat syntax with other 
    selection gimmicks.
    
    NOTES:
    ------
    You can also add a sub-brick selection list after the end of the
    * The TR and other time-axis properties are taken from the
      first input dataset that is itself 3D+time.  If no input
      datasets contain such information, then TR is set to 1.0.
      This can be altered later using the 3drefit program.
    
    * The sub-bricks are output in the order specified, which may
      not be the order in the original datasets.  For example, using
         fred+orig[0..$(2),1..$(2)]
      will cause the sub-bricks in fred+orig to be output into the
      new dataset in an interleaved fashion.  Using
         fred+orig[$..0]
      will reverse the order of the sub-bricks in the output.
      If the -rlt option is used, the sub-bricks selected from each
      input dataset will be re-ordered into the output dataset, and
      then this sequence will be detrended.
    
    * You can use the '3dinfo' program to see how many sub-bricks
      a 3D+time or a bucket dataset contains.
    
    * The '$', '(', ')', '[', and ']' characters are special to
      the shell, so you will have to escape them.  This is most easily
      done by putting the entire dataset plus selection list inside
      single quotes, as in 'fred+orig[5..7,9]'.
    
    * You may wish/need to use the 3drefit program on the output
      dataset to modify some of the .HEAD file parameters.
    
    * The program does internal wildcard expansion on the filenames
      provided to define the datasets.  The software first strips the
      sub-brick selector string '[...]' off the end of each filename
      BEFORE wildcard expansion, then re-appends it to the results
      AFTER the expansion; for example, '*+orig.HEAD[4..7]' might
      expand to 'fred+orig.HEAD[4..7]' and 'wilma+orig.HEAD[4..7]'.
     ++ However, the '[...]' construct is also a shell wildcard,
        It is not practicable to use this feature for filename
        selection with 3dTcat if you are also using sub-brick
        selectors.
     ++ Since wildcard expansion looks for whole filenames, you must
        use wildcard expansion in the form (e.g.) of '*+orig.HEAD',
        NOT '*+orig' -- since the latter form doesn't match filenames.
     ++ Don't use '*+orig.*' since that will match both the .BRIK and
        .HEAD files, and each dataset will end up being read in twice!
     ++ If you want to see the filename expansion results, run 3dTcat
        with the option '-DAFNI_GLOB_DEBUG=YES'
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
