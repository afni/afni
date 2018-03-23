.. _ahelp_3dClustCount:

************
3dClustCount
************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dClustCount [options] dataset1 ... 
    
    This program takes as input 1 or more datasets, thresholds them at various
    levels, and counts up the number of clusters of various sizes.  It is
    adapted from 3dClustSim, but only does the cluster counting functions --
    where the datasets come from is the user's business.  It is intended for
    use in a simulation script.
    
    -------
    OPTIONS
    -------
     -prefix sss = Use string 'sss' as the prefix of the filename into which
                   results will be summed.  The actual filename will be
                   'sss.clustcount.niml'.  If this file already exists, then
                   the results from the current run will be summed into the
                   existing results, and the file then re-written.
    
     -final      = If this option is given, then the results will be output
                   in a format like that used from 3dClustSim -- as 1D and
                   NIML formatted files with probabilities of various
                   cluster sizes.
                   ++ You can use '-final' without any input datasets if
                      you want to create the final output files from the
                      saved '.clustcount.niml' output file from earlier runs.
    
     -quiet      = Don't print out the progress reports, etc.
                   ++ Put this option first to quiet most informational messages.
    
    --------
    EXAMPLE:
    -------
    The steps here are
      (a) Create a set of 250 3dGroupInCorr results from a set of 190 subjects,
          using 250 randomly located seed locations.  Note the use of '-sendall'
          to get the individual subject results -- these are used in the next
          step, and are in sub-bricks 2..191 -- the collective 3dGroupInCorr
          results (in sub-bricks 0..1) are not actually used here.
      (b) For each of these 250 output datasets, create 80 random splittings
          into 2 groups of 95 subjects each, and carry out a 2-sample t-test
          between these groups.
          ++ Note the use of program 2perm to create the random splittings into
             files QQ_A and QQ_B, drawn from sub-bricks 2..191 of the ${fred}
             datasets.
          ++ Note the use of the '[1dcat filename]' construction to specify
             which sub-bricks of the ${fred} dataset are used for input to
             the '-setX' options of 3dttest++.
      (c) Count clusters from the '[1]' sub-brick of the 80 t-test outputs --
          the t-statistic sub-brick.
          ++  Note the use of a wildcard filename with a sub-brick selector:
              'QQ*.HEAD[1]' -- 3dClustCount will do the wildcard expansion
              internally, then add the sub-brick selector '[1]' to each expanded
              dataset filename.
      (d) Produce the final report files for empirical cluster-size thresholds
          for 3dGroupInCorr analyses -- rather than rely on 3dClustSim's assumption
          of Gaussian-shaped spatial correlation structure.
    The syntax is C-shell (tcsh), naturally.
    
        \rm -f ABscat*
        3dGroupInCorr -setA A.errts.grpincorr.niml                 \
                      -setB B.errts.grpincorr.niml                  \
                      -labelA A -labelB B -seedrad 5 -nosix -sendall \
                      -batchRAND 250 ABscat
        foreach fred ( ABscat*.HEAD )
          foreach nnn ( `count -dig 2 0 79` )
            2perm -prefix QQ 2 191
            3dttest++ -setA ${fred}'[1dcat QQ_A]' \
                      -setB ${fred}'[1dcat QQ_B]' \
                      -no1sam -prefix QQ${nnn}
          end
          3dClustCount -prefix ABcount 'QQ*.HEAD[1]'
          \rm -f QQ*
        end
        3dClustCount -final -prefix ABcount
        \rm -f ABscat*
    
    --------------------------------
    ---- RW Cox -- August 2012 -----
    --------------------------------
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
