.. contents:: 
    :depth: 4 

*************
3dGroupInCorr
*************

.. code-block:: none

    Usage: 3dGroupInCorr [options]
    
    * Also see
      https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/instastuff.pdf
    
    * This program operates as a server for AFNI or SUMA.  It reads in dataset
      collections that have been prepared by 3dSetupGroupInCorr, and then
      connects to the AFNI or SUMA GUI program (via TCP/IP).  Then it waits
      for a command to be sent from AFNI/SUMA before it actually does anything.
    
    * The command from AFNI is sent when the user (you) clicks the 'InstaCorr Set' *
    * button in the [A] controller image viewer right-mouse-click popup menu; or,  *
    * when you hold down the Shift and Control (Ctrl) keys on the keyboard at the  *
    * same time you left-mouse-click in the image viewer.                          *
    
      (-: However,  the new [Feb 2011] '-batch' option,  described far below, :-)
      (-: lets you run 3dGroupInCorr by itself, without AFNI or SUMA, writing :-)
      (-: results to disk instead of transmitting them to the client program. :-)
    
    * At the same time as you run 3dGroupInCorr, you also have to run the
      AFNI GUI program, with a command like 'afni -niml'.  3dGroupInCorr
      by itself will only do something when AFNI sends it a command, which
      you do by using the 'InstaCorr Set' button on the [A] image viewer
      right-click popup menu, after 3dGroupInCorr has connected to AFNI.
    
    * When AFNI sends a seed voxel command, 3dGroupInCorr will extract
      that voxel times series from each input dataset, will compute the
      correlation map of each dataset with the corresponding seed time
      series, then will compute the voxel-wise collection of t-tests of
      that bunch of correlation maps, and return the resulting 3D volumes
      to AFNI for display.
     ++ A lot of computing can be required if there are a lot of datasets
        in the input collections.  3dGroupInCorr is carefully written to
        be fast.  For example, on a Mac Pro with 8 3GHz CPUs, running
        with 1.2 GBytes of data (100 datasets each with 69K voxels), each
        group correlation map takes about 0.3 seconds to calculate and
        transmit to AFNI -- this speed is why it's called 'Insta'.
    
    * You must start AFNI with the '-niml' option to allow it to accept
      incoming TCP/IP socket connections.
     ++ Or you can press the 'NIML+PO' button in the GUI, if you forgot
        to type the AFNI command line correctly.
     ++ If you are running 3dGroupInCorr and AFNI on separate computers,
        you also have to setup 'host trusting' correctly -- for details,
        see the description of the '-ah' option, far below.
    
    * In the AFNI 'A' controller, once 3dGroupInCorr is connected to AFNI,
      you don't have to switch to 'GrpInCorr' on the 'InstaCorr' menu to
      use the 'InstaCorr Set' controls -- unlike the individual subject
      InstaCorr, which requires setup inside AFNI.  For Group InstaCorr,
      the setup is already done in 3dSetupGroupInCorr.  The ONLY reason
      for using the 'GrpInCorr' setup controls in AFNI is to change the
      value of the '-seedrad' option' radius interactively.
    
    * More detailed outline of processing in 3dGroupInCorr:
     ++ For each 3D+time dataset in the input dataset collections:
       -- Extract the seed voxel time series (averaging locally per 'seedrad')
            [you could do this manually with 3dmaskave]
       -- Correlate it with all other voxel time series in the same dataset
            [you could do this manually with 3dDeconvolve or 3dfim]
       -- Result is one 3D correlation map per input dataset
       -- The standard processing uses Pearson correlation between time series
            vectors.  You can also pre-process the data to use Spearman (rank)
            correlation instead.  This alteration must be done in program
            3dSetupGroupInCorr, or with program 3dTransformGroupInCorr.
     ++ Then carry out the t-test between/among these 3D correlation maps,
          possibly allowing for dataset-level covariates.
       -- Actually, between the arctanh() of these maps:
           cf. RA Fisher: http://en.wikipedia.org/Fisher_transformation
           [you could do the arctanh() conversion manually via 3dcalc;]
           [then do the t-tests manually with 3dttest++;  then convert]
           [the t-statistics to Z-scores using yet another 3dcalc run.]
       -- To be overly precise, if the correlation is larger than 0.999329,
           then the arctanh is clipped to 4.0, to avoid singularities.
           If you consider this clipping to be a problem, please go away.
     ++ The dataset returned to AFNI converts the t-statistic maps
        to Z-scores, for various reasons of convenience.
       -- Conversion is done via the same mechanism used in program
            cdf -t2z fitt TSTAT DOF
       -- The individual correlation maps that were t-test-ed are discarded.
       -- Unless you use the new [Jan 2011] '-sendall' option :-)
    
    * When 3dGroupInCorr starts up, it has to 'page fault' all the data
      into memory.  This can take several minutes, if it is reading (say)
      10 Gbytes of data from a slow disk.  After that, if your computer
      has enough RAM, then the program should run pretty quickly.
     ++ If your computer DOESN'T have enough RAM to hold all the data,
        then this program will be painfully slow -- buy more memory!
     ++ Note that the .data file(s) are mapped directly into memory (mmap),
        rather than being read with standard file input methods (read function).
     ++ This memory-mapping operation may not work well on network-mounted
        drives, in which case you will have to run 3dGroupInCorr on the same
        computer with the data files [Feb 2016 -- but see the new '-read' option].
     ++ However, 3dGroupInCorr does NOT need to be run on the same computer
        as AFNI or SUMA: see the '-ah' option (described far below).
    
    * Once 3dGroupInCorr is connected to AFNI, you can 'drive' the selection
      of seed points via the AFNI driver commands (e.g., via the plugout_drive
      program).  For details, see the README.driver document.
    
    * One reason this program is a server (rather than being built in
      to AFNI) is that it is compiled to use OpenMP, which will let
      it make use of multiple CPU cores on the computer system :-)
     ++ For more information, see the very end of this '-help' output.
    
    * If you have only the .niml and .data files, and not original datasets,
      you can partially reconstruct the datasets by using the program
      3dExtractGroupInCorr.
    
    ===================================================================
                           COMMAND LINE OPTIONS
    [Most options are not case sensitive -- e.g., '-apair' == '-Apair']
    ===================================================================
    
    -----------------------*** Input Files ***-------------------------
    
     -setA AAA.grpincorr.niml
       = Give the setup file (from 3dSetupGroupInCorr) that describes
         the first dataset collection:
      ++ This 'option' is MANDATORY (you have to input SOMETHING).
      ++ Of course, 'AAA' should be replaced with the correct name of
         your input dataset collection file!
      ++ 3dGroupInCorr can use byte-valued or short-valued data as
         produced by the '-byte' or '-short' options to 3dSetupGroupInCorr.
      ++ You can also put the '.data' filename here, or leave off the '.niml';
         the program will look for these cases and patch the filename as needed.
    
     -setB BBB.grpincorr.niml
       = Give the setup file that describes the second dataset collection:
      ++ This option IS optional.
      ++ If you use only -setA, then the program computes a one-sample t-test.
      ++ If you use also -setB, then the program computes a two-sample t-test.
        -- The exact form of the 2-sample t-test used is controlled by one of the
           three options described below (which are mutually exclusive).
      ++ The sign of a two sample t-test is 'A-B'; that is, a positive result
         means that the A set of correlations average larger than the B set.
      ++ The output t-statistics are converted to Z-scores for transmission to AFNI,
         using the same code as the 'fitt_t2z(t,d)' function in 3dcalc:
        -- e.g, the output of the command
              ccalc 'fitt_t2z(4,15)'
           is 3.248705, showing that a t-statistic of 4 with 15 degrees-of-freedom
           (DOF) has the same p-value as a Z-score [N(0,1) deviate] of 3.248705.
        -- One reason for using Z-scores is that the DOF parameter varies between
           voxels when you choose the -unpooled option for a 2-sample t-test.
    
     -Apair = Instead of using '-setB', this option tells the program to use
              the '-setA' collection in its place; however, the seed location
              for this second copy of setA is a different voxel/node.  The result
              is to contrast (via a paired t-test) the correlation maps from the
              different seeds.
             ++ For Alex Martin and his horde of myrmidons.
           -->> You cannot use '-Apair' with '-setB' or with '-batch'.
             ++ To use this in the AFNI GUI, you first have to set the Apair seed
                using the 'GIC: Apair Set' button on the image viewer right-click
                popup menu.  After that, the standard 'InstaCorr Set' button will
                pick the new seed to contrast with the Apair seed.
             ++ Or you can select 'GIC: Apair MirrorOFF' to switch it to 'MirrorON*'.
                In that case, selecting 'InstaCorr Set' will automatically also set
                the Apair seed to the left-right mirror image location (+x -> -x).
             ++ The resulting correlation maps will have a positive (red) hotspot
                near the InstaCorr seed and a negative (blue) hotspot near the
                Apair seed.  If you don't understand why, then your understanding
                of resting state FMRI correlation analyses needs some work.
           -->> It is regions AWAY from the positive and negative seeds that are
                potentially interesting -- significant results at region Q indicate
                a difference in 'connectivity' between Q and the two seeds.
             ++ In the case of mirroring, Q is asymmetrically 'connected' to one
                side of brain vs. the other; e.g., I've found that the left Broca's
                area (BA 45) makes a good seed -- much of the left temporal lobe is
                asymmetrically connected with respect to this seed and its mirror,
                but not so much of the right temporal lobe.
    
     -labelA aaa = Label to attach (in AFNI) to sub-bricks corresponding to setA.
                   If you don't give this option, the label used will be the prefix
                   from the -setA filename.
    
     -labelB bbb = Label to attach (in AFNI) to sub-bricks corresponding to setB.
                  ++ At most the first 11 characters of each label will be used!
                  ++ In the case of '-Apair', you can still use '-labelB' to indicate
                     the label for the negative (Apair) seed; otherwise, the -setA
                     filename will be used with 'AP:' prepended.
    
    -----------------------*** Two-Sample Options ***-----------------------
    
     -pooled   = For a two-sample un-paired t-test, use a pooled variance estimator
     -unpooled = For a two-sample un-paired t-test, use an unpooled variance estimator
                ++ Statistical power declines a little, and in return,
                   the test becomes a little more robust.
     -paired   = Use a two-sample paired t-test
                ++ Which is the same as subtracting the two sets of 3D correlation
                   maps, then doing a one-sample t-test.
                ++ To use '-paired', the number of datasets in each collection
                   must be the same, and the datasets must have been input to
                   3dSetupGroupInCorr in the same relative order when each
                   collection was created. (Duh.)
                ++ '-paired' is automatically turned on when '-Apair' is used.
     -nosix    = For a 2-sample situation, the program by default computes
                 not only the t-test for the difference between the samples,
                 but also the individual (setA and setB) 1-sample t-tests, giving
                 6 sub-bricks that are sent to AFNI.  If you don't want
                 these 4 extra 1-sample sub-bricks, use the '-nosix' option.
                ++ See the Covariates discussion, below, for an example of how
                   '-nosix' affects which covariate sub-bricks are computed.
                ++ In the case of '-Apair', you may want to keep these extra
                   sub-bricks so you can see the separate maps from the positive
                   and negative seeds, to make sure your results make sense.
    
     **-->> None of these 'two-sample' options means anything for a 1-sample
            t-test (i.e., where you don't use -setB or -Apair).
    
    -----------------*** Dataset-Level Covariates [May 2010] ***-----------------
    
     -covariates cf = Read file 'cf' that contains covariates values for each dataset
                      input (in both -setA and -setB; there can only at most one
                      -covariates option).  Format of the file
         FIRST LINE  -->  subject IQ   age
         LATER LINES -->  Elvis   143   42
                          Fred     85   59
                          Ethel   109   49
                          Lucy    133   32
            This file format should be compatible with 3dMEMA.
    
            ++ The first column contains the labels that must match the dataset
                labels stored in the input *.grpincorr.niml files, which are
                either the dataset prefixes or whatever you supplied in the
                3dSetupGroupInCorr program via '-labels'.
                -- If you ran 3dSetupGroupInCorr before this update, its output
                   .grpincorr.niml file will NOT have dataset labels included.
                   Such a file cannot be used with -covariates -- Sorry.
    
            ++ The later columns contain numbers: the covariate values for each
                input dataset.
                -- 3dGroupInCorr does not allow voxel-level covariates.  If you
                   need these, you will have to use 3dttest++ on the '-sendall'
                   output (of individual dataset correlations), which might best
                   be done using '-batch' mode (cf. far below).
    
            ++ The first line contains column headers.  The header label for the
                first column isn't used for anything.  The later header labels are
                used in the sub-brick labels sent to AFNI.
    
            ++ If you want to omit some columns in file 'cf' from the analysis,
                you can do so with the standard AFNI column selector '[...]'.
                However, you MUST include column #0 first (the dataset labels) and
                at least one more numeric column.  For example:
                  -covariates Cov.table'[0,2..4]'
                to skip column #1 but keep columns #2, #3, and #4.
    
            ++ At this time, only the -paired and -pooled options can be used with
                covariates.  If you use -unpooled, it will be changed to -pooled.
                -unpooled still works with a pure t-test (no -covariates option).
                -- This restriction might be lifted in the future.  Or it mightn't.
    
            ++ If you use -paired, then the covariates for -setB will be the same
                as those for -setA, even if the dataset labels are different!
                -- This also applies to the '-Apair' case, of course.
    
            ++ By default, each covariate column in the regression matrix will have
                its mean removed (centered). If there are 2 sets of subjects, each
                set's matrix will be centered separately.
                -- See the '-center' option (below) to alter this default.
    
            ++ For each covariate, 2 sub-bricks are produced:
                -- The estimated slope of arctanh(correlation) vs covariate
                -- The Z-score of the t-statistic of this slope
    
            ++ If there are 2 sets of subjects, then each pair of sub-bricks is
                produced for the setA-setB, setA, and setB cases, so that you'll
                get 6 sub-bricks per covariate (plus 6 more for the mean, which
                is treated as a special covariate whose values are all 1).
                -- At present, there is no way to tell 3dGroupInCorr not to send
                   all this information back to AFNI/SUMA.
    
            ++ The '-donocov' option, described later, lets you get the results
                calculated without covariates in addition to the results with
                covariate regression included, for comparison fun.
                -- Thus adding to the number of output bricks, of course.
    
            ++ EXAMPLE:
               If there are 2 groups of datasets (with setA labeled 'Pat', and setB
               labeled 'Ctr'), and one covariate (labeled IQ), then the following
               sub-bricks will be produced:
    
           # 0: Pat-Ctr_mean    = mean difference in arctanh(correlation)
           # 1: Pat-Ctr_Zscr    = Z score of t-statistic for above difference
           # 2: Pat-Ctr_IQ      = difference in slope of arctanh(correlation) vs IQ
           # 3: Pat-Ctr_IQ_Zscr = Z score of t-statistic for above difference
           # 4: Pat_mean        = mean of arctanh(correlation) for setA
           # 5: Pat_Zscr        = Z score of t-statistic for above mean
           # 6: Pat_IQ          = slope of arctanh(correlation) vs IQ for setA
           # 7: Pat_IQ_Zscr     = Z score of t-statistic for above slope
           # 8: Ctr_mean        = mean of arctanh(correlation) for setB
           # 9: Ctr_Zscr        = Z score of t-statistic for above mean
           #10: Ctr_IQ          = slope of arctanh(correlation) vs IQ for setB
           #11: Ctr_IQ_Zscr     = Z score of t-statistic for above slope
    
            ++ However, the single-set results (sub-bricks #4-11) will NOT be
               computed if the '-nosix' option is used.
    
            ++ If '-sendall' is used, the individual dataset arctanh(correlation)
               maps (labeled with '_zcorr' at the end) will be appended to this
               list.  These setA sub-brick labels will start with 'A_' and these
               setB labels with 'B_'.
    
            ++ If you are having trouble getting the program to read your covariates
               table file, then set the environment variable AFNI_DEBUG_TABLE to YES
               and run the program -- the messages may help figure out the problem.
               For example:
                 3dGroupInCorr -DAFNI_DEBUG_TABLE=YES -covariates cfile.txt |& more
    
      -->>**++ A maximum of 31 covariates are allowed.  If you need more, then please
               consider the possibility that you are completely deranged or demented.
    
     *** CENTERING ***
     Covariates are processed using linear regression.  There is one column in the
     regression matrix for each covariate, plus a column of all 1s for the mean
     value.  'Centering' refers to the process of subtracting some value from each
     number in a covariate's column, so that the fitted model for the covariate's
     effect on the data is zero at this subtracted value; the model (1 covariate) is:
       data[i] = mean + slope * ( covariate[i] - value )
     where i is the dataset index.  The standard (default) operation is that 'value'
     is the mean of the covariate[i] numbers.
    
     -center NONE = Do not remove the mean of any covariate.
    
     -center DIFF = Each set will have the means removed separately [default].
    
     -center SAME = The means across both sets will be computed and subtracted.
                   * This option only applies to a 2-sample unpaired test.
                   * You can attach '_MEDIAN' after 'DIFF' or 'SAME' to have the
                     centering be done at the median of covariate values, rather
                     than the mean, as in 'DIFF_MEDIAN' or 'SAME_MEDIAN'.
                     (Why you would do this is up to you, as always.)
    
     -center VALS A.1D [B.1D]
                    This option (for Gang Chen) allows you to specify the
                    values that will be subtracted from each covariate before
                    the regression analysis.  If you use this option, then
                    you must supply a 1D file that gives the values to be
                    subtracted from the covariates; if there are 3 covariates,
                    then the 1D file for the setA datasets should have 3 numbers,
                    and the 1D file for the setB datasets (if present) should
                    also have 3 numbers.
                  * For example, to put these values directly on the command line,
                    you could do something like this:
                      -center VALS '1D: 3 7 9' '1D: 3.14159 2.71828 0.91597'
                  * As a special case, if you want the same values used for
                    the B.1D file as in the A.1D file, you can use the word
                    'DITTO' in place of repeating the A.1D filename.
                  * Of course, you only have to give the B.1D filename if there
                    is a setB collection of datasets, and you are not doing a
                    paired t-test.
    
     Please see the discussion of CENTERING in the 3dttest++ help output.  If
     you change away from the default 'DIFF', you should really understand what
     you are doing, or an elephant may sit on your head, which no one wants.
    
    ---------------------------*** Other Options ***---------------------------
    
     -seedrad r = Before performing the correlations, average the seed voxel time
                  series for a radius of 'r' millimeters.  This is in addition
                  to any blurring done prior to 3dSetupGroupInCorr.  The default
                  radius is 0, but the AFNI user can change this interactively.
    
     -sendall   = Send all individual subject results to AFNI, as well as the
                  various group statistics.
                 ++ These extra sub-bricks will be labeled like 'xxx_zcorr', where
                    'xxx' indicates which dataset the results came from; 'zcorr'
                    denotes that the values are the arctanh of the correlations.
                 ++ If there are a lot of datasets, then the results will be VERY
                    large and take up a lot of memory in AFNI.
               **++ Use this option with some judgment and wisdom, or bad things
                    might happen! (e.g., your computer runs out of memory.)
                 ++ This option is also known as the 'Tim Ellmore special'.
    
     -donocov   = If covariates are used, this option tells 3dGroupInCorr to also
                  compute the results without using covariates, and attach those
                  to the output dataset -- presumably to facilitate comparison.
                 ++ These extra output sub-bricks have 'NC' attached to their labels.
                 ++ If covariates are NOT used, this option has no effect at all.
    
     -dospcov   = If covariates are used, compute the Spearman (rank) correlation
                  coefficient of the subject correlation results vs. each covariate.
                 ++ These extra sub-bricks are in addition to the standard
                    regression analysis with covariates, and are added here at
                    the request of the IMoM (PK).
                 ++ These sub-bricks will be labeled as 'lll_ccc_SP', where
                      'lll' is the group label (from -labelA or -labelB)
                      'ccc' is the covariate label (from the -covariates file)
                      '_SP' is the signal that this is a Spearman correlation
                 ++ There will be one sub-brick produced for each covariate,
                    for each group (1 or 2 groups).
    
     -clust PP  = This option lets you input the results from a 3dClustSim run,
                  to be transmitted to AFNI to aid with the interactive Clusterize.
                  3dGroupInCorr will look for files named
                    PP.NN1_1sided.niml  PP.NN1_2sided.niml  PP.NN1_bisided.niml
                    (and similarly for NN2 and NN3 clustering), plus PP.mask
                  and if at least one of these .niml files is found, will send
                  it to AFNI to be incorporated into the dataset.  For example,
                  if the datasets' average smoothness is 8 mm, you could do
                    3dClustSim -fwhm 8 -mask Amask+orig -niml -prefix Gclus
                    3dGroupInCorr ... -clust Gclus
             -->> Presumably the mask would be the same as used when you ran
                  3dSetupGroupInCorr, and the smoothness you would have estimated
                  via 3dFWHMx, via sacred divination, or via random guesswork.
                  It is your responsibility to make sure that the 3dClustSim files
                  correspond properly to the 3dGroupInCorr setup!
             -->>++ This option only applies to AFNI usage, not to SUMA.
                 ++ See the Clusterize notes, far below, for more information on
                    using the interactive clustering GUI in AFNI with 3dGroupInCorr.
    
     -read    = Normally, the '.data' files are 'memory mapped' rather than read
                into memory.  However, if your files are on a remotely mounted
                server (e.g., a remote RAID), then memory mapping may not work.
                Or worse, it may seem to work, but return 'data' that is all zero.
                Use this '-read' option to force the program to read the data into
                allocated memory.
               ++ Using read-only memory mapping is a way to avoid over-filling
                  the system's swap file, when the .data files are huge.
               ++ You must give '-read' BEFORE '-setA' or '-setB', so that the
                  program knows what to do when it reaches those options!
    
     -ztest   = Test the input to see if it is all zero.  This option is for
                debugging, not for general use all the time.
    
     -ah host = Connect to AFNI/SUMA on the computer named 'host', rather than
                on the current computer system 'localhost'.
         ++ This allows 3dGroupInCorr to run on a separate system than
            the AFNI GUI.
           -- e.g., If your desktop is weak and pitiful, but you have access
              to a strong and muscular multi-CPU server (and the network
              connection is fast).
         ++ Note that AFNI must be setup with the appropriate
            'AFNI_TRUSTHOST_xx' environment variable, so that it will
            allow the external socket connection (for the sake of security):
          -- Example: AFNI running on computer 137.168.0.3 and 3dGroupInCorr
             running on computer 137.168.0.7
          -- Start AFNI with a command like
               afni -DAFNI_TRUSTHOST_01=137.168.0.7 -niml ...
          -- Start 3dGroupInCorr with a command like
               3dGroupInCorr -ah 137.168.0.3 ...
          -- You may use hostnames in place of IP addresses, but numerical
             IP addresses will work more reliably.
          -- If you are very trusting, you can set NIML_COMPLETE_TRUST to YES
             to allow NIML socket connections from anybody. (This only affects
             AFNI programs, not any other software on your computer.)
          -- You might also need to adjust your firewall settings to allow
             the reception of TCP/IP socket connections from outside computers.
             Firewalls are a separate issue from setting up AFNI host 'trusting',
             and the mechanics of how you can setup your firewall permissions is
             not something about which we can give you advice.
    
       -np PORT_OFFSET: Provide a port offset to allow multiple instances of
                        AFNI <--> SUMA, AFNI <--> 3dGroupIncorr, or any other
                        programs that communicate together to operate on the same
                        machine. 
                        All ports are assigned numbers relative to PORT_OFFSET.
             The same PORT_OFFSET value must be used on all programs
               that are to talk together. PORT_OFFSET is an integer in
               the inclusive range [1025 to 65500]. 
             When you want to use multiple instances of communicating programs, 
               be sure the PORT_OFFSETS you use differ by about 50 or you may
               still have port conflicts. A BETTER approach is to use -npb below.
       -npq PORT_OFFSET: Like -np, but more quiet in the face of adversity.
       -npb PORT_OFFSET_BLOC: Simliar to -np, except it is easier to use.
                              PORT_OFFSET_BLOC is an integer between 0 and
                              MAX_BLOC. MAX_BLOC is around 4000 for now, but
                              it might decrease as we use up more ports in AFNI.
                              You should be safe for the next 10 years if you 
                              stay under 2000.
                              Using this function reduces your chances of causing
                              port conflicts.
    
             See also afni and suma options: -list_ports and -port_number for 
                information about port number assignments.
    
             You can also provide a port offset with the environment variable
                AFNI_PORT_OFFSET. Using -np overrides AFNI_PORT_OFFSET.
    
       -max_port_bloc: Print the current value of MAX_BLOC and exit.
                       Remember this value can get smaller with future releases.
                       Stay under 2000.
       -max_port_bloc_quiet: Spit MAX_BLOC value only and exit.
       -num_assigned_ports: Print the number of assigned ports used by AFNI 
                            then quit.
       -num_assigned_ports_quiet: Do it quietly.
    
         Port Handling Examples:
         -----------------------
             Say you want to run three instances of AFNI <--> SUMA.
             For the first you just do: 
                suma -niml -spec ... -sv ...  &
                afni -niml &
             Then for the second instance pick an offset bloc, say 1 and run
                suma -niml -npb 1 -spec ... -sv ...  &
                afni -niml -npb 1 &
             And for yet another instance:
                suma -niml -npb 2 -spec ... -sv ...  &
                afni -niml -npb 2 &
             etc.
    
             Since you can launch many instances of communicating programs now,
                you need to know wich SUMA window, say, is talking to which AFNI.
                To sort this out, the titlebars now show the number of the bloc 
                of ports they are using. When the bloc is set either via 
                environment variables AFNI_PORT_OFFSET or AFNI_PORT_BLOC, or  
                with one of the -np* options, window title bars change from 
                [A] to [A#] with # being the resultant bloc number.
             In the examples above, both AFNI and SUMA windows will show [A2]
                when -npb is 2.
    
    
     -NOshm = Do NOT reconnect to AFNI using shared memory, rather than TCP/IP,
              when using 'localhost' (i.e., AFNI and 3dGroupInCorr are running
              on the same system).
           ++ The default is to use shared memory for communication when
              possible, since this method of transferring large amounts of
              data between programs on the same computer is much faster.
           ++ If you have a problem with the shared memory communication,
              use '-NOshm' to use TCP/IP for all communications.
           ++ If you use '-VERB', you will get a very detailed progress report
              from 3dGroupInCorr as it computes, including elapsed times for
              each stage of the process, including transmit time to AFNI.
    
     -suma = Talk to suma instead of afni, using surface-based i/o data.
     -sdset_TYPE = Set the output format in surface-based batch mode to
                   TYPE. For allowed values of TYPE, search for option
                   called -o_TYPE in ConvertDset -help.
                   Typical values would be: 
                      -sdset_niml, -sdset_1D, or -sdset_gii
     -quiet = Turn off the 'fun fun fun in the sun sun sun' informational messages.
     -verb  = Print out extra informational messages for more fun!
     -VERB  = Print out even more informational messages for even more fun fun!!
     -debug = Do some internal testing (slows things down a little)
    
    ---------------*** Talairach (+trlc) vs. Original (+orig) ***---------------
    
    Normally, AFNI assigns the dataset sent by 3dGroupInCorr to the +tlrc view.
    However, you can tell AFNI to assign it to the +orig view instead.
    To do this, set environment variable AFNI_GROUPINCORR_ORIG to YES when
    starting AFNI; for example:
    
      afni -DAFNI_GROUPINCORR_ORIG=YES -niml
    
    This feature might be useful to you if you are doing a longitudinal study on
    some subject, comparing resting state maps before and after some treatment.
    
    -----------*** Group InstaCorr and AFNI's Clusterize function ***-----------
    
    In the past, you could not use Clusterize in the AFNI A controller at the
    same time that 3dGroupInCorr was actively connected.
               ***** This situation is no longer the case:   *****
              ****** Clusterize is available with InstaCorr! ******
    In particular, the 'Rpt' (report) button is very useful with 3dGroupInCorr.
    
    If you use '-covariates' AND '-sendall', 3dGroupInCorr will send to AFNI
    a set of 1D files containing the covariates.  You can use one of these
    as a 'Scat.1D' file in the Clusterize GUI to plot the individual subject
    correlations (averaged across a cluster) vs. the covariate values -- this
    graph can be amusing and even useful.
     --  If you don't know how to use this feature in Clusterize, then learn!
    
    ---------------*** Dataset-Level Scale Factors [Sep 2012] ***---------------
    
     -scale sf = Read file 'sf' that contains a scale factor value for each dataset
                 The file format is essentially the same as that for covariates:
                 * first line contains labels (which are ignored)
                 * each later line contains a dataset identifying label and a number
         FIRST LINE  -->  subject factor
         LATER LINES -->  Elvis   42.1
                          Fred    37.2
                          Ethel   2.71828
                          Lucy    3.14159
                 * The arctanh(correlation) values from dataset Elvis will be
                   multiplied by 42.1 before being put into the t-test analysis.
                 * All values reported and computed by 3dGroupInCorr will reflect
                   this scaling (e.g., the results from '-sendall').
                 * This option is for the International Man Of Mystery, PK.
                   -- And just for PK, if you use this option in the form '-SCALE',
                      then each value X in the 'sf' file is replaced by sqrt(X-3).
    
    --------------------------*** BATCH MODE [Feb 2011] ***-----------------------
    
    * In batch mode, instead of connecting AFNI or SUMA to get commands on
      what to compute, 3dGroupInCorr computes correlations (etc.) based on
      commands from an input file.
      ++ Batch mode works to produce 3D (AFNI, or NIfTI) or 2D surface-based 
         (SUMA or GIFTI format) datasets. 
    
    * Each line in the command file specifies the prefix for the output dataset
      to create, and then the set of seed vectors to use.
      ++ Each command line produces a distinct dataset.
      ++ If you want to put results from multiple commands into one big dataset,
         you will have to do that with something like 3dbucket or 3dTcat after
         running this program.
      ++ If an error occurs with one command line (e.g., a bad seed location is
         given), the program will not produce an output dataset, but will try
         to continue with the next line in the command file.
      ++ Note that I say 'seed vectors', since a distinct one is needed for
         each dataset comprising the inputs -setA (and -setB, if used).
    
    * Batch mode is invoked with the following option:
    
       -batch METHOD COMMANDFILENAME
    
      where METHOD specifies how the seed vectors are to be computed, and
      where COMMANDFILENAME specifies the file with the commands.
      ++ As a special case, if COMMANDFILENAME contains a space character,
         then instead of being interpreted as a filename, it will be used
         as the contents of a single line command file; for example:
           -batch IJK 'something.nii 33 44 55'
         could be used to produce a single output dataset named 'something.nii'.
      ++ Only one METHOD can be used per batch mode run of 3dGroupInCorr!
         You can't mix up 'IJK' and 'XYZ' modes, for example.
      ++ Note that this program WILL overwrite existing datasets, unlike most
         AFNI programs, so be careful.
    
    * METHOD must be one of the following strings (not case sensitive):
    
      ++ IJK     ==> the 3D voxel grid index triple (i,j,k) is given in FILENAME,
     or  IJKAVE      which tells the program to extract the time series from
                     each input dataset at that voxel and use that as the seed
                     vector for that dataset (if '-seedrad' is given, then the
                     seed vector will be averaged as done in interactive mode).
                  ** This is the same mode of operation as the interactive seed
                     picking via AFNI's 'InstaCorr Set' menu item.
                 -- FILE line format:  prefix i j k
    
      ++ XYZ     ==> very similar to 'IJK', but instead of voxel indexes being
     or  XYZAVE      given to specify the seed vectors, the RAI (DICOM) (x,y,z)
                     coordinates are given ('-seedrad' also applies).
                  ** If you insist on using LPI (neurological) coordinates, as
                     Some other PrograMs (which are Fine Software tooLs) do,
                     set environment variable AFNI_INSTACORR_XYZ_LPI to YES,
                     before running this program.
                 -- FILE line format:  prefix x y z
    
      ++ NODE    ==> the index of the surface node where the seed is located.
                     A simple line would contain a prefix and a node number.
                     The prefix sets the output name and the file format, 
                     if you include the extension. See also -sdset_TYPE option.
                     for controlling output format.
                     The node number specifies the seed node. Because you might
                     have two surfaces (-LRpairs option in 3dSetupGroupInCorr)
                     you can add 'L', or 'R' to the node index to specify its
                     hemisphere.
                     For example:
                         OccipSeed1 L720
                         OccipSeed2 R2033
                     If you don't specify the side in instances where you are
                     working with two hemispheres, the default is 'L'.
    
      ++ MASKAVE ==> each line on the command file specifies a mask dataset;
                     the nonzero voxels in that dataset are used to define
                     the list of seed voxels that will be averaged to give
                     the set of seed vectors.
                  ** You can use the usual '[..]' and '<..>' sub-brick and value
                     range selectors to modify the dataset on input.  Do not
                     put these selectors inside quotes in the command file!
                 -- FILE line format:  prefix maskdatasetname
    
      ++ IJKPV   ==> very similar to IJKAVE, XYZAVE, and MASKAVE (in that order),
      ++ XYZPV       but instead of extracting the average over the region
      ++ MASKPV      indicated, extracts the Principal Vector (in the SVD sense;
                     cf. program 3dLocalPV).
                  ** Note that IJKPV and XYZPV modes only work if seedrad > 0.
                  ** In my limited tests, the differences between the AVE and PV
                     methods are very small.  YMMV.
    
      ++ VECTORS ==> each line on the command file specifies an ASCII .1D
                     file which contains the set of seed vectors to use.
                     There must be as many columns in the .1D file as there
                     are input datasets in -setA and -setB combined.  Each
                     column must be as long as the maximum number of time
                     points in the longest dataset in -setA and -setB.
                  ** This mode is for those who want to construct their own
                     set of reference vectors in some clever way.
                  ** N.B.: This method has not yet been tested!
                 -- FILE line format:  prefix 1Dfilename
    
    -----------------------*** NEW BATCH MODES [Aug 2012] ***--------------------
    
     * These new modes allow you to specify a LOT of output datasets directly on the
       command line with a single option.  They are:
    
     -batchRAND n prefix ==> scatter n seeds around in space and compute the
                             output dataset for each of these seed points, where
                             'n' is an integer greater than 1.
    
     -batchGRID d prefix ==> for every d-th point along each of the x,y,z axes,
                             create an output dataset, where 'd' is an integer
                             in the range 1..9.  Note that setting d=1 will use
                             every voxel as a seed, and presumably produce a vast
                             armada of datasets through which you'll have to churn.
    
     * Each output dataset gets a filename of the form 'prefix_xxx_yyy_zzz', where
       'prefix' is the second argument after the '-batchXXXX' option, and 'xxx'
       is the x-axis index of the seed voxel, 'yyy' is the y-axis index of the
       seed voxel, and 'zzz' is the z-axis index of the seed voxel.
    
     * These options are like using the 'IJK' batch mode of operation at each seed
       voxel.  The only difference is that the set of seed points is generated by
       the program rather than being given by the user (i.e., you).  These two options
       differ only in the way the seed points are chosen (pseudo-randomly or regularly).
    
    ** You should be prepared for a LONG run and filling up a  **
    ** LOT of disk space when you use either of these options! **
    
     =========================================================================
    * This binary version of 3dGroupInCorr is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system.  You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful?  That varies with the program, and how well
       it was coded.  You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 8.
    * The maximum number of CPUs that will be used is now set to .... 8.
     =========================================================================
    ++ Authors: Bob Cox and Ziad Saad
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
