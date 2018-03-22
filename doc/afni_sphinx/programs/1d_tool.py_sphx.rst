**********
1d_tool.py
**********

.. _ahelp_1d_tool.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    =============================================================================
    1d_tool.py      - for manipulating and evaluating 1D files
    
       This program is meant to read/manipulate/write/diagnose 1D datasets.
       Input can be specified using AFNI sub-brick[]/time{} selectors.
    
    ---------------------------------------------------------------------------
    examples (very basic for now):
    
       Example 1.  Select by rows and columns, akin to 1dcat.
    
             1d_tool.py -infile 'data/X.xmat.1D[0..3]{0..5}' -write t1.1D
    
       Example 2.  Compare with selection by separate options.
    
             1d_tool.py -infile data/X.xmat.1D                  \
                        -select_cols '0..3' -select_rows '0..5' \
                        -write t2.1D
             diff t1.1D t2.1D
    
       Example 2b. Select or remove columns by label prefixes.
    
           Keep only bandpass columns:
    
             1d_tool.py -infile X.xmat.1D -write X.bandpass.1D    \
                        -label_prefix_keep bandpass
    
           Remove only bandpass columns (maybe for 3dRFSC):
    
             1d_tool.py -infile X.xmat.1D -write X.no.bandpass.1D \
                        -label_prefix_drop bandpass
    
           Keep polort columns (start with 'Run') motion shifts ('d') and labels
           starting with 'a' and 'b'.  But drop 'bandpass' columns:
    
             1d_tool.py -infile X.xmat.1D -write X.weird.1D   \
                        -label_prefix_keep Run d a b          \
                        -label_prefix_drop bandpass
    
       Example 2c. Select columns by group values, 3 examples.
    
           First be sure of what the group labels represent.
    
             1d_tool.py -infile X.xmat.1D -show_group_labels
    
           i) Select polort (group -1) and other baseline (group 0) terms.
    
             1d_tool.py -infile X.xmat.1D -select_groups -1 0 -write baseline.1D
    
           ii) Select everything but baseline groups (anything positive).
    
             1d_tool.py -infile X.xmat.1D -select_groups POS -write regs.of.int.1D
    
           iii) Reorder to have rests of interest, then motion, then polort.
    
             1d_tool.py -infile X.xmat.1D -select_groups POS 0, -1 -write order.1D
    
       Example 2d. Select specific runs from the input.  Note that X.xmat.1D may
            have runs defined automatically, but for an arbitrary input, they may
            need to be specified via -set_run_lengths.
    
            i) 
    
             1d_tool.py -infile X.xmat.1D -write X.bandpass.1D    \
    
       Example 3.  Transpose a dataset, akin to 1dtranspose.
    
             1d_tool.py -infile t3.1D -transpose -write ttr.1D
    
       Example 4a. Pad a file of regressors for a single run (#2) with zeros, so
           that it becomes run 2 of 7 (runs are 1-based).
    
             1d_tool.py -infile ricor_r02.1D -pad_into_many_runs 2 7 \
                        -write ricor_r02_all.1D
    
       Example 4b. Similar to 4a, but specify varying TRs per run.  The number of
           runs must match the number of run_lengths parameters.
    
             1d_tool.py -infile ricor_r02.1D -pad_into_many_runs 2 7 \
                        -set_run_lengths 64 61 67 61 67 61 67        \
                        -write ricor_r02_all.1D
    
       Example 5.  Display small details about a 1D dataset:
    
           a. Display number of rows and columns for a 1D dataset.
    
             1d_tool.py -infile ricor_r02.1D -show_rows_cols
    
           b. Display indices of regressors of interest.
    
             1d_tool.py -infile X.xmat.1D -show_indices_interest
    
           c. Display labels by group.
    
             1d_tool.py -infile X.xmat.1D -show_group_labels
    
       Example 6a.  Show correlation matrix warnings for this matrix.
    
             1d_tool.py -infile X.xmat.1D -show_cormat_warnings
    
       Example 6b.  Show entire correlation matrix.
    
             1d_tool.py -infile X.xmat.1D -show_cormat
    
       Example 7a. Output temporal derivative of motion regressors.  There are
           9 runs in dfile_rall.1D, and derivatives are applied per run.
    
             1d_tool.py -infile dfile_rall.1D -set_nruns 9 \
                        -derivative -write motion.deriv.1D
    
       Example 7b. Similar to 7a, but let the run lengths vary.  The sum of run
           lengths should equal the number of time points.
    
             1d_tool.py -infile dfile_rall.1D                       \
                        -set_run_lengths 64 64 64 64 64 64 64 64 64 \
                        -derivative -write motion.deriv.rlens.1D
    
       Example 7c. Use forward differences, instead of the default backward
           differences.
    
             1d_tool.py -infile dfile_rall.1D                       \
                        -set_run_lengths 64 64 64 64 64 64 64 64 64 \
                        -forward_diff -write motion.deriv.rlens.1D
    
       Example 8.  Verify whether labels show slice-major ordering (where all
           slice0 regressors come first, then all slice1 regressors, etc).  Either
           show the labels and verify visually, or print whether it is true.
    
             1d_tool.py -infile scan_2.slibase.1D'[0..12]' -show_labels
             1d_tool.py -infile scan_2.slibase.1D -show_labels
             1d_tool.py -infile scan_2.slibase.1D -show_label_ordering
    
       Example 9a. Given motion.1D, take the derivative (ignoring run breaks) and
           the Euclidean Norm, and write as e.norm.1D.  This might be plotted to
           show show sudden motion as a single time series.
    
             1d_tool.py -infile motion.1D -set_nruns 9              \
                        -derivative  -collapse_cols euclidean_norm  \
                        -write e.norm.1D
    
       Example 9b. Like 9a, but supposing the run lengths vary (still 576 TRs).
    
             1d_tool.py -infile motion.1D                           \
                        -set_run_lengths 64 61 67 61 67 61 67 61 67 \
                        -derivative  -collapse_cols euclidean_norm  \
                        -write e.norm.rlens.1D
    
       Example 9c. Like 9b, but weight the rotations as 0.9 mm.
    
             1d_tool.py -infile motion.1D                           \
                        -set_run_lengths 64 61 67 61 67 61 67 61 67 \
                        -derivative  -collapse_cols weighted_enorm  \
                        -weight_vec .9 .9 .9 1 1 1                  \
                        -write e.norm.weighted.1D
    
      Example 10.  Given motion.1D, create censor files to use in 3dDeconvolve,
           where a TR is censored if the derivative values have a Euclidean Norm
           above 1.2.  It is common to also censor each previous TR, as motion may
           span both (previous because "derivative" is actually a backward
           difference).
    
           The file created by -write_censor can be used with 3dD's -censor option.
           The file created by -write_CENSORTR can be used with -CENSORTR.  They
           should have the same effect in 3dDeconvolve.  The CENSORTR file is more
           readable, but the censor file is better for plotting against the data.
     
           a. general example
    
              1d_tool.py -infile motion.1D -set_nruns 9     \
                         -derivative -censor_prev_TR        \
                         -collapse_cols euclidean_norm      \
                         -moderate_mask -1.2 1.2            \
                         -show_censor_count                 \
                         -write_censor subjA_censor.1D      \
                         -write_CENSORTR subjA_CENSORTR.txt
    
           b. using -censor_motion
    
              The -censor_motion option is available, which implies '-derivative',
              '-collapse_cols euclidean_norm', 'moderate_mask -LIMIT LIMIT', and the
              prefix for '-write_censor' and '-write_CENSORTR' output files.  This
              option will also result in subjA_enorm.1D being written, which is the
              euclidean norm of the derivative, before the extreme mask is applied.
    
              1d_tool.py -infile motion.1D -set_nruns 9     \
                         -show_censor_count                 \
                         -censor_motion 1.2 subjA           \
                         -censor_prev_TR
    
           c. allow the run lengths to vary
    
              1d_tool.py -infile motion.1D                           \
                         -set_run_lengths 64 61 67 61 67 61 67 61 67 \
                         -show_censor_count                          \
                         -censor_motion 1.2 subjA_rlens              \
                         -censor_prev_TR
    
           Consider also '-censor_prev_TR' and '-censor_first_trs'.
    
      Example 11.  Demean the data.  Use motion parameters as an example.
    
           The demean operation is done per run (default is 1 when 1d_tool.py
           does not otherwise know).
    
           a. across all runs (if runs are not known from input file)
    
             1d_tool.py -infile dfile_rall.1D -demean -write motion.demean.a.1D
    
           b. per run, over 9 runs of equal length
    
             1d_tool.py -infile dfile_rall.1D -set_nruns 9      \
                    -demean -write motion.demean.b.1D
    
           c. per run, over 9 runs of varying length
    
             1d_tool.py -infile dfile_rall.1D                   \
                    -set_run_lengths 64 61 67 61 67 61 67 61 67 \
                    -demean -write motion.demean.c.1D
    
      Example 12.  "Uncensor" the data, zero-padding previously censored TRs.
    
           Note that an X-matrix output by 3dDeconvolve contains censor
           information in GoodList, which is the list of uncensored TRs.
    
           a. if the input dataset has censor information
    
             1d_tool.py -infile X.xmat.1D -censor_fill -write X.uncensored.1D
    
           b. if censor information needs to come from a parent
    
             1d_tool.py -infile sum.ideal.1D -censor_fill_parent X.xmat.1D \
                        -write sum.ideal.uncensored.1D
    
           c. if censor information needs to come from a simple 1D time series
    
             1d_tool.py -censor_fill_parent motion_FT_censor.1D \
                        -infile cdata.1D -write cdata.zeropad.1D
    
      Example 13. Show whether the input file is valid as a numeric data file.
    
           a. as any generic 1D file
    
              1d_tool.py -infile data.txt -looks_like_1D
    
           b. as a 1D stim_file, of 3 runs of 64 TRs (TR is irrelevant)
    
              1d_tool.py -infile data.txt -looks_like_1D \
                         -set_run_lengths 64 64 64
    
           c. as a stim_times file with local times
    
              1d_tool.py -infile data.txt -looks_like_local_times \
                         -set_run_lengths 64 64 64 -set_tr 2
    
           d. as a 1D or stim_times file with global times
    
              1d_tool.py -infile data.txt -looks_like_global_times \
                         -set_run_lengths 64 64 64 -set_tr 2
    
           e. report modulation type (amplitude and/or duration)
    
              1d_tool.py -infile data.txt -looks_like_AM 
    
           f. perform all tests, reporting all errors
    
              1d_tool.py -infile data.txt -looks_like_test_all \
                         -set_run_lengths 64 64 64 -set_tr 2
    
       Example 14. Split motion parameters across runs, but keep them at the
           original length so they apply to the same multi-run regression.  Each
           file will be the same as the original for the run it applies to, but
           zero across all other runs.
    
           Note that -split_into_pad_runs takes the output prefix as a parameter.
    
             1d_tool.py -infile motion.1D                   \
                        -set_run_lengths 64 64 64           \
                        -split_into_pad_runs mot.padded
    
           The output files are:
              mot.padded.r01.1D   mot.padded.r02.1D   mot.padded.r03.1D
    
           If the run lengths are the same -set_nruns is shorter...
    
             1d_tool.py -infile motion.1D                   \
                        -set_nruns 3                        \
                        -split_into_pad_runs mot.padded
    
       Example 15a. Show the maximum pairwise displacement in the motion parameter
           file.  So over all TRs pairs, find the biggest displacement.
    
           In one direction it is easy (AP say).  If the minimum AP shift is -0.8
           and the maximum is 1.5, then the maximum displacement is 2.3 mm.  It
           is less clear in 6-D space, and instead of trying to find an enveloping
           set of "coordinates", distances between all N choose 2 pairs are
           evaluated (brute force).
    
            1d_tool.py -infile dfile_rall.1D -show_max_displace
    
       Example 15b. Like 15a, but do not include displacement from censored TRs.
    
            1d_tool.py -infile dfile_rall.1D -show_max_displace \
                       -censor_infile motion_censor.1D
    
       Example 16. Randomize a list of numbers, say, those from 1..40.
    
           The numbers can come from 1deval, with the result piped to
           '1d_tool.py -infile stdin -randomize_trs ...'.
    
            1deval -num 40 -expr t+1 |   \
               1d_tool.py -infile stdin -randomize_trs -write stdout
    
            See also -seed.
    
       Example 17. Display min, mean, max, stdev of 1D file.
    
            1d_tool.py -show_mmms -infile data.1D
    
           To be more detailed, get stats for each of x, y, and z directional
           blur estimates for all subjects.  Cat(enate) all of the subject files
           and pipe that to 1d_tool.py with infile - (meaning stdin).
    
            cat subject_results/group.*/sub*/*.results/blur.errts.1D \
                    | 1d_tool.py -show_mmms -infile -
    
       Example 18. Just output censor count for default method.
    
           This will output nothing but the number of TRs that would be censored,
           akin to using -censor_motion and -censor_prev_TR.
    
            1d_tool.py -infile dfile_rall.1D -set_nruns 3 -quick_censor_count 0.3
    
            1d_tool.py -infile dfile_rall.1D -set_run_lengths 100 80 120 \
                       -quick_censor_count 0.3
    
       Example 19. Compute GCOR from some 1D file.
    
           * Note, time should be in the vertical direction of the file
             (else use -transpose).
    
            1d_tool.py -infile data.1D -show_gcor
    
           Or get some GCOR documentation and many values.
            
            1d_tool.py -infile data.1D -show_gcor_doc
            1d_tool.py -infile data.1D -show_gcor_all
    
       Example 20. Display censored or uncensored TRs lists (for use in 3dTcat).
    
           TRs which were censored:
    
              1d_tool.py -infile X.xmat.1D -show_trs_censored encoded
    
           TRs which were applied in analysis (those NOT censored):
    
              1d_tool.py -infile X.xmat.1D -show_trs_uncensored encoded
    
           Only those applied in run #2 (1-based).
    
              1d_tool.py -infile X.xmat.1D -show_trs_uncensored encoded \
                         -show_trs_run 2
    
       Example 21. Convert to rank order.
    
           a. show rank order of slice times from a 1D file
    
             1d_tool.py -infile slice_times.1D -rank -write -
    
           b. show rank order of slice times piped directly from 3dinfo
    
             3dinfo -slice_timing epi+orig | 1d_tool.py -infile - -rank -write -
    
           c. show rank order using 'competition' rank, instead of default 'dense'
    
             3dinfo -slice_timing epi+orig \
                    | 1d_tool.py -infile - -rank_style competition -write -
    
       Example 22. Guess volreg base index from motion parameters.
    
             1d_tool.py -infile dfile_rall.1D -collapse_cols enorm -show_argmin
    
       Example 23. Convert volreg parameters to those suitable for 3dAllineate.
    
             1d_tool.py -infile dfile_rall.1D -volreg2allineate \
                        -write allin_rall_aff12.1D
    
       Example 24. Show TR counts per run.
    
            a. list the number of TRs in each run
    
              1d_tool.py -infile X.xmat.1D -show_tr_run_counts trs
    
            b. list the number of TRs censored in each run
    
              1d_tool.py -infile X.xmat.1D -show_tr_run_counts trs_cen
    
            c. list the number of TRs prior to censoring in each run
    
              1d_tool.py -infile X.xmat.1D -show_tr_run_counts trs_no_cen
    
            d. list the fraction of TRs censored per run
    
              1d_tool.py -infile X.xmat.1D -show_tr_run_counts frac_cen
    
            e. list the fraction of TRs censored in run 3
    
              1d_tool.py -infile X.xmat.1D -show_tr_run_counts frac_cen \
                         -show_trs_run 3
    
       Example 25. Show number of runs.
    
              1d_tool.py -infile X.xmat.1D -show_num_runs
    
       Example 26. Convert global index to run and TR index.
    
           Note that run indices are 1-based, while TR indices are 0-based,
           as usual.  Confusion is key.
    
           a. explicitly, given run lengths
    
              1d_tool.py -set_run_lengths 100 80 120 -index_to_run_tr 217
    
           b. implicitly, given an X-matrix (** be careful about censoring **)
    
              1d_tool.py -infile X.nocensor.xmat.1D -index_to_run_tr 217
    
       Example 27. Display length of response curve.
    
              1d_tool.py -show_trs_to_zero -infile data.1D
    
           Print out the length of the input (in TRs, say) until the data
           values become a constant zero.  Zeros that are followed by non-zero
           values are irrelevant.
    
       Example 28. convert slice order to slice times.
    
           A slice order might be the sequence in which slices were acquired.
           For example, with 33 slices, perhaps the order is:
    
              set slice_order = ( 0 6 12 18 24 30 1 7 13 19 25 31 2 8 14 20 \
                                  26 32 3 9 15 21 27 4 10 16 22 28 5 11 17 23 29 )
    
           Put this in a file:
    
              echo $slice_order > slice_order.1D
              1d_tool.py -set_tr 2 -slice_order_to_times \
                         -infile slice_order.1D -write slice_times.1D
    
           Or as a filter:
    
              echo $slice_order | 1d_tool.py -set_tr 2 -slice_order_to_times \
                                             -infile - -write -
    
    ---------------------------------------------------------------------------
    basic informational options:
    
       -help                        : show this help
       -hist                        : show the module history
       -show_valid_opts             : show all valid options
       -ver                         : show the version number
    
    ----------------------------------------
    required input:
    
       -infile DATASET.1D           : specify input 1D file
    
    ----------------------------------------
    general options:
    
       -add_cols NEW_DSET.1D        : extend dset to include these columns
    
       -backward_diff               : take derivative as first backward difference
    
            Take the backward differences at each time point.  For each index > 0,
            value[index] = value[index] - value[index-1], and value[0] = 0.
    
            This option is identical to -derivative.
    
            See also -forward_diff, -derivative, -set_nruns, -set_run_lens.
    
       -collapse_cols METHOD        : collapse multiple columns into one, where
    
            METHOD is one of: min, max, minabs, maxabs, euclidean_norm,
                              weighted_enorm.
    
            Consideration of the euclidean_norm method:
    
               For censoring, the euclidean_norm method is used (sqrt(sum squares)).
               This combines rotations (in degrees) with shifts (in mm) as if they
               had the same weight.
    
               Note that assuming rotations are about the center of mass (which
               should produce a minimum average distance), then the average arc
               length (averaged over the brain mask) of a voxel rotated by 1 degree
               (about the CM) is the following (for the given datasets):
    
                  TT_N27+tlrc:        0.967 mm (average radius = 55.43 mm)
                  MNIa_caez_N27+tlrc: 1.042 mm (average radius = 59.69 mm)
                  MNI_avg152T1+tlrc:  1.088 mm (average radius = 62.32 mm)
    
               The point of these numbers is to suggest that equating degrees and
               mm should be fine.  The average distance caused by a 1 degree
               rotation is very close to 1 mm (in an adult human).
    
             * 'enorm' is short for 'euclidean_norm'.
    
             * Use of weighted_enorm requires the -weight_vec option.
    
                  e.g. -collapse_cols weighted_enorm -weight_vec .9 .9 .9 1 1 1 
    
       -censor_motion LIMIT PREFIX  : create censor files
    
            This option implies '-derivative', '-collapse_cols euclidean_norm',
            'moderate_mask -LIMIT LIMIT' and applies PREFIX for '-write_censor'
            and '-write_CENSORTR' output files.  It also outputs the derivative
            of the euclidean norm, before the limit it applied.
    
            The temporal derivative is taken with run breaks applied (derivative
            of the first run of a TR is 0), then the columns are collapsed into
            one via each TR's vector length (Euclidean Norm: sqrt(sum of squares)).
            After that, a mask time series is made from TRs with values outside
            (-LIMIT,LIMIT), i.e. if >= LIMIT or <= LIMIT, result is 1.
    
            This binary time series is then written out in -CENSORTR format, with
            the moderate TRs written in -censor format (either can be applied in
            3dDeconvolve).  The output files will be named PREFIX_censor.1D,
            PREFIX_CENSORTR.txt and PREFIX_enorm.1D (e.g. subj123_censor.1D,
            subj123_CENSORTR.txt and subj123_enorm.1D).
    
            Besides an input motion file (-infile), the number of runs is needed
            (-set_nruns or -set_run_lengths).
    
            Consider also '-censor_prev_TR' and '-censor_first_trs'.
            See example 10.
    
       -censor_fill                 : expand data, filling censored TRs with zeros
       -censor_fill_parent PARENT   : similar, but get censor info from a parent
    
            The output of these operation is a longer dataset.  Each TR that had
            previously been censored is re-inserted as a zero.
    
            The purpose of this is to make 1D time series data properly align
            with the all_runs dataset, for example.  Otherwise, the ideal 1D data
            might have missing TRs, and so will align worse with responses over
            the duration of all runs (it might start aligned, but drift earlier
            and earlier as more TRs are censored).
    
            See example 12.
    
       -censor_infile CENSOR_FILE   : apply censoring to -infile dataset
    
            This removes TRs from the -infile dataset where the CENSOR_FILE is 0.
            The censor file is akin to what is used with "3dDeconvolve -censor",
            where TRs with 1 are kept and those with 0 are excluded from analysis.
    
            See example 15b.
    
       -censor_first_trs N          : when censoring motion, also censor the first
                                      N TRs of each run
       -censor_next_TR              : for each censored TR, also censor next one
                                      (probably for use with -forward_diff)
       -censor_prev_TR              : for each censored TR, also censor previous
       -cormat_cutoff CUTOFF        : set cutoff for cormat warnings (in [0,1])
       -demean                      : demean each run (new mean of each run = 0.0)
    
       -derivative                  : take the temporal derivative of each vector
                                      (done as first backward difference)
    
            Take the backward differences at each time point.  For each index > 0,
            value[index] = value[index] - value[index-1], and value[0] = 0.
    
            This option is identical to -backward_diff.
    
            See also -backward_diff, -forward_diff, -set_nruns, -set_run_lens.
    
       -extreme_mask MIN MAX        : make mask of extreme values
    
            Convert to a 0/1 mask, where 1 means the given value is extreme
            (outside the (MIN, MAX) range), and 0 means otherwise.  This is the
            opposite of -moderate_mask (not exactly, both are inclusive).
    
            Note: values = MIN or MAX will be in both extreme and moderate masks.
    
            Note: this was originally described incorrectly in the help.
    
       -forward_diff                : take first forward difference of each vector
    
            Take the first forward differences at each time point.  For index<last,
            value[index] = value[index+1] - value[index], and value[last] = 0.
    
            The difference between -forward_diff and -backward_diff is a time shift
            by one index.
    
            See also -backward_diff, -derivative, -set_nruns, -set_run_lens.
    
       -index_to_run_tr INDEX       : convert global INDEX to run and TR indices
    
            Given a list of run lengths, convert INDEX to a run and TR index pair.
    
            This option requires -set_run_lens or maybe an Xmat.
            
            See also -set_run_lens example 26.
    
       -moderate_mask MIN MAX       : make mask of moderate values
    
            Convert to a 0/1 mask, where 1 means the given value is moderate
            (within [MIN, MAX]), and 0 means otherwise.  This is useful for
            censoring motion (in the -censor case, not -CENSORTR), where the
            -censor file should be a time series of TRs to apply.
    
            See also -extreme_mask.
    
       -label_prefix_drop prefix1 prefix2 ... : remove labels matching prefix list
    
            e.g. to remove motion shift (starting with 'd') and bandpass labels:
    
                 -label_prefix_drop d bandpass
    
            This is a type of column selection.
    
            Use this option to remove columns from a matrix that have labels
            starting with any from the given prefix list.
    
            This option can be applied along with -label_prefix_keep.
    
            See also -label_prefix_keep and example 2b.
    
       -label_prefix_keep prefix1 prefix2 ... : keep labels matching prefix list
    
            e.g. to keep only motion shift (starting with 'd') and bandpass labels:
    
                 -label_prefix_keep d bandpass
    
            This is a type of column selection.
    
            Use this option to keep columns from a matrix that have labels starting
            with any from the given prefix list.
    
            This option can be applied along with -label_prefix_drop.
    
            See also -label_prefix_drop and example 2b.
    
       "Looks like" options:
    
            These are terminal options that check whether the input file seems to
            be of type 1D, local stim_times or global stim_times formats.  The only
            associated options are currently -infile, -set_run_lens, -set_tr and
            -verb.
    
            They are terminal in that no other 1D-style actions are performed.
            See 'timing_tool.py -help' for details on stim_times operations.
    
       -looks_like_1D               : is the file in 1D format
    
            Does the input data file seem to be in 1D format?
    
                - must be rectangular (same number of columns per row)
                - duration must match number of rows (if run lengths are given)
    
       -looks_like_AM               : does the file have modulators?
    
            Does the file seem to be in local or global times format, and
            do the times have modulators?
    
                - amplitude modulators should use '*' format (e.g. 127.3*5.1)
                - duration modulators should use trailing ':' format (12*5.1:3.4)
                - number of amplitude modulators should be constant
    
       -looks_like_local_times      : is the file in local stim_times format
    
            Does the input data file seem to be in the -stim_times format used by
            3dDeconvolve (and timing_tool.py)?  More specifically, is it the local
            format, with one scanning run per row.
    
                - number of rows must match number of runs
                - times cannot be negative
                - times must be unique per run (per row)
                - times cannot exceed the current run time
    
       -looks_like_global_times     : is the file in global stim_times format
    
            Does the input data file seem to be in the -stim_times format used by
            3dDeconvolve (and timing_tool.py)?  More specifically, is it the global
            format, either as one long row or one long line?
    
                - must be one dimensional (either a single row or column)
                - times cannot be negative
                - times must be unique
                - times cannot exceed total duration of all runs
    
       -looks_like_test_all         : run all -looks_like tests
    
            Applies all "looks like" test options: -looks_like_1D, -looks_like_AM,
            -looks_like_local_times and -looks_like_global_times.
    
       -overwrite                   : allow overwriting of any output dataset
    
       -pad_into_many_runs RUN NRUNS : pad as current run of num_runs
    
            e.g. -pad_into_many_runs 2 7
    
            This option is used to create a longer time series dataset where the
            input is consider one particular run out of many.  The output is
            padded with zero for all run TRs before and after this run.
    
            Given the example, there would be 1 run of zeros, then the input would
            be treated as run 2, and there would be 5 more runs of zeros.
    
       -quick_censor_count LIMIT    : output # TRs that would be censored
    
            e.g. -quick_censor_count 0.3
    
            This is akin to -censor_motion, but it only outputs the number of TRs
            that would be censored.  It does not actually create a censor file.
    
            This option essentially replaces these:
    
               -derivative -demean -collapse_cols euclidean_norm
               -censor_prev_TR -verb 0 -show_censor_count 
               -moderate_mask 0 LIMIT
    
       -rank                        : convert data to rank order
                                      0-based index order of small to large values
                                      The default rank STYLE is 'dense'.
    
            See also -rank_style.
    
       -rank_style STYLE            : convert to rank using the given style
    
            The STYLE refers to what to do in the case of repeated values.
            Assuming inputs 4 5 5 9...
    
                dense      - repeats get same rank, no gaps in rank
                            - same a "3dmerge -1rank"
                            - result: 0 1 1 2
    
                competition - repeats get same rank, leading to gaps in rank
                            - same a "3dmerge -1rank"
                            - result: 0 1 1 3
                              (case '2' is counted, though no such rank occurs)
    
            Option '-rank' uses style 'dense'.
    
            See also -rank.
    
       -reverse_rank                : convert data to reverse rank order
                                      (large values come first)
    
       -reverse                     : reverse data over time
       -randomize_trs               : randomize the data over time
       -seed SEED                   : set random number seed (integer)
       -select_groups g0 g1 ...     : select columns by group numbers
    
            e.g. -select groups 0
            e.g. -select groups POS 0
    
            An X-matrix dataset (e.g. X.xmat.1D) often has columns partitioned by
            groups, such as:
                    -1  : polort regressors
                     0  : motion regressors and other (non-polort) baseline terms
                     N>0: regressors of interest
    
            This option can be used to select columns by integer groups, with
            special cases of POS (regs of interest), NEG (probably polort).
            Note that NONNEG is unneeded as it is the pair POS 0.
    
            See also -show_group_labels.
    
       -select_cols SELECTOR        : apply AFNI column selectors, [] is optional
                                      e.g. '[5,0,7..21(2)]'
       -select_rows SELECTOR        : apply AFNI row selectors, {} is optional
                                      e.g. '{5,0,7..21(2)}'
       -select_runs r1 r2 ...       : extract the given runs from the dataset
                                      (these are 1-based run indices)
                                      e.g. 2
                                      e.g. 2 3 1 1 1 1 1 4
       -set_nruns NRUNS             : treat the input data as if it has nruns
                                      (e.g. applies to -derivative and -demean)
    
            See examples 7a, 10a and b, and 14.
    
       -set_run_lengths N1 N2 ...   : treat as if data has run lengths N1, N2, etc.
                                      (applies to -derivative, for example)
    
            Notes:  o  option -set_nruns is not allowed with -set_run_lengths
                    o  the sum of run lengths must equal NT
    
            See examples 7b, 10c and 14.
    
       -set_tr TR                   : set the TR (in seconds) for the data
       -show_argmin                 : display the index of min arg (of first column)
       -show_censor_count           : display the total number of censored TRs
                               Note : if input is a valid xmat.1D dataset, then the
                                      count will come from the header.  Otherwise
                                      the input is assumed to be a binary censor
                                      file, and zeros are simply counted.
       -show_cormat                 : display correlation matrix
       -show_cormat_warnings        : display correlation matrix warnings
       -show_gcor                   : display GCOR: the average correlation
       -show_gcor_all               : display many ways of computing (a) GCOR
       -show_gcor_doc               : display descriptions of those ways
       -show_group_labels           : display group and label, per column
       -show_indices_baseline       : display column indices for baseline
       -show_indices_motion         : display column indices for motion regressors
       -show_indices_interest       : display column indices for regs of interest
       -show_label_ordering         : display the labels
       -show_labels                 : display the labels
       -show_max_displace           : display max displacement (from motion params)
                                      - the maximum pairwise distance (enorm)
       -show_mmms                   : display min, mean, max, stdev of columns
       -show_num_runs               : display number of runs found
       -show_rows_cols              : display the number of rows and columns
       -show_tr_run_counts STYLE    : display TR counts per run, according to STYLE
                                      STYLE can be one of:
                                         trs        : TR counts
                                         trs_cen    : censored TR counts
                                         trs_no_cen : TR counts, as if no censoring
                                         frac_cen   : fractions of TRs censored
            See example 24.
    
       -show_trs_censored STYLE     : display a list of TRs which were censored
       -show_trs_uncensored STYLE   : display a list of TRs which were not censored
                                      STYLE can be one of:
                                         comma      : comma delimited
                                         space      : space delimited
                                         encoded    : succinct selector list
                                         verbose    : chatty
            See example 20.
    
       -show_trs_run RUN            : restrict -show_trs_[un]censored to the given
                                      1-based run
       -show_trs_to_zero            : display number of TRs before final zero value
                                      (e.g. length of response curve)
    
       -slice_order_to_times        : convert a list of slice indices to times
    
            Programs like to3d, 3drefit, 3dTcat and 3dTshift expect slice timing
            to be a list of slice times over the sequential slices.  But in some
            cases, people have an ordered list of slices.  So the sorting needs
            to change.
    
            If TR=2 and the slice order is:  0  2  4  6  8  1  3  5  7  9
    
            Then the slices/times ordered by time (as input) are:
    
               slices: 0    2    4    6    8    1    3    5    7    9
               times:  0.0  0.2  0.4  0.6  0.8  1.0  1.2  1.4  1.6  1.8
    
            And the slices/times ordered instead by slice index are:
    
               slices: 0    1    2    3    4    5    6    7    8    9
               times:  0.0  1.0  0.2  1.2  0.4  1.4  0.6  1.6  0.8  1.8
    
            It is this final list of times that is output.
    
            See example 28.
    
       -sort                        : sort data over time (smallest to largest)
                                      - sorts EVERY vector
                                      - consider the -reverse option
    
       -split_into_pad_runs PREFIX  : split input into one padded file per run
    
            e.g. -split_into_pad_runs motion.pad
    
            This option is used for breaking a set of regressors up by run.  The
            output would be one file per run, where each file is the same as the
            input for the run it corresponds to, and is padded with 0 across all
            other runs.
    
            Assuming the 300 row input dataset spans 3 100-TR runs, then there
            would be 3 output datasets created, each still be 300 rows:
    
                motion.pad.r01.1D   : 100 rows as input, 200 rows of 0
                motion.pad.r02.1D   : 100 rows of 0, 100 rows as input, 100 of 0
                motion.pad.r03.1D   : 200 rows of 0, 100 rows as input
    
            This option requires either -set_nruns or -set_run_lengths.
    
            See example 14.
    
       -transpose                   : transpose the input matrix (rows for columns)
       -transpose_write             : transpose the output matrix before writing
       -volreg2allineate            : convert 3dvolreg parameters to 3dAllineate
    
            This option should be used when the -infile file is a 6 column file
            of motion parameters (roll, pitch, yaw, dS, dL, dP).  The output would
            be converted to a 12 parameter file, suitable for input to 3dAllineate
            via the -1Dparam_apply option.
    
            volreg:     roll, pitch, yaw,   dS,    dL,     dP
            3dAllinate: -dL,  -dP,   -dS,   roll,  pitch,  yaw,  0,0,0,  0,0,0
    
            These parameters would be to correct the motion, akin to what 3dvolreg
            did (i.e. they are the negative estimates of how the subject moved).
    
            See example 23.
    
       -write FILE                  : write the current 1D data to FILE
    
       -weight_vec v1 v2 ...        : supply weighting vector
    
            e.g. -weight_vec 0.9 0.9 0.9 1 1 1
    
            This vector currently works only with the weighted_enorm method for
            the -collapse_cols option.  If supplied (as with the example), it will
            weight the angles at 0.9 times the weights of the shifts in the motion
            parameters output by 3dvolreg.
    
            See also -collapse_cols.
    
       -write_censor FILE           : write as boolean censor.1D
    
            e.g. -write_censor subjA_censor.1D
    
            This file can be given to 3dDeconvolve to censor TRs with excessive
            motion, applied with the -censor option.
    
                e.g. 3dDeconvolve -censor subjA_censor.1D
    
            This file works well for plotting against the data, where the 0 entries
            are removed from the regression of 3dDeconvolve.  Alternatively, the
            file created with -write_CENSORTR is probably more human readable.
    
       -write_CENSORTR FILE         : write censor times as CENSORTR string
    
            e.g. -write_CENSORTR subjA_CENSORTR.txt
    
            This file can be given to 3dDeconvolve to censor TRs with excessive
            motion, applied with the -CENSORTR option.
    
                e.g. 3dDeconvolve -CENSORTR `cat subjA_CENSORTR.txt`
    
            Which might expand to:
    
                     3dDeconvolve -CENSORTR '1:16..19,44 3:28 4:19,37..39'
    
            Note that the -CENSORTR option requires the text on the command line.
    
            This file is in the easily readable format applied with -CENSORTR.
            It has the same effect on 3dDeconvolve as the sister file from
            -write_censor, above.
    
       -verb LEVEL                  : set the verbosity level
    
    -----------------------------------------------------------------------------
    R Reynolds    March 2009
    =============================================================================
