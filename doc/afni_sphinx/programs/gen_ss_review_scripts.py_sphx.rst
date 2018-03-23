.. _ahelp_gen_ss_review_scripts.py:

************************
gen_ss_review_scripts.py
************************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    =============================================================================
    gen_ss_review_scripts.py - generate single subject analysis review scripts
    
            o  figure out basic details (sid, trs, xmat, censor stats files, etc.)
            o  generate an @ss_review_basic script to output simple details about
               this subject and results
            o  generate a @ss_review_driver script to actually inspect the results
               (running some commands by the user's control)
            o  generate @ss_review_driver_commands
               (same as @ss_review_driver, but a pure command file)
    
       Consider following this with gen_ss_review_table.py, after many/all
       subjects are analyzed.  For example:
    
          cd subject_results
          gen_ss_review_table.py -tablefile review_table.xls \
              -infiles group.*/subj.*/*.results/out.ss_review.*
    
    ------------------------------------------
    
       examples:
    
          1. Run this program without any options, assuming everything is there.
    
                    gen_ss_review_scripts.py
    
             Additional run the basic review script and then the drive script.
    
                    ./@ss_review_basic
                    ./@ss_review_driver
    
          2. Esoteric.  Set all the output file names (for now via control vars).
    
                    gen_ss_review_scripts.py              \
                            -cvar scr_basic ~/tmp/s.basic \
                            -cvar scr_drive ~/tmp/s.drive \
                            -cvar cmds_drive ~/tmp/s.cmds \
                            -cvar xstim ~/tmp/x.stim.1D
    
           2b. Similar to 2, but put all scripts and intermediate files under
               a new ~/tmp/gen_dir.  So as an example for testing:
    
                    mkdir ~/tmp/gen_dir
                    gen_ss_review_scripts.py -cvar out_prefix ~/tmp/gen_dir/
    
               Note that if out_prefix is a directory, it will need a trailing
               '/', since it is a file name prefix.
    
           2c. Simplified.  Use -prefix instead of -cvar out_prefix.
    
                    gen_ss_review_scripts.py -prefix test.
    
    ------------------------------------------
    
       required files/datasets (these must exist in the current directory):
    
          variable name        example file name
          -------------        -----------------
          tcat_dset            pb00.FT.r01.tcat+orig.HEAD
          outlier_dset         outcount_rall.1D
          enorm_dset           motion_FT_enorm.1D
          censor_dset          motion_FT_censor.1D
          motion_dset          dfile_rall.1D
          volreg_dset          pb02.FT.r01.volreg+tlrc.HEAD
          xmat_regress         X.xmat.1D
          final_anat           FT_anat+tlrc.HEAD
    
       optional files/datasets (censor files are required if censoring was done):
    
          mask_dset            full_mask.FT+tlrc.HEAD
          censor_dset          motion_FT_censor.1D
          sum_ideal            sum_ideal.1D
          stats_dset           stats.FT+tlrc.HEAD
          errts_dset           errts.FT.fanaticor+tlrc.HEAD
          xmat_uncensored      X.nocensor.xmat.1D
          tsnr_dset            TSNR.ft+tlrc.HEAD
          gcor_dset            out.gcor.1D
          mask_corr_dset       out.mask_ae_corr.txt
    
    ------------------------------------------
    
       terminal options:
    
          -help                     : show this help
          -help_fields              : show help describing fields from review_basic
          -help_fields_brief        : show only the brief field help
          -hist                     : show module history
          -show_uvar_dict           : show all user variables
          -show_uvar_eg             : show example of user variables
          -show_valid_opts          : list valid options
          -ver                      : show current version
    
       other options
    
          -exit0                    : regardless of errors, exit with status 0
          -prefix OUT_PREFIX        : set prefix for script names
          -verb LEVEL               : set the verbosity level
    
       options for setting main variables
    
          -subj SID                 : subject ID
          -rm_trs N                 : number of TRs removed per run
          -num_stim N               : number of main stimulus classes
          -motion_dset DSET         : motion parameters
          -outlier_dset DSET        : outlier fraction time series
          -enorm_dset DSET          : euclidean norm of motion params
          -mot_limit LIMIT          : (optional) motion limit - maybe for censoring
          -out_limit LIMIT          : (optional) outlier fraction limit
          -xmat_regress XMAT        : X-matrix file used in regression (X.xmat.1D)
          -xmat_uncensored XMAT     : if censoring, un-censored X-matrix file
          -stats_dset DSET          : output from 3dDeconvolve
          -final_anat DSET          : final anatomical dataset
          -final_view VIEW          : final view of data (e.g. 'orig' or 'tlrc')
    
          -cvar VAR PARAMS ...      : generic option form for control variables
          -uvar VAR PARAMS ...      : generic option form for user variables
    
    
    -----------------------------------------------------------------------------
    
    Here are some potential artifacts to ponder (just so they are saved
    somewhere), as noted by many of us, including D Glen and J Gonzalez.
    We can try to add to this list, and maybe even do something to take
    them off <gasp!>.
    
        1. Striping - across slices - EPI, anatomical
        2. Artifacts - checkerboard, ringing - EPI, anatomical
        3. Spiking (regional or global)
            - global would be caught in the outlier fractions
        4. Shifts in baseline (regional or global)
            - maybe @ANATICOR can help to deal with it, but how to notice?
        5. "PURE" on or off / acquisition protocol changes
        6. Poor contrast between CSF and WM/GM in EPI
        7. Low resolution anatomical data
        8. Noisy anatomical data
        9. Left-right flipping between anatomical and EPI
            - run align_epi_anat.py between flipped versions
              (as was done by _____ on the fcon_1000 data)
       10. Poor alignment between anatomical and EPI
            - currently users can view as part of @ss_review_driver
            - can use some large limit test on value from out.mask_overlap.txt
       11. Excessive motion
            - currently report average motion and censor details
       12. "Reshimming-like" shears between EPI volumes
       13. Non-uniformity because of surface coils
       14. Incorrect DICOM data
       15. Inconsistent data types within a study
       16. TR not properly set
       17. Missing data
       18. Inconsistent number of TRs within multiple EPI datasets
       19. Missing pre-steady state in EPI data
    
    -----------------------------------------------------------------------------
    
    Thanks to J Jarcho and C Deveney for suggestions, feedback and testing.
    
    R Reynolds    July 2011
    =============================================================================
