***************
uber_subject.py
***************

.. _uber_subject.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    ===========================================================================
    uber_subject.py               - graphical interface to afni_proc.py
    
    The expected use of this program is to simply run it without any options.
    That will start the graphical user interface (GUI), which has its own set
    of help and help tools.
    
            usage:  uber_subject.py
    
    ---
    
    This help describes only the command line options to this program, which
    enables one to:
    
            - run without the GUI
            - initialize subject variables in the GUI
            - initialize control variables for control of execution
            - pass PyQt4 options directly to the GUI
    
    ----------------------------------------------------------------------
    Examples:
    
       GUI examples:
    
          uber_subject.py
          uber_subject.py -qt_opts -style=motif
          uber_subject.py -svar sid FT -svar gid idiots  \
                          -svar anat FT_anat+orig.HEAD   \
                          -svar epi FT_epi_r*.HEAD       \
                          -svar stim AV*.txt             \
                          -svar stim_basis 'BLOCK(15,1)'
          uber_subject.py -cvar subj_dir my/subject/dir
    
       Informational examples:
    
          uber_subject.py -help
          uber_subject.py -help_gui
          uber_subject.py -help_install
          uber_subject.py -hist
          uber_subject.py -show_valid_opts
          uber_subject.py -show_default_vars
          uber_subject.py -todo
    
       Non-GUI examples (all have -no_gui):
    
          1. Akin to the GUI example, but use subject variables directly, not
             via -svar.
    
             uber_subject.py -no_gui -save_ap_command cmd.AP.1 \
                 -sid FT -gid horses                           \
                 -anat FT_anat+orig.HEAD -epi FT_epi_r*.HEAD   \
                 -stim AV*.txt -stim_basis 'BLOCK(15,1)'
    
          2. Process the EPI data as resting state analysis.
    
             Pass a subject ID, anat and EPI datasets, and # TRs to remove.
             Also, bandpass via 3dDeconvolve (while censoring), and regress
             motion derivatives (in addition to motion).
    
             uber_subject.py -no_gui -save_ap_command cmd.rest_state  \
                 -sid FT.rest -tcat_nfirst 2                          \
                 -anat FT/FT_anat+orig -epi FT/FT_epi_r*.HEAD         \
                 -regress_bandpass 0.01 0.1 -regress_mot_deriv yes
    
    ----------------------------------------------------------------------
    Note, for passing subject variables, use of -svar is safer then using
    variable names directly (e.g. "-svar stim AV*.txt" vs. "-stim AV*.txt"),
    because if there is a mistake in the variable name, it would be grouped
    with the previous variable.
    
    For example, compare these 2 versions of the same mistake:
    
            -svar stim stim_*.txt -svar eppppi EPI_r*.HEAD
       vs.
            -stim stim_*.txt      -eppppi EPI_r*.HEAD
    
    In the former case, there would be an error about epppi not being a
    valid variable.  But in the latter case, the program would not know
    that you mean -eppppi as a new variable, so -eppppi and the EPI*.HEAD
    files would be taken as more -stim inputs.
    
    In any case, passing variables this way is mostly available for my own
    evil purposes.  This is supposed to be a GUI after all...
    
    ----------------------------------------------------------------------
    
    OptionList: uber_subject.py options (len 55)
        -help                   : show this help
        -help_gui               : show help for GUI
        -help_howto_program     : help for programming
        -help_install           : show install notes
        -help_install_nokia     : Nokia install help
        -hist                   : show revision history
        -show_default_vars      : show variable defaults
        -show_valid_opts        : show all valid options
        -show_svar_dict         : show subject var dictionary
        -ver                    : show module version
        -verb                   : set verbose level
        -no_gui                 : do not open graphical interface
        -qt_opts                : pass the given options to PyQt
        -print_ap_command       : show afni_proc.py script
        -save_ap_command        : save afni_proc.py script
        -exec_ap_command        : run afni_proc.py command
        -exec_proc_script       : run proc script
        -cvar                   : set control variable to value
        -svar                   : set subject variable to value
        -align_cost             : specify cost function for anat/EPI alignment
        -align_giant_move       : yes/no: use -giant_move in AEA.py
        -align_opts_aea         : specify extra options for align_epi_anat.py
        -anal_domain            : set data domain (volume/rest)
        -anal_type              : set analysis type (task/rest)
        -anat                   : set anatomical dataset name
        -anat_has_skull         : yes/no: whether anat has skull
        -blocks                 : set list of processing blocks to apply
        -blur_size              : set blur size, in mm
        -compute_fitts          : yes/no: whether to just compute the fitts
        -epi                    : set list of EPI datasets
        -epi_wildcard           : yes/no: use wildcard for EPI dsets
        -get_tlrc               : yes/no: get any +tlrc anat dset
        -gid                    : set group ID
        -gltsym                 : specify list of symbolic GLTs
        -gltsym_label           : set corresponding GLT labels
        -motion_limit           : set per-TR motion limit, in mm
        -outlier_limit          : specify outlier limit for censoring
        -regress_GOFORIT        : set GOFORIT level in 3dDeconvolve
        -regress_bandpass       : specify bandpass limits to remain after regress
        -regress_jobs           : number of jobs to use in 3dDeconvolve
        -regress_mot_deriv      : yes/no: regress motion derivatives
        -regress_opts_3dD       : specify extra options for 3dDeconvolve
        -reml_exec              : yes/no: whether to run 3dREMLfit
        -run_clustsim           : yes/no: whether to run 3dClustSim
        -sid                    : set subject ID
        -stim                   : set list of stim timing files
        -stim_basis             : set basis functions for stim classes
        -stim_label             : set stim file labels
        -stim_type              : set stim types for stim classes
        -stim_wildcard          : yes/no: use wildcard for stim files
        -tcat_nfirst            : set number of TRs to remove, per run
        -tlrc_base              : specify anat for standard space alignment
        -tlrc_ok_maxite         : yes/no: pass -OK_maxite to @auto_tlrc
        -tlrc_opts_at           : specify extra options for @auto_tlrc
        -volreg_base            : set volreg base string (first/third/last)
    
    
    - R Reynolds  Feb, 2011
    ===========================================================================
