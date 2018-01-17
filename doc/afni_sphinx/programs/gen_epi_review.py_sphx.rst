*****************
gen_epi_review.py
*****************

.. _gen_epi_review.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    ===========================================================================
    gen_epi_review.py:
    
        This program will generate an AFNI processing script that can be used
        to review EPI data (possibly called @review_epi_data).
    
        The @review_epi_data script is meant to provide an easy way to quickly
        review the (preferably un-altered) EPI data.  It runs afni and then a
        looping set of drive_afni commands.
    
        Note that there should not be another instance of 'afni' running on
        the system when the script is run, as 'drive_afni' will communicate
        with only the first invoked 'afni' program.
    
        The most simple usage comes with the -dsets option, along with the
        necessary pieces of the gen_epi_review.py command.
    
    --------------------------------------------------
    examples:
    
        These examples assume the EPI dataset names produced as a result of
        the afni_proc.py processing script proc.sb23.blk, produced by the
        command in AFNI_data4/s1.afni_proc.block, provided with the class data.
    
        Yes, that means running the s1.afni_proc.block (tcsh) script to call
        the afni_proc.py (python) script to produce the proc.sb23.blk (tcsh)
        script, which calls the gen_epi_review.py (python) script to produce
        the @review_epi_data (tcsh) script, which can be run to review your EPI 
        data.  Ahhhhhhh...  :)
    
        Note that when using wildcards, the datasets must exist in the current
        directory.  But when using the {1,2,..} format, the files do not yet
        need to exist.  So command #2 could be run anywhere and still create the
        same script, no data needed.
    
        1. simple usage, just providing datasets (and general options)
    
            gen_epi_review.py -dsets pb00.sb23.blk.r??.tcat+orig.HEAD
    
        2. expand 5 runs with shell notation, rather than wildcards, and
           specify an alternate script name
    
            gen_epi_review.py -dsets pb00.sb23.blk.r{1,2,3,4,5}.tcat        \
                    -script @review_epi_5runs
    
        3. choose to see all three image windows
    
            gen_epi_review.py -dsets pb00.sb23.blk.r*.tcat+orig.HEAD        \
                    -windows sagittal axial coronal                         \
                    -script @review_epi_windows
    
        4. specify the graph size and position (can do the same for image windows)
    
            gen_epi_review.py -dsets pb00.sb23.blk.r*.tcat+orig.HEAD        \
                    -gr_size 600 450 -gr_xoff 100 -gr_yoff 200              \
                    -script @review_epi_posn
    
    ----------------------------------------------------------------------
    OPTIONS:
    ----------------------------------------------------------------------
    informational arguments:
    
        -help                       : display this help
        -hist                       : display the modification history
        -show_valid_opts            : display all valid options (short format)
        -ver                        : display the version number
    
    ----------------------------------------
    required argument:
    
        -dsets dset1 dset2 ...      : specify input datasets for processing
    
            e.g. -dsets epi_r*+orig.HEAD
    
            This option is used to provide a list of datasets to be processed
            in the resulting script.
    
    ----------------------------------------
    optional arguments:
    
        -script SCRIPT_NAME         : specify the name of the generated script
    
            e.g. -script review.epi.subj23
    
            By default, the script name will be '@' followed by the name used
            for the '-generate' option.  So when using '-generate review_epi_data',
            the default script name will be '@review_epi_data'.
    
            This '-script' option can be used to override the default.
    
        -verb LEVEL                 : specify a verbosity level
    
            e.g. -verb 3
    
            Use this option to print extra information to the screen
    
        -windows WIN1 WIN2 ...      : specify the image windows to open
    
            e.g. -windows sagittal axial
    
            By default, the script will open 2 image windows (sagittal and axial).
            This option can be used to specify exactly which windows get opened,
            and in which order.
    
            Acceptable window names are: sagittal, axial, coronal
    
    ----------------------------------------
    geometry arguments (optional):
    
        -im_size dimX dimY          : set image dimensions, in pixels
    
            e.g. -im_size 300 300
    
            Use this option to alter the size of the image windows.  This
            option takes 2 parameters, the pixels in the X and Y directions.
    
        -im_xoff XOFFSET            : set the X-offset for the image, in pixels
    
            e.g. -im_xoff 420
    
            Use this option to alter the placement of images along the x-axis.
            Note that the x-axis is across the screen, from left to right.
    
        -im_yoff YOFFSET            : set the Y-offset for the image, in pixels
    
            e.g. -im_xoff 400
    
            Use this option to alter the placement of images along the y-axis.
            Note that the y-axis is down the screen, from top to bottom.
    
        -gr_size dimX dimY          : set graph dimensions, in pixels
    
            e.g. -gr_size 400 300
    
            Use this option to alter the size of the graph window.  This option
            takes 2 parameters, the pixels in the X and Y directions.
    
        -gr_xoff XOFFSET            : set the X-offset for the graph, in pixels
    
            e.g. -gr_xoff 0
    
            Use this option to alter the placement of the graph along the x-axis.
            Note that the x-axis is across the screen, from left to right.
    
        -gr_yoff YOFFSET            : set the Y-offset for the graph, in pixels
    
            e.g. -gr_xoff 400
    
            Use this option to alter the placement of the graph along the y-axis.
            Note that the y-axis is down the screen, from top to bottom.
    
    
    - R Reynolds  June 27, 2008
    ===========================================================================
