.. _ahelp_3dBRAIN_VOYAGERtoAFNI:

*********************
3dBRAIN_VOYAGERtoAFNI
*********************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dBRAIN_VOYAGERtoAFNI <-input BV_VOLUME.vmr> 
                                 [-bs] [-qx] [-tlrc|-acpc|-orig] [<-prefix PREFIX>]
     
     Converts a BrainVoyager vmr dataset to AFNI's BRIK format
     The conversion is based on information from BrainVoyager's
     website: www.brainvoyager.com. 
     Sample data and information provided by 
      Adam Greenberg and Nikolaus Kriegeskorte.
    
      If you get error messages about the number of
     voxels and file size, try the options below.
     I hope to automate these options once I have
     a better description of the BrainVoyager QX format.
    
      Optional Parameters:
      -bs: Force byte swapping.
      -qx: .vmr file is from BrainVoyager QX
      -tlrc: dset in tlrc space
      -acpc: dset in acpc-aligned space
      -orig: dset in orig space
      If unspecified, the program attempts to guess the view from
      the name of the input.
       [-novolreg]: Ignore any Rotate, Volreg, Tagalign, 
                    or WarpDrive transformations present in 
                    the Surface Volume.
       [-noxform]: Same as -novolreg
       [-setenv "'ENVname=ENVvalue'"]: Set environment variable ENVname
                    to be ENVvalue. Quotes are necessary.
                 Example: suma -setenv "'SUMA_BackgroundColor = 1 0 1'"
                    See also options -update_env, -environment, etc
                    in the output of 'suma -help'
      Common Debugging Options:
       [-trace]: Turns on In/Out debug and Memory tracing.
                 For speeding up the tracing log, I recommend 
                 you redirect stdout to a file when using this option.
                 For example, if you were running suma you would use:
                 suma -spec lh.spec -sv ... > TraceFile
                 This option replaces the old -iodbg and -memdbg.
       [-TRACE]: Turns on extreme tracing.
       [-nomall]: Turn off memory tracing.
       [-yesmall]: Turn on memory tracing (default).
      NOTE: For programs that output results to stdout
        (that is to your shell/screen), the debugging info
        might get mixed up with your results.
    
    
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
    
    
    Compile Date:
       Mar 22 2018
    
