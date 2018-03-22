************
SurfDsetInfo
************

.. _ahelp_SurfDsetInfo:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: SurfDsetInfo [options] -input DSET1 -input DSET2 ...
       or: SurfDsetInfo [options] DSET1 DSET2 ... 
       Optional Params:
          -debug DBG: if DBG = 2, show dset->ngr in its entirety in NIML form.
    
      SUMA dataset input options:
          -input DSET: Read DSET1 as input.
                       In programs accepting multiple input datasets
                       you can use -input DSET1 -input DSET2 or 
                       input DSET1 DSET2 ...
           NOTE: Selecting subsets of a dataset:
                 Much like in AFNI, you can select subsets of a dataset
                 by adding qualifiers to DSET.
               Append #SEL# to select certain nodes.
               Append [SEL] to select certain columns.
               Append {SEL} to select certain rows.
               The format of SEL is the same as in AFNI, see section:
               'INPUT DATASET NAMES' in 3dcalc -help for details.
               Append [i] to get the node index column from
                          a niml formatted dataset.
               *  SUMA does not preserve the selection order 
                  for any of the selectors.
                  For example:
                  dset[44,10..20] is the same as dset[10..20,44]
                  Also, duplicate values are not supported.
                  so dset[13, 13] is the same as dset[13].
                  I am not proud of these limitations, someday I'll get
                  around to fixing them.
    
    
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
       Mar  7 2018
    
