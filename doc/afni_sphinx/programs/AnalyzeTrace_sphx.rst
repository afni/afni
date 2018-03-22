************
AnalyzeTrace
************

.. _AnalyzeTrace:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: A program to analyze SUMA (and AFNI's perhaps) stack output
           The program can detect functions that return with RETURN without
           bothering to go on the stack.
       AnaylzeTrace [options] FILE 
           where FILE is obtained by redirecting program's trace output.
    Optional Param:
       -max_func_lines N: Set the maximum number of code lines before a function
                          returns. Default is no limit.
       -suma_c: FILE is a SUMA_*.c file. It is analyzed for functions 
                that use SUMA_ RETURN 
                (typo on purpose to avoid being caught here) without ENTRY
           Note: The file for this program has special strings 
                (in comments at times)
                to avoid false alarms when processing it.
                
       -max_err MAX_ERR: Stop after encountering MAX_ERR errors
                         reported in log. Default is 5.
                         Error key terms are:
                         'Error', 'error', 'corruption'
    
       You should also search for the string: 'Note No RETURN or exit here'.
       Its occurrence can be an error at times.
    
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
    
