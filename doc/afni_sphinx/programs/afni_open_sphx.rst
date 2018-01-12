.. contents:: 
    :depth: 4 

*********
afni_open
*********

.. code-block:: none

    
    A program to open various AFNI/SUMA files
    
      afni_open [OPTIONS] FILE1 [FILE2 ...]
    
    Examples:
      afni_open  xmat.1D.xmat
      afni_open -aw roi_11.pdf
      afni_open -r driv
    
    Options:
    ===========
      -w METHOD: Use METHOD to open FILES.
                 Acceptable values for METHOD are:
                 editor: Open with text editor.
                 downloader: Fetch with wget or curl.
                 browser: Open in browser
                 afni: Open with AFNI
                 suma: Open with SUMA
                 1dplot: Open with 1dplot
                 ExamineXmat: Open with ExamineXmat
                 iviewer: Open with image viewer
                 afniweb: Get from afni website.
                 readme: Search for appropriate README
                         This option is in the same spirit of 
                         apsearch -view_readme option. To see a list of
                         all readme files, run:
                         apsearch -list_all_afni_readmes
      -e: Same as -w editor
      -d: Same as -w downloader
      -x: Same as -w ExamineXmat
      -b: Same as -w browser
      -r: Same as -w readme
      -aw: Same as -w afniweb
    
         If no method is specifed, the program tries to guess
         from the filename.
    
      -global_help: Show help for global options.
      -gopts_help:  Show help for global options.
      -help: You're looking at it.
    
    Global Options:
    ===============
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
      
       -overwrite: Overwrite existing output dataset.
                   Equivalent to setting env. AFNI_DECONFLICT=OVERWRITE
       -ok_1D_text: Zero out uncommented text in 1D file.
                    Equivalent to setting env. AFNI_1D_ZERO_TEXT=YES
       -Dname=val: Set environment variable 'name' to value 'val'
                 For example: -DAFNI_1D_ZERO_TEXT=YES
       -Vname=: Print value of environment variable 'name' to stdout and quit.
                This is more reliable that the shell's env query because it would
                include envs set in .afnirc files and .sumarc files for SUMA
                programs.
                 For example: -VAFNI_1D_ZERO_TEXT=
       -skip_afnirc: Do not read the afni resource (like ~/.afnirc) file.
       -pad_to_node NODE: Output a full dset from node 0 to MAX_NODE-1
                       ** Instead of directly setting NODE to an integer you 
                          can set NODE to something like:
                       ld120 (or rd17) which sets NODE to be the maximum 
                          node index on an Icosahedron with -ld 120. See 
                          CreateIcosahedron for details.
                       d:DSET.niml.dset which sets NODE to the maximum node found
                          in dataset DSET.niml.dset.
                       ** This option is for surface-based datasets only.
                          Some programs may not heed it, so check the output if
                          you are not sure.
       -pif SOMETHING: Does absolutely nothing but provide for a convenient
                       way to tag a process and find it in the output of ps -a
       -echo_edu: Echos the entire command line to stdout (without -echo_edu)
                  for edification purposes
    
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
