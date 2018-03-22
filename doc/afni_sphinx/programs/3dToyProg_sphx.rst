*********
3dToyProg
*********

.. _ahelp_3dToyProg:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dToyProg [-prefix PREF] [-mask MSET] [-datum DATUM] 
                     [-h|-help] <-input ISET>
       A program to illustrate dataset creation, and manipulation in C using
       AFNI's API. Comments in the code (should) explain it all.
    
     -input ISET: reference dataset 
     -prefix PREF: Prefix of output datasets. 
     -mask MSET: Restrict analysis to non-zero voxels in MSET
     -datum DATUM: Output datum type for one of the datasets.
                   Choose from 'float' or 'short'. Default is
                   'float'
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
     
    
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
