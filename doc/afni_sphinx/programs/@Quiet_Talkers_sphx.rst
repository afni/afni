**************
@Quiet_Talkers
**************

.. _@Quiet_Talkers:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    A script to find and kill AFNI processes
    ps is used to lookfor processes running certain AFNI programs
    (default list: afni 3dGroupInCorr plugout_drive suma DriveSuma 3dSkullStrip SurfSmooth)
    with certain command line options
    
       @Quiet_Talkers [-sudo] [-prog PROG]
                     [-npb_val NV] [-npb_range NV0 NV1]
                     [-pif KEY_STRING] [-no_npb]
                     [-list] [-quiet]
    
    OPTIONS
     -sudo: Invoke higher powers to kill processes that you do not own
     -prog PROG: Instead of the default program list, only kill PROG
                 You can use multiple -prog options
     -npb_val NV: Kill those programs using NIML port block NV
     -npb_range NV0 NV1: Kill those using NIML port blocks between 
                         NV0 and NV1
     -pif KEY_STRING: Kill those programs that have a string matching
                      KEY_STRING in their commandline.
                      Most AFNI programs allow for a -pif KEY_STRING
                      option that does nothing but serve a process
                      identification purpose
     -no_npb: Kill any program in the list regardless of -npb options
              or -pif
     -list: Just list process numbers, don't run kill command
     -quiet: Do it quietly
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
    
    Examples:
       To kill all programs in list that used the -npb option
       @Quiet_Talkers
    
       To kill all those with either -npb 3 or 6
       @Quiet_Talkers -npb_val 3 -npb_val 6
    
       To kill all those with -npb values in the range 5..9
       @Quiet_Talkers -npb_range 5 9
    
       To restrict the search to certain programs only:
       @Quiet_Talkers -prog suma -prog afni -npb_range 5 9
    
    General purpose destruction:
    You can also kill process that have a certain string in the
    command line. Usually such commands are flagged with the 
    hidden AFNI option -pif.
    Example:
       suma -pif SOME_KEY_STRING &
       @Quiet_Talkers -prog suma -pif SOME_KEY_STRING
    
    Note that with -pif, the npb options are disabled.
    
    Say you want to kill any 'afni'
       @Quiet_Talkers -prog afni -pif ' '
     or 
       @Quiet_Talkers -prog afni -no_npb
    
    Ziad S. Saad   saadz@mail.nih.gov
