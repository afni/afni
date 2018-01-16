****************
@NoisySkullStrip
****************

.. _@NoisySkullStrip:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage: @NoisySkullStrip <-input ANAT> 
                         [-keep_tmp] [-3dSkullStrip_opts OPTS]
    
    Strips the skull of anatomical datasets with low SNR
    You can recognize such dataset by the presence of relatively
    elevated (grayish) signal values outside the skull.
    
    This script does some pre-processing before running 3dSkullStrip
    If you're intrigued, read the code.
    
    This script is experimental and has only been tested on a dozen nasty
    datasets. So use it ONLY when you need it, i.e. when 3dSkullStrip 
    fails on its own and you have low SNR
    
    Examples of use:
       For a normal anatomy with low SNR
       @NoisySkullStrip -input anat+orig
    
       For an anatomy with lots of CSF and low SNR
       Note how 3dSkullStrip options are passed after -3dSkullStrip_opts
    @NoisySkullStrip  -input old_anat+orig \
                   -3dSkullStrip_opts \
                      -use_skull -blur_fwhm 1 -shrink_fac_bot_lim 0.4
    
    
    Mandatory parameters:
       -input ANAT : The anatomical dataset
    Optional parameters:
       -3dSkullStrip_opts SSOPTS: Anything following this option is passed
                                  to 3dSkullStrip
       -keep_tmp: Do not erase temporary files at the end.
    
    The script outputs the following:
       ANAT.ns  : A skull stripped version of ANAT
       ANAT.air and ANAT.skl: A couple of special masks
       ANAT.lsp : A volume that is used to threshold 'air'
                  out of the volume to be stripped.
                  @NoisySkullStrip tries to choose a threshold
                  automatically but fails at times. You can set
                  the threshold manually with -lspth and rerun
                  the script to try and get a better result
    
    Do send me feedback on this script's performance.
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
    Ziad S. Saad, March 28 08.
    saadz@mail.nih.gov
