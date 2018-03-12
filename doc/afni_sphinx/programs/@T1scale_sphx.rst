********
@T1scale
********

.. _@T1scale:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @T1scale <-T1 T1vol> <-PD PDvol> 
    
    Fix bias field shading in T1 by scaling it with PD image.
    You can also get a decent result even without the PD volume.
    
       -T1 T1vol: The T1 volume
       -PD PDvol: The PD volume (aligned to T1)
       -odir ODIR: Directory where output gets dumped. 
                  Default is T1scale/ 
                  ODIR will contain multiple volumes with the one
                  of most interest being T1.uni+orig
    
             Script will reuse existing  volumes
    
       -align: Align PD volume to T1. Script assumes volumes are in
               close alignment. With this option, output PD+orig volume
               will be in alignment with T1+orig.
            Without this option, PDvol is assumed in alignment with T1vol
       -mask MVOL: Create mask for the output
             If not specified, the script will generate one with
             3dAutomask on fattened PDvol.
       -head_mask: Create mask using 3dSkullStrip's -head option.
       -unmasked_uni: Do not apply masking to uniformized volume (default)
                      You can mask the output after you decide which mask
                      is best. Here is an example with smask:
    
                      3dcalc -a T1.uni+orig. -b smask+orig. \
                             -expr 'step(b)*a' -prefix T1.uni.m
       -masked_uni: Apply masking to uniformized volume
    
       -echo: Set echo
       -help: this message
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
