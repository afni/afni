.. contents:: 
    :depth: 4 

************
unWarpEPI.py
************

.. code-block:: none

    Usage:   unWarpEPI.py -f run1+orig'[0..5]' -r blip_down+orig -d 'run1,run2' -a anat+orig -s unwarp_folder
    
    Routine to unwarp EPI data set using another data set with opposite polarity
    or B0 field map
    
    Options:
      -h, --help            show this help message and exit
      -f FORWARD, --forward=FORWARD
                            calibration matching data to be corrected
      -r REVERSE, --reverse=REVERSE
                            calibration with opposing polarity to data to be
                            corrected
      -a ANAT4WARP, --anat4warp=ANAT4WARP
                            reference anatomical data set
      -d DATA, --data=DATA  data to be corrected (same polarity as forward
                            calibration data). Separate with commas if specifying
                            multiple datasets. Do NOT put +orig at the end of
                            these dataset names, or the script will fail!
      -s SUBJID, --subjID=SUBJID
                            ID of subject to be corrected
      -g, --giant_move      Set giant_move option for align_epi_anat if final
                            align of anatomy to corrected EPI fails if datasets
                            are far apart in space.
    
    For questions, suggestions, information, please contact Vinai Roopchansingh,
