.. _fp_prepre_II:

Pre-preprocessing, II
=====================

.. contents::
   :depth: 3

Overview
--------







.. _fp_select_vols:

**fat_proc_select_vols**: GUI to select+record baddies
------------------------------------------------------

**Proc:**: run::


    # I/O path, same as before
    set path_P_ss = data_proc/SUBJ_001

    fat_proc_select_vols  \
        -in_dwi  $path_P_ss/dwi_00/ap.nii.gz                     \
        -in_img  $path_P_ss/dwi_00/ap_sepscl.sag.png             \
        -prefix  $path_P_ss/dwi_01/dwi_sel_ap

    echo "\n\n++  Select out PA DWIs!\n\n"
    fat_proc_select_vols  \
        -in_dwi  $path_P_ss/dwi_00/pa.nii.gz                     \
        -in_img  $path_P_ss/dwi_00/pa_sepscl.sag.png             \
        -in_bads $path_P_ss/dwi_01/dwi_sel_ap_bads.txt           \
        -prefix  $path_P_ss/dwi_01/dwi_sel_both


.. _fp_filter_dwis:

**fat_proc_filter_dwis**: apply filter to keep goodies
------------------------------------------------------

**Proc:**: run::

    # I/O path, same as before
    set path_P_ss = data_proc/SUBJ_001

    # the string of *good* volumes after selecting *bads*
    set selstr = `cat $path_P_ss/dwi_01/dwi_sel_both_goods.txt`

    # ---------- filter from both AP and PA dwis ---------
    fat_proc_filter_dwis  \
        -in_dwi        $path_P_ss/dwi_00/ap.nii.gz   \
        -in_col_matT   $path_P_ss/dwi_00/ap_matT.dat \
        -select        "$selstr"                     \
        -prefix        $path_P_ss/dwi_02/ap

    ../NEWFAT_FUNCTIONS/fat_proc_filter_dwis  \
        -in_dwi        $path_P_ss/dwi_00/pa.nii.gz   \
        -in_col_matT   $path_P_ss/dwi_00/pa_matT.dat \
        -select        "$selstr"                     \
        -prefix        $path_P_ss/dwi_02/pa
