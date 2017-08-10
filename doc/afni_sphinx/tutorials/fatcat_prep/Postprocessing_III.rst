.. _fp_postpre_III:

Post-preproc, III: ROI making and prep
======================================

.. contents::
   :depth: 3

Overview
--------

The FreeSurfer (FS) defined regions may be useful for ROI-based
analysis or for tracking.  However, it should be noted that among the
GM regions can be non-local bits of general cortex that were not
specifically classified.  These come under the FS-labelling of
"Left-Cerebral-Cortex" (FS #3 and AFNI REN #2) and
"Right-Cerebral-Cortex" (FS #42 and AFNI REN #22) in both the "2000"
and "2009" parcellations.  These leftover-ish voxels don't really form
a solid region unto itself in the sense that most people would likely
want for any ROI-based analysis, even though they may indeed be in the
GM tissue mask.  So, in order to prepare for analyses where one just
wants localized regions, we demonstrate de-selecting these regions
from the GM maps in DT space that were created using
``fat_proc_map_to_dti`` in the step, ":ref:`postpre_II`".

Additionally, specifically in preparation for tracking, one might want
to inflate the GM regions, which we consider "targets" for the
tracking function, in a controlled way: 

* they can extend *up to*, but not too much into, DTI-defined WM (which,
  for healthy adult humans, is usually determined by where FA>0.2, in
  a standard proxy)

* they can extend *up to*, but not into, any other GM regions

The regions can be inflated with these properties by the AFNI-FATCAT
function ``3dROIMaker``, which is described in more detail (including
other options for controlling the ROIs) :ref:`HERE <Making_ROIs>`.


ROI selection and **3dROIMaker**
--------------------------------

.. note:: At some point there may be a separate function created for
          combining these capabilities.  For the present, we just
          describe one example of how the GM regions can be selected
          and then inflated.

**Proc:**

run::

    # for I/O, same path variable as above
    set odir    = "$path_P_ss/dwi_05"

    # for path/variable simplicity, just move to that directory
    cd $odir

    # list all (= 2, probably) renumbered GM maps
    set ren_gm  = `ls indt_*_REN_gm.nii.gz`

    # supplementary files used in ROI process: FA map and WB mask
    set wmfa    = "dt_FA.nii.gz"
    set wbmask  = "dwi_mask.nii.gz"

    # process each GM map
    foreach gm_in ( $ren_gm )

        set gm_sel = "sel_${gm_in}"

        # apply the ROI (de)selector
        3dcalc                                       \
            -echo_edu                                \
            -a $gm_in                                \
            -expr 'a*not(equals(a,2)+equals(a,22))'  \
            -prefix $gm_sel

        # keep the labeltables from the original
        3drefit                                      \
            -copytables                              \
            $gm_in                                   \
            $gm_sel

        set gm_roi = "${gm_sel:gas/.nii.gz//}"

        # inflate GM ROIs up to (but not overlapping) the FA-WM
        3dROIMaker                                   \
            -inset    $gm_sel                        \
            -refset   $gm_sel                        \
            -skel_stop_strict                        \
            -skel_thr 0.2                            \
            -wm_skel  $wmfa                          \
            -inflate  1                              \
            -neigh_upto_vert                         \
            -mask     $wbmask                        \
            -prefix   $gm_roi                        \
            -nifti   

    end

