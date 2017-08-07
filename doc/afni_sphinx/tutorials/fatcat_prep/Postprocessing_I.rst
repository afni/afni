.. _fp_postpre_I:

Post-preproc, I: tensor estimation and checking
===============================================

.. contents::
   :depth: 3

Overview
--------

Now we want to estimate the diffusion tensors (DTs) and DT parameters.
There are additional considerations:

* After TORTOISE processing, the DWIs overlay on the reference
  anatomical, but this volume itself generally has a different grid,
  (resolution, origin and orientation) from the originally input
  volume (where other things like a T1w volume, etc. might already
  have been aligned).  Therefore, too keep our volumes overlaying as
  much as possible, we move both the reference *structural.nii* and
  DWI volume to overlay the originally input T2w reference volume.
  
* There is a fun phenomenon in DWI acquisition + processing, that
  scanner vendors and various softwares somehow often have different
  meanings to gradient orientations along a volume's FOV: for example,
  for the *x*\-coordinate, is a positive value in the right or left
  direction? (and so forth for each component).  This is known as the
  "gradient flipping" problem, and I have whined about it voluminously
  in ":ref:`FlippingGrads`".  Here, we briefly discuss using
  ``@GradFlipTest`` to check things as part of a pipeline and
  demonstrate how it can be incorporated into scripts (with cautionary
  notes for doing so!).

* We might find it interesting to estimate the uncertainty of some DTI
  parameters in preparation for either full probabilistic or
  mini-probabilistic tracking.  (If the implications of either of
  terms are unfamiliar, then perhaps check out more information about
  FATCAT tracking options in ":ref:`Tracking`".

* On a mundane note, the physical scale of many scalar DTI parameters
  is pretty small, for example having :math:`{\rm MD} \approx 0.0007~
  {\rm mm}^2~{\rm s}^{-1}`.  Having such tiny numbers can lead to
  annoyances during analysis, for example with their even smaller
  differences and floating point precision.  Therefore, instead of
  having "small number" in units of :math:`{\rm mm}^2~{\rm s}^{-1}`,
  we find it more convenient to have "number of order unity" in units
  of ":math:`10^{-3}\,{\rm mm}^2~{\rm s}^{-1}`", which is totally legit,
  too.

|

.. _fp_postproc_@gradfliptest:

**@GradFlipTest** note
----------------------

The primary documentation for ``@GradFlipTest`` is provided on this
page, ":ref:`GradFlipTest`".

In brief, if you do not know whether and/or how the gradients in your
data set should be "flipped" in order to be consistent with processing
in AFNI (every software and scanner/vendor might be using different
systems, oddly enough), then you can use ``@GradFlipTest`` to guess
what is right.  The tool uses whole brain tracking under all possible
gradient flip configurations to hypothesize a relevant flip (including
no-flip) for the data set at hand.  It also produces commands to view
your results in SUMA to double check.  Note the caveats with blindly
including this in scripts without checking (-> though, in truth,
*every* step should be checked without blindly trusting!).

In the usage example of ``fat_proc_dwi_to_dt``, below, we show how
``@GradFlipTest`` can be incorporated into a pipeline directly.

.. _fp_postproc_dwitodt:

**fat_proc_dwi_to_dt** (and **@GradFlipTest**)
----------------------------------------------

At the end of the DWI preprocessing step, there may be several data
sets that are in various "spaces" (i.e., with different grids), and,
particularly if non-AFNI software is being used, things like the FOV,
origin and orientation of data may have been changed.  In short, there
may be several data sets that 1) don't overlap with each other and 2)
don't overlap with the data originally entered, even though they
probably should. This happens, for example, when using TORTOISE (see
the description of outputs in this section,
":ref:`fp_preproc_tort_drbuddi`"; it similarly happens when only
``DIFFPREP`` is used, as well).  Esp. since TORTOISE aligns DWIs to
the reference anatomical volume, there is no reason that the data
shouldn't overlay with the anatomical originally entered (and to which
other data, such as a T1w volume and FreeSurfer output, may also be
aligned).  For convenience, ``fat_proc_dwi_to_dt`` can help the
processed data sets align with the original anatomical (to the degree
that the data itself allows) by simply adjusting the origins and
orientations.

During the tensor fitting stage, several useful options are switched
on for ``3dDWItoDT``:

* using nonlinear alignment via ``-nonlinear``, essentially default
  for primary tensor fitting

* performing an extra round of tensor fitting with ``-reweight``\ing,
  and the cumulative weight values are output with ``-cumulative_wts``

* scaling physical measures conveniently via ``-scale_out_1000``, so
  numerical values aren't tiny (for adult humans)-- units are in
  ":math:`10^{-3}\,{\rm mm}^2~{\rm s}^{-1}`"

* calculating chi-square results via ``-debug_briks``, for
  goodness-of-(tensor)-fit measures

* masking via the anatomical, which hopefully has more "clarity" and
  less brightness inhomogeneity than the DWI volumes themselves

Additionally, uncertainty values of DTI parameters are calculated with
``3dDWUncert``, for use with tracking in ``3dTrackID``.

QC images are also output. The DWI :math:`b_0` volume is shown
overlayed and edge-ified on the reference anatomical are made, as well
as thresholded FA maps (FA>0.2, for the standard proxy of DTI-defined
WM maps in healthy adult humans; alterable for other cases).

**Proc:** Processing in this case involves two steps: A) use the DWIs
*b*\-matrix gradient info to estimate what flip is necessary to make
those data pieces consistent for AFNI; and B) make the DWI output
overlay on the original input T2w volume well, and then add in the
*b*\-matrix (with any necessary flip) to estimate the DT and
associated parameters.  Step "B" involves using both the pre- and
post-TORTOISE anatomical reference file; NB: one doesn't *have* to
include these anatomical files, but if you *are* using TORTOISE (as we
are here) then you really should. Also, masking of DWIs to find the
brain can be problematic due to brightness inhomogeneities, etc., so
using the structural image for this aspect is also beneficial (though
not guaranteed to be perfect).  Therefore, one can run::

    # I/O path, same as above, following earlier steps
    set path_P_ss = data_proc/SUBJ_001

    # shortcut names for what will be our input (-> from TORT proc)
    # and output (-> another dwi_* directory)
    set itort = $path_P_ss/dwi_04
    set odir  = $path_P_ss/dwi_05

    # A) do autoflip check: not ideal to need this, but such is life
    @GradFlipTest \
        -in_dwi       $itort/buddi.nii        \
        -in_col_matT  $itort/buddi.bmtxt      \
        -outdir       "$itort"                \
        -prefix       GradFlipTest_rec.txt

    # get the 'recommended' flip; still should verify visually!!
    set my_flip = `cat $itort/GradFlipTest_rec.txt`

    # B) DT+parameter estimates, with flip chosen from @GradFlipTest
    fat_proc_dwi_to_dt \
        -in_dwi       $itort/buddi.nii                    \
        -in_col_matT  $itort/buddi.bmtxt                  \
        -in_struc_res $itort/structural.nii               \
        -in_ref_orig  $path_P_ss/anat_01/t2w.nii          \
        -prefix       $odir/dwi                           \
        -mask_from_struc                                  \
        $my_flip

-> putting the flip-test files and directory into the
existing+populated 'data_proc/SUBJ_001/dwi_04/', and the DT and
parameters into 'data_proc/SUBJ_001/dwi_05/':

.. list-table:: 
   :header-rows: 1
   :widths: 90

   * - Directory substructure for example data set
   * - .. image:: media/postpre_i/fp_10_gradflip_and_dwitodt.png
          :width: 100%
          :align: center
   * - *Output from @GradFlipTest and fat_proc_dwi_to_dt.*

**Part A: @GradFlipTest output.** Note that only the last three files
in '\*/dwi_04/' (Grad\* and _tmp\*/) were made by @GradFlipTest.

.. list-table:: 
   :header-rows: 1
   :widths: 20 80
   :stub-columns: 0

   * - Outputs of
     - @GradFlipTest
   * - **GradFlipTest_rec_echo.txt**
     - textfile, with a record exact command that was run at the top;
       importantly, it also contains the number of tract counts that
       lead to the program's flip guess as well as ``suma`` commands
       to view the outputs in the temp directory (which is saved by
       default), so users can check things for themselves.
   * - **GradFlipTest_rec.txt**
     - textfile, simply the "best guess" of flip (one of: ``-flip_x``,
       ``-flip_y``, ``-flip_z`` or ``-no_flip``) which could be
       incorporated into another script.
   * - **_tmp_TESTFLIP/**
     - "working" directory for the script, but also contains the
       tracked outputs for each tested flip; the user can use command
       calls from the "Grad\*echo.txt" file to view the relative whole
       brain trackings and thereby judge whether the function's guess
       should be used or not.

|

.. list-table:: 
   :header-rows: 1
   :widths: 90

   * - Contents of the GradFlipTest_rec\*.txt text files
   * - .. image:: media/postpre_i/fp_Grad_txtfile_output.png
          :width: 100%
          :align: center
   * - *Text file content displayed in the terminal. The ``suma``
       commands in Grad\*echo\*.txt file can be copy+pasted into
       terminal to check the believability of results visually.*

|

.. list-table:: 
   :header-rows: 1
   :widths: 50 50

   * - Examples of using ``suma`` to check results
     - (looking at 2/4 flip cases tested)
   * - ``suma ...`` output for **no flip** (cor and axi views).
     - ``suma ...`` output for **flip z** (cor and axi views).
   * - .. image:: media/postpre_i/autorecord.A.170731_153458.876.jpg
          :width: 100%   
          :align: center
     - .. image:: media/postpre_i/autorecord.A.170731_153352.894.jpg
          :width: 100%   
          :align: center
   * - .. image:: media/postpre_i/autorecord.A.170731_153508.661.jpg
          :width: 100%   
          :align: center
     - .. image:: media/postpre_i/autorecord.A.170731_153405.758.jpg
          :width: 100%   
          :align: center
   * - Whole brain tracking results for 'no flip'-- some flaws in
       expected tracking results: corpus callosum missing, not a lot
       of cortical-spinal tracts, missing corticocortical connections,
       etc.  Badness due to "flip" of grads being wrong for AFNI.
     - Whole brain tracking results for 'flip z'-- looks pretty much
       like expected for whole brain tracking-- see corpus callosum,
       full coverage of cortex, etc.

.. note:: Something that can happen at this step to be aware of: in
          ``@GradFlipTest``, the default method for making the whole
          brain mask within which to perform tracking is simple
          automasking of the DWI's [0]th volume.  This may not be
          great, both missing out parts of the brain (esp. if there
          are large brightness inhomogeneities across the volume) or
          including skull, non-brain tissue etc. depending on the
          image.  A separate mask could be made by the user an input,
          if necessary.

          If you look closely, you can see that this has even happened
          in the above case: in the cor view, a gap in tracts is
          visible inferior to the corpus callosum.  This occured
          because the volume for automasking has large dark regions.
          However, the remaining mask provided enough coverage for
          guessing what flip would be appropriate. Such are the
          judgments users need to make-- and *another* reason to look
          at your data!!

|

**Part B: fat_proc_dwi_to_dt.** All of the output from
``fat_proc_dwi_to_dt`` in '\*/dwi_05/' should overlay the original T2w
reference that was input into TORTOISE.
