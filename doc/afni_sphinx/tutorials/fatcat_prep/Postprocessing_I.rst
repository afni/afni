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

**@GradFlipTest**
-----------------

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
