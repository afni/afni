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




|

.. _fp_postproc_dwitodt:

**fat_proc_dwi_to_dt** (and **@GradFlipTest**)
----------------------------------------------

