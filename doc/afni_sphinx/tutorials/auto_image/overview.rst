.. _tut_auto_overview:

Overview
========

.. contents::
   :depth: 3

*************************
Auto-image making in AFNI
*************************

Here we present examples of automatic image-making with AFNI (in the
sense of being command line callable and scriptable) using several
tools, such as:

* :ref:`@chauffeur_afni <tut_auto_@chauffeur_afni>` (:ref:`-help
  <ahelp_@chauffeur_afni>`)

* :ref:`@snapshot_volreg <tut_auto_@snapshot_volreg>` (:ref:`-help
  <ahelp_@snapshot_volreg>`)

* :ref:`@djunct_4d_imager <tut_auto_@djunct_4d_imager>`
  (:ref:`-help <ahelp_@djunct_4d_imager>`)

* :ref:`imcat <tut_auto_imcat>` (:ref:`-help <ahelp_imcat>`)

These programs do many of the kinds of things you would do to look at
datasets in the AFNI GUI (click buttons, turn on transparency, change
colorbar, set thresholds, change overlay/underlay, make montages,
etc.).  They can be quite useful for making figures, since this
process often involves tweaking small things and regenerating images
(thanks, Reviewer \#2!)-- it can save a *lot* of time to have an
adjustable script for making images.

While it is great to look at the datasets interactively, using
commands like these one can have images waiting for you to look at
various processing steps (like, alignment, ROI placement, distortion
correction, artifact detection, etc.).  In the era of big data (ooooh,
that buzzword!), you are still obligated to know about your analysis
steps if you don't want nasty (and hidden) surprises in your results.
These tools allow you to look at your data **systematically** across a
group, and that can be quite useful and powerful.

Each of these "star" programs either "drives" AFNI in a virtual
environment or just runs in normal memory, so they don't actually open
up AFNI separately and can be run on remote systems without any
special considerations.

The examples in this tutorial section use data sets that are publicly
available as part of the AFNI Bootcamp Demo set, freely downloadable
as described :ref:`here <Bootcamping>`.  Unless otherwise stated, the
data sets in the ``AFNI_data6/afni/`` directory are used.  The code
snippets are all in ``tcsh`` syntax.

Some definitions/abbreviations
------------------------------

**ulay**
  Underlay (dset). Typically grayscale.

**olay**
  Overlay (dset). Typically mapped to a colorbar to present results

**thr**
  Threshold (dset). For voxelwise thresholding-- used to determine
  whether an overlay voxel is seen or not.  Need *not* be the same
  dataset as "olay": in FMRI, thr and olay are often different; in
  DTI, one might set the same dset as olay and thr (e.g., FA map).

**coef** 
  The "coefficient" part of a statistical model regressor: the
  **effect estimate**.  If you have scaled your data during processing
  to have meaningful units (like "BOLD % signal change" for FMRI), as
  well as any response model (like a BLOCK, again in FMRI), then this
  should have physical, meaningful, interpretable and comparable
  units.  This is typically used to set the olay colors.

**stat** 
  The "statistic" part of a statistical model regressor.  When
  present, this kind of volume is typically used to threshold the olay
  data.  In both the AFNI GUI and on the commandline, there are
  functions/programs to convert a p-value to a statistic value, for
  convenience in thresholding.

