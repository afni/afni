.. _class_recordings:


********************************
AFNI Bootcamp Lecture Recordings
********************************

.. contents::
   :depth: 3

******IN PROGRESS!*******

Overview
========

The current set of Bootcamp recordings, unless otherwise noted, were
recorded during the AFNI Bootcamp held at the NIH in October, 2017.  

They are organized by the day and title of the lecture according to
the schedule of that week, shown :download:`here
<media/AFNI_Oct2017_5day_schedule.pdf>`.

The videos provided here are screen captures with audio and toggleable
captioning.

Bootcamp Videos
===============

Monday
------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - AFNI Introduction (Cox)
     - `afni01_intro.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni01_intro.pdf>`_,
       `AFNI_interactive2.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/AFNI_interactive2.pdf>`_
     - AFNI datasets; image viewer; side controls
       (brightness/contrast, zoom); help; display panel; montage;
       multiple controllers
   * - AFNI Interactive (Glen)
     - `AFNI_interactive2.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/AFNI_interactive2.pdf>`_,
       `afni03_interactive.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni03_interactive.pdf>`_
     - graphing across time, overlay control (opacity and
       thresholding); clusterize plugin; other plugins; "driving" AFNI
       viewer from command line
   * - Single Subject Analysis (Cox)
     - 
     -
   * - Regression Hands-On (Reynolds)
     - 
     -


Tuesday
-------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - Alignment and Registration, pt1 (Glen)
     - `afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_
     - definitions and overview (rigid; linear affine; motion
       correction and regressors (3dvolreg); cross-modality (anat to
       epi); cost functions (esp. lpc and lpa); cross-modality (anat
       to epi; align_epi_anat.py); programs for visualization of
       alignment
   * - Alignment and Registration, pt1 (Glen)
     - `afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_
     - visualization in GUI; checking alignment quality; left-right
       flipping; alignment to standard space; non-human alignment;
       afni_proc.py alignment; nonlinear warping (3dQwarp);
       blip-up/down (EPI distortion) correction; atlas definitions;
       templates; @auto_tlrc
   * - FMRI Analysis: Start-to-Middle (Reynolds)
     - 
     -
   * - FMRI Analysis: Middle-to-End (Reynolds)
     - 
     -


Wednesday
---------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - Group Analysis in FMRI (Chen)
     - 
     -
   * - Atlases, ROIs & Driving AFNI, pt1 (Glen)
     -`afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_
     - standard space/template definitions; atlas definitions;
       nonlinear alignment to template; choosing a template/examples;
       coordinate order/systems; visualizing atlases; "whereami"
       function (GUI+command line); atlas GUI features ("Go to atlas"
       location; Atlas Colors); "other" atlases (pediatric, infant,
       cerebellum, macaque, marmoset, rat)
   * - Atlases, ROIs & Driving AFNI, pt1 (Glen)
     - `afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_,
       `afni11_roi.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni11_roi.pdf>`_,
       `afni11_roi_cmds.txt
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni11_roi_cmds.txt>`_
     - creating templates+atlases using AFNI; ROI definitions; methods
       to create ROIs (draw; clusterize; from atlas); AFNI draw
       plugin; resampling ROIs; extracting quantities from ROIs
       (averages, masks); clustering ROIs; ROIs from atlas regions;
       transforming between native subject and template spaces
   * - Group Analysis Hands-On (Chen)
     - 
     -
   * - Advanced Regression++ (Cox)
     - 
     -


Thursday
--------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - SUMA & Surface Analysis (Taylor)
     - `suma.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/suma.pdf>`_,
       `suma_keystrokes.txt
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/suma_keystrokes.txt>`_
     - Surface mapping in AFNI; what are surfaces/meshes; how to
       create surfaces for SUMA; how surface analysis complements
       volumetric analysis; SUMA data visualization (interactive).
   * - *More* SUMA & Surface Analysis (Reynolds)
     - 
     -
   * - Group Analysis Hands-On++ (Chen)
     - 
     -
   * - Resting State & InstaCorr (Cox)
     - 
     -


Friday
------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - FMRI Clustering (Cox)
     - 
     -
   * - DTI, FATCAT & more SUMA (Taylor)
     - `FATCAT_02_dti_tracking_intro.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_02_dti_tracking_intro.pdf>`_,
       `FATCAT_03_dti_tracking_funcs.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_03_dti_tracking_funcs.pdf>`_,
       `FATCAT_HO.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_HO.pdf>`_
     - stuff


Supplementary lectures
======================

The following lectures were recorded after the Bootcamp itself, but
mirror those lectures given during the "DTI Breakout" session on the
Monday of the Bootcamp.  At some point in the near(ish) future, we
hope to record the presentations given by the other DTI session
presenters, as well, and add them here.

DTI & FATCAT lectures
------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - Background on DWI and DTI (Taylor)
     - `FATCAT_01_background_dti.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_01_background_dti.pdf>`_
     - what is diffusion; how it is used in MRI to describe structure;
       geometry of DTI parameters; what are DW images; what noise and
       uncertainty features are in DWI/DTI data
   * - Introduction to DTI tracking (Taylor)
     - `FATCAT_02_dti_tracking_intro.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_02_dti_tracking_intro.pdf>`_
     - assumptions in DTI; important scales to understand; AFNI's
       tracking algorithm; different types ("modes") of tracking;
       basic terminology; making targets for tracking; what useful
       quantities to we get out; caveats+philosophical musings
   * - Practicalities for tractography in AFNI (Taylor)
     - `FATCAT_03_dti_tracking_funcs.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_03_dti_tracking_funcs.pdf>`_
     - motivating network-oriented paradigm; combining FMRI and DTI;
       FATCAT overview; comparing 3dTrackID modes, esp. probabilistic;
       options for tracking and target-making (with 3dROIMaker);
       @GradFlipTest to check grads; 3dDWUncert for estimating
       parameter uncertainty; extensions to HARDI and connectomes
   * - Investigating networks with multivariate modeling (Taylor)
     - `FATCAT_04_netw_stats_mvm.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_04_netw_stats_mvm.pdf>`_
     - taking tracking outputs for quantitative analysis; tracked
       results into the multivariate modeling (MVM) framework;
       fat_mvm_prep.py and fat_mvm_scripter.py to combine MRI and
       non-MRI data for modeling; example analysis from real study







