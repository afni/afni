.. _class_recordings:

************************************
**AFNI Bootcamp Lecture Recordings**
************************************

.. contents::
   :depth: 3

******IN PROGRESS!*******

Overview
========

The current set of Bootcamp recordings, unless otherwise noted, were
recorded during the AFNI Bootcamp held at the NIH in October, 2017.
The videos provided here are screen captures with audio and toggleable
captioning. They are organized by the day and title of the lecture
according to the schedule of that week, shown :download:`here
<media/AFNI_Oct2017_5day_schedule.pdf>`.

For each lecture, links to accompanying PDF/TXT files are provided,
which are either directly used or contain records of the information
presented (particularly for visualization-based presentations using
the AFNI and SUMA GUIs).  Additional files that might are relevant,
though perhaps not directly used in the presentations, are also
provided in parentheses.

The Bootcamp demo data directory can be downloaded by following
:ref:`these "Boot up" instructions <install_bootcamp>`.  We also
strongly recommend that people unfamiliar with scripting take a quick
tour through the handy :ref:`Linux tutorial <U_all>`.


.. _class_recordings_bootcamp:

Bootcamp Videos
===============

.. _class_recordings_mon:

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
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/AFNI_interactive2.pdf>`_,
       `QuickTasks.pptx.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/QuickTasks.pptx.pdf>`_
     - AFNI datasets; image viewer; side controls
       (brightness/contrast, zoom); help; display panel; montage;
       multiple controllers
   * - `AFNI Interactive (Glen), Pt 1
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day1/AFNI_Interactive_DG_Pt1.mp4>`_,
       `Pt 2
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day1/AFNI_Interactive_DG_Pt2.mp4>`_
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
   * - `Regression Hands-On (Reynolds), Pt 1
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day1/HandsOn_Regression_RR_Pt1.mp4>`_, 
       `Pt 2
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day1/HandsOn_Regression_RR_Pt2.mp4>`_ 
     - `afni05_regression.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni05_regression.pdf>`_
     - hands-on: preprocessing overview and simple linear regression example


.. _class_recordings_tues:

Tuesday
-------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - `Alignment and Registration, pt1 (Glen)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day2/Alignment_Registration_DG_Pt1.mp4>`_, 
     - `afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_
     - definitions and overview (rigid; linear affine; motion
       correction and regressors (3dvolreg); cross-modality (anat to
       epi); cost functions (esp. lpc and lpa); cross-modality (anat
       to epi; align_epi_anat.py); programs for visualization of
       alignment
   * - `Alignment and Registration, pt2 (Glen)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day2/Alignment_Registration_DG_Pt1.mp4>`_, 
     - `afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_
       (`3dQwarp.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/3dQwarp.pdf>`_)
     - visualization in GUI; checking alignment quality; left-right
       flipping; alignment to standard space; non-human alignment;
       afni_proc.py alignment; nonlinear warping (3dQwarp);
       blip-up/down (EPI distortion) correction; atlas definitions;
       templates; @auto_tlrc
   * - `FMRI Analysis: Start-to-Middle (Reynolds)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day2/FMRI_Analysis_STM_RR_Day2_Pt1.mp4>`_, 

     - `afni16_start_to_finish.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni16_start_to_finish.pdf>`_
     - hands-on: afni_proc.py; detailed analysis (preprocessing through linear
       regression) of FT_analysis data; looking at data
   * - `FMRI Analysis: Middle-to-End (Reynolds)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/Day2/FMRI_Analysis_STM_RR_Day2_Pt2.mp4>`_, 
     - `afni16_start_to_finish.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni16_start_to_finish.pdf>`_
     - continuation of Start-to-Middle; @ss_review_driver
   * - Exercises and Consultations (class) 
     - `afni19_jazz.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni19_jazz.pdf>`_,
       `afni19_jazz_hints.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni19_jazz_hints.pdf>`_,
       `afni19_jazz_answers.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni19_jazz_answers.pdf>`_,
     - *This session does not contain any lecture; students can ask
       questions, and/or work on the AFNI Jazzercise questions, which
       are provided here.  These PDFs are meant to help new users gain
       familiarity with some AFNI functions; feel free to use the
       "hints" and "answers" for assistance!*


.. _class_recordings_wedn:

Wednesday
------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - Group Analysis in FMRI (Chen)
     - `afni24_GroupAna.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni24_GroupAna.pdf>`_
     - basic concepts and terminologies in group analysis;
       group analysis approaches: t-tests, GLM, ANOVA, ANCOVA, LME;
       miscellaneous issues: centering, intraclass correlation,
       inter-subject correlation
   * - Atlases, ROIs & Driving AFNI, pt1 (Glen)
     - `afni10_volreg_talairach.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni10_volreg_talairach.pdf>`_
     - standard space/template definitions; atlas definitions;
       nonlinear alignment to template; choosing a template/examples;
       coordinate order/systems; visualizing atlases; "whereami"
       function (GUI+command line); atlas GUI features ("Go to atlas"
       location; atlas colors); "other" atlases (pediatric, infant,
       cerebellum, macaque, marmoset, rat)
   * - Atlases, ROIs & Driving AFNI, pt2 (Glen)
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
     - `afni25_GroupAna_HO.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni25_GroupAna_HO.pdf>`_
     - Bayesian Multilevel Modeling; 
       available group analysis programs in AFNI: 3dttest++, 3dMEMA,
       3dANOVAx, 3dMVM, 3dLME;
       a few hands-on group analysis examples
   * - Advanced Regression++ (Cox)
     - a
     - b

.. _class_recordings_thurs:

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
     - `suma.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/suma.pdf>`_,
     - hands-on: SUMA continuation; surface ROI drawing and mapping to volume;
       complete single subject surface-based analysis with afni_proc.py;
       viewing effects of processing on surface data
   * - Group Analysis Hands-On++ (Chen)
     - `afni25_GroupAna_HO.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni25_GroupAna_HO.pdf>`_
     - available group analysis programs in AFNI: 3dttest++, 3dMEMA,
       3dANOVAx, 3dMVM, 3dLME;
       a few hands-on group analysis examples
   * - Resting State & InstaCorr (Cox)
     - 
     -

.. _class_recordings_fri:

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
     - 
     - *see the "DTI & FATCAT videos" section, and in particular the
       "FATCAT Hands-On Demo" lecture, below.*


.. _class_recordings_suppl:

Supplementary lectures
======================

The following lectures were recorded after the Bootcamp itself, but
mirror those lectures given during the "DTI Breakout" session on the
Monday of the Bootcamp.  At some point in the near(ish) future, we
hope to record the presentations given by the other DTI session
presenters, as well, and add them here; at present, some brief notes
by the TORTOISE group on EPI distortions in DTI and motivation for
using TORTOISE to correct them are provided here: `TORTOISE_Okan.pdf
<https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/TORTOISE_Okan.pdf>`_.

For the "Hands-On" demo viewing, it helps to have gone through the
first SUMA session from :ref:`Thursday, above
<class_recordings_thurs>`.


.. _class_recordings_fatcat:

DTI & FATCAT videos
-------------------

.. list-table:: 
   :header-rows: 1
   :widths: 20 30 50
   :stub-columns: 0

   * - Video
     - Associated handouts
     - Contents, notes
   * - `Background on DWI and DTI (Taylor)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/FATCAT/FATCAT_01.mp4>`_
     - `FATCAT_01_background_dti.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_01_background_dti.pdf>`_
     - what is diffusion; how it is used in MRI to describe structure;
       geometry of DTI parameters; what are DW images; what noise and
       uncertainty features are in DWI/DTI data
   * - `Introduction to DTI tracking (Taylor)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/FATCAT/FATCAT_02.mp4>`_
     - `FATCAT_02_dti_tracking_intro.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_02_dti_tracking_intro.pdf>`_
     - assumptions in DTI; important scales to understand; AFNI's
       tracking algorithm; different types ("modes") of tracking;
       basic terminology; making targets for tracking; what useful
       quantities to we get out; caveats+philosophical musings
   * - `Practicalities for tractography in AFNI (Taylor)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/FATCAT/FATCAT_03.mp4>`_
     - `FATCAT_03_dti_tracking_funcs.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_03_dti_tracking_funcs.pdf>`_
     - motivating network-oriented paradigm; combining FMRI and DTI;
       FATCAT overview; comparing 3dTrackID modes, esp. probabilistic;
       options for tracking and target-making (with 3dROIMaker);
       @GradFlipTest to check grads; 3dDWUncert for estimating
       parameter uncertainty; extensions to HARDI and connectomes
   * - `Investigating networks with multivariate modeling (Taylor)
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/FATCAT/FATCAT_04.mp4>`_
     - `FATCAT_04_netw_stats_mvm.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_04_netw_stats_mvm.pdf>`_
     - taking tracking outputs for quantitative analysis; tracked
       results into the multivariate modeling (MVM) framework;
       fat_mvm_prep.py and fat_mvm_scripter.py to combine MRI and
       non-MRI data for modeling; example analysis from real study
   * - `FATCAT Hands-On Demo (Taylor) [first half only, at the moment]
       <https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2017-10-NIH/FATCAT/FATCAT_05_HandsOnDemo.mp4>`_
     - `FATCAT_05_HO.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/FATCAT_HO.pdf>`_,
       `suma.pdf
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/suma.pdf>`_,
       `suma_keystrokes.txt
       <https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/suma_keystrokes.txt>`_
     - Overview of FATCAT Demo; visualization of DTI (+FMRI) data
       using AFNI and SUMA; viewing tractography output (solo, with
       surfaces, with ROIs, with FMRI data); making ROIs from FMRI;
       whole brain tracking and mask controller (with InstaTract);
       matrices and graphs in SUMA; FMRI+tractography visualizations









