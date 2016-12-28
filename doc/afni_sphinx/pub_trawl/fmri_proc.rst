.. _fmri_proc:


*****************
**FMRI Examples**
*****************

.. contents::
   :depth: 3

Overview
========

Publications are listed in reverse chronological order. Many thanks to
the authors who have made their work available for general benefit.

Below are searchable tags and labels for each paper.  This may help
when searching for a paper with a given type of data or desired
processing step. 


.. table:: (Optional) descriptors for each paper, for searchability
    :widths: 1 3
    :column-alignment: left left
    :column-wrapping: true true 
    :column-dividers: double single double

    =======================  =============================================================
    Tag                      Label
    =======================  =============================================================
    FMRI paradigm:           task-block, task-event, resting, naturalistic, par-other
    FMRI dset:               EPI, dual phase (AP-PA), fmri-other
    Anatomical dset:         MPRAGE, T1w, T2w, T1map, T2map, FLAIR, FLASH, PD, SWI, Angio, 
                             none, anat-other
    Subject population:      human, nonhuman primate, macaque, rat, simulation, pop-other
    Subject characteristic:  patient, control, char-other
    Subject age:             prenatal, newborn, infant, child, juvenile, adult, senior, 
                             age-other
    Template space:          MNI, Talairach, Haskins-ped, native, sp-other
    Template align method:   linear, nonlinear, al-other
    Tissue segmentation:     3dSeg, FreeSurfer, seg-other
    Tissue regression:       ANATICOR, fANATICOR, PCA, WM+Vent, reg-other
    =======================  =============================================================


Publications with example scripts
=================================

.. _public_ex_fmri_2017:

**2017**
---------


|

.. _public_ex_fmri_2016:

**2016**
---------
 
*  .. include:: fmri_proc/paper_2016_ChenEtal.rst

   .. literalinclude:: fmri_proc/paper_2016_ChenEtal/script_01_init.tcsh
   |
   .. literalinclude:: fmri_proc/paper_2016_ChenEtal/script_02_ap.tcsh
