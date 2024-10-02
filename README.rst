What is AFNI?
----------------------

AFNI (Analysis of Functional NeuroImages) is a suite of programs for
looking at and analyzing MRI brain images at all stages of analysis
(planning, setting up acquisition, preprocessing, analysis, quality
control and statistical analysis).  It contains C, Python and R
programs, as well as shell scripts, primarily developed for the
analysis and display of multiple MRI modalities:

* functional MRI (FMRI)
  * resting state, task-based or naturalistic paradigms
  * single- or multi-echo acquisitions
* anatomical/structural MRI
  * at various field strengths
* diffusion weighted imaging (DWI)
  * for DTI or HARDI modeling and tractography

Many AFNI programs have been applied and adapted to other modalities,
such as ECoG, EEG, MEG, and more.

It has graphical displays for both slice-wise and surface-based
viewing.  In both cases, many aspects of visualization can be scripted
for automatic image generation.
  
Please visit these websites for more information and to ask questions:

* `Main AFNI docs <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/>`_

* `AFNI Message Board <https://discuss.afni.nimh.nih.gov>`_


Install AFNI on your computer
-----------------------------

You can install AFNI on various Linux, macOS and Windows systems.
Detailed instructions for each system are provided `here
<https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/main_toc.html>`_.
This is the way most users get the AFNI code to use.

Please make sure to use the AFNI system check to help guide you in the
process, as well facilitate asking questions::

  afni_system_check.py -check_all


Getting started with AFNI: quick guide links
----------------------------------------------

* Installing AFNI on your system

    * `Instructions per OS <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/main_toc.html>`_
    
    * `Docker container notes <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/container.html>`_

    * `AFNI Message Board <https://discuss.afni.nimh.nih.gov>`_

* Educational resources

    * `AFNI Academy series <https://www.youtube.com/c/afnibootcamp>`_
    
    * `Additional Bootcamp recordings <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/educational/bootcamp_recordings.html>`_
        
    * `Links to AFNI handouts, keyboard shortcuts and startup tips <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/educational/main_toc.html>`_
    
    * `Page linking to all program helps <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/main_toc.html>`_
    
    * `"Classified" program guide (AFNI programs grouped thematically) <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/educational/classified_progs.html>`_
    
    * `Quick Unix tutorial <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/unix_tutorial/index.html>`_

* Code, command, script and functionality examples

    * `Tutorials and demos <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/tutorials/main_toc.html>`_

    * `Publication-based code examples (AFNI Codex) <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/codex/main_toc.html>`_

* Additional resources

    * `Methods publications and reference list <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/published/citations.html>`_

    * `Nonhuman projects and processing (with demos) <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/nonhuman/main_toc.html>`_
        
    * `Templates and atlases <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/template_atlas/main_toc.html>`_

    * `Statistics notes <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/statistics/main_toc.html>`_
    
    * `Notes on building AFNI Sphinx docs (afni_doc) locally <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/devdocs/sphinx_docs/setup.html>`_


Additional software collaborations
--------------------------------------

AFNI benefits from integration and collaboration with several other
actively developed neuroimaging software packages.  We greatly
appreciate the work of (and often the discussions with) their
developers, and note some of these projects here.  Further useful
dependencies are cited within the codebase.

The following software are distributed within AFNI directly:

* `dcm2niix <https://github.com/rordenlab/dcm2niix>`_,
  included as ``dcm2niix_afni``
  
* `NiiVue <https://github.com/niivue/niivue>`_,
  included as ``niivue_afni.umd.js``
  
AFNI also has several programs and features that 
complement directly with the following projects:

* `FreeSurfer <https://surfer.nmr.mgh.harvard.edu/>`_,
  via ``@SUMA_Make_Spec_FS`` and ``afni_proc.py``

* `LayNii <https://github.com/layerfMRI/LAYNII>`_, for layer FMRI
  processing and visualization

* `tedana <https://tedana.readthedocs.io/en/stable/>`_,
  via ``afni_proc.py``

* `TORTOISE <https://github.com/eurotomania/TORTOISEV4>`_, via the
  FATCAT and other DWI tools in AFNI

One aspect of AFNI's wide usage and motivation for maintaining
long-term stability is that it serves as an underlying part within
several other software projects and pipeline tools in the
field. Some projects that use AFNI include:

* `BrainSuite <https://brainsuite.org/bfp/>`_: Functional Pipeline:
  open-source workflow for processing FMRI data
* `C-PAC <https://fcp-indi.github.io/>`_: Configurable Pipeline for
  the Analysis of Connectomes software package
* `DPABI <http://rfmri.org/DPABI>`_: Data Processing & Analysis for
  Brain Imaging software suite
* `ENIGMA HALFpipe
  <https://enigma.ini.usc.edu/protocols/functional-protocols/>`_:
  software from a consortium combining imaging and genetics
* `fMRIPrep <https://github.com/nipreps/fmriprep>`_: FMRI
  preprocessing pipeline that combines tools from well-known software
  packages
* `LONI QC <https://qc.loni.usc.edu/>`_: data review platform for
  neuroimaging studies with one or more centers
* `Micapipe <https://micapipe.readthedocs.io/en/latest/>`_: a 
  processing pipeline to analyze multimodal MRI data
* `NeoRS <https://github.com/venguix/NeoRS>`_: Neonatal Resting 
  State fMRI data preprocessing pipeline
* `NeuroDebian <https://neuro.debian.net/>`_: an open, turnkey
  platform for neuroscience, integrating software
* `NeuroDesk <https://www.neurodesk.org/>`_: open platform of data
  analysis environments for reproducible neuroimaging
* `Nipype
  <https://nipype.readthedocs.io/en/latest/api/generated/nipype.interfaces.afni.html>`_:
  an open-source initiative to integrate different packages in a
  workflow
* `PESTICA <https://github.com/wanyongshinccf/PESTICA>`_ & 
  `SLOMOCO <https://github.com/wanyongshinccf/SLOMOCO>`_: 
  physio and slice-oriented motion correction tools

NIFTI and GIFTI
----------------

AFNI developers also lead the maintenance of the following fundamental
neuroimaging repositories:

* `NIFTI C-library <https://github.com/NIFTI-Imaging/nifti_clib>`_

* `GIFTI C-library <https://github.com/NIFTI-Imaging/gifti_clib>`_

These are the code for the standard volumetric and surface dataset
formats, respectively, in MRI. These formats are central to how
software input/output standard data, and most neuroimaging packages in
the field (including AFNI itself) use at least one of these formats.

NIFTI was first developed by a consortium of neuroimaging developers
in 2004, with AFNI's Bob Cox being a primary architect and first
author of the format description, which is still so widely used today:

* Cox RW, Ashburner J, Breman H, Fissell K, Haselgrove C, Holmes CJ,
  Lancaster JL, Rex DE, Smith SM, Woodward JB, Strother SC
  (2004). (sort of) new image data format standard:
  NiFTI-1. `Presented at the 10th Annual Meeting of the Organization
  for Human Brain Mapping
  <https://nifti.nimh.nih.gov/nifti-1/documentation/hbm_nifti_2004.pdf>`_.

AFNI code directory
-------------------

Currently, the top directory contains three sub-directories, each with
further code stratification:

doc/
    documentation for AFNI (though this is outdated; current doc
    content resides in its own git tree here:
    https://github.com/afni/afni_doc)
src/
    source code for AFNI
src/python_scripts/
    Python command programs and library files; distributed together as
    the **afnipy** module, which can be imported and used as:

    ``from afnipy import ...``
tests/
    tests for AFNI codebase


Compilation of AFNI
-------------------

In addition to standard installation of AFNI on computers, you can
also compile the code locally on your computer (e.g., for
development). In src/, you need to choose one of the Makefile.\* files
that is closest to your system, and cp it to be named Makefile.
Makefile is set up to install into the INSTALLDIR location, defined in
that file -- you should probably change that to be appropriate for
your use.

If you are using Mac OS X, choose one of the Makefile.macosx_* files.

For later versions of Mac OS X, Apple's C compiler does not support
OpenMP, so we recommend downloading and installing a version of gcc
from http://hpc.sourceforge.net/ or purchasing a commercial C compiler
(e.g., Intel's icc) that does support OpenMP.  Several important
programs in the AFNI suite are parallelized via OpenMP, and will run
much faster if compiled appropriately.

If you are using Linux, try Makefile.linux_openmp_64 first.

To make and install everything do::

    make vastness

The command::

    make cleanest

will remove all the *.o files, etc.

Making a pull request to the AFNI code base
-------------------------------------------

| Notes for making a fork and pull request to AFNI are provided here:
| https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/devdocs/pull_requests/pr_ex.html

| Users and developers are also welcome to open up GitHub Issues here:
| https://github.com/afni/afni/issues

| We also have a very active Message Board for asking questions about
  using AFNI programs, getting help with installations, seeing new
  features, staying up-to-date with Bootcamp and other announcements,
  and more: 
| https://discuss.afni.nimh.nih.gov/


Online testing notes
--------------------

.. image:: https://travis-ci.org/afni/afni.svg?branch=master
    :target: https://travis-ci.org/afni/afni
    
.. image:: https://circleci.com/gh/afni/afni/tree/master.svg?style=shield
    :target: https://circleci.com/gh/afni/afni/tree/master

.. image:: https://codecov.io/gh/afni/afni/branch/master/graph/badge.svg
    :target: https://codecov.io/gh/afni/afni
