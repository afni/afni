What is AFNI?
----------------------

.. image:: https://travis-ci.org/afni/afni.svg?branch=master
    :target: https://travis-ci.org/afni/afni
    
.. image:: https://circleci.com/gh/afni/afni/tree/master.svg?style=shield
    :target: https://circleci.com/gh/afni/afni/tree/master

.. image:: https://codecov.io/gh/afni/afni/branch/master/graph/badge.svg
    :target: https://codecov.io/gh/afni/afni


AFNI (Analysis of Functional NeuroImages) is a suite of programs for looking at and analyzing MRI brain
images at all stages of analysis (planning, setting up acquisiton, preprocessing, analysis, quality control and statistical analysis).  It contains C, Python and R programs, as well as shell scripts, primarily developed for the 
analysis and display of multiple MRI modalities: 

* functional MRI (FMRI)
    * resting state, task-based or naturalistic paradigms
    * single- or multi-echo acquisitions
* anatomical/structural MRI
    * at various field strengths
* diffusion weighted imaging (DWI)
    * for DTI or HARDI modeling and tractography

Many AFNI programs have been applied and adapted to other modalities, such as ECoG, EEG, MEG, and more.  

It has graphical displays for both slice-wise and surface-based viewing.  In both cases, many aspects of visualization can be scripted for automatic image generation.
  
| Please visit these websites for more information:
| AFNI homepage: https://afni.nimh.nih.gov/
| Main AFNI docs: https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/ 

|

| For questions on using AFNI programs, our Message Board is here:  
| https://afni.nimh.nih.gov/afni/community/board/list.php?1 .


Getting started with AFNI: quick guide links
----------------------------------------------

* Installing AFNI on your system

    * `Instructions per OS <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/main_toc.html>`_
    
    * `Docker container notes <https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/container.html>`_

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


AFNI code directory
-------------------

Currently, the top directory contains three sub-directories, each with further code stratification:

doc/
    documentation for AFNI (though this is outdated; current doc content resides in its own git tree here: https://github.com/afni/afni_doc)
src/
    source code for AFNI
src/python_scripts/
    Python command programs and library files; distributed together as the **afnipy** module, which can be imported and used as: ``from afnipy import ...``
tests/
    tests for AFNI codebase


Relevant git-ology for AFNI
---------------------------

First time stuff
~~~~~~~~~~~~~~~~

1. Make yourself known to git-land::

    git config --global user.name   "Fred Mertz"
    git config --global user.email  mertzf@bargle.argle
    git config --global core.editor vim

2. Create a copy of the repository on your machine::

    git clone https://github.com/afni/afni.git

3. Forking a branch and making a pull request (PR):

    https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/devdocs/pull_requests/pr_ex.html

Stuff to do as needed
~~~~~~~~~~~~~~~~~~~~~

- Getting updates from the master branch of the repository::

    git pull origin master

- Seeing what changes you have made locally::

    git status

- To commit some files to your LOCAL repository (preferred)::

    git commit -m "PLEASE comment"   FILE1 FILE2 ...

- To commit all tracked files with changes (locally)::

    git commit -a -m "PLEASE try to put a comment here"

- If you have new files to add into the repository;
  **PLEASE PLEASE PLEASE, be careful with wildcards!!!**
  The main thing is to avoid adding very large files (such as binaries)
  by mistake!::

    git add -f FILE1 FILE2 ...

- Sending the local updates to the master (github.com) repository::

    git push origin master


Compilation of AFNI
-------------------

In src/, you need to choose one of the Makefile.* files that is closest
to your system, and cp it to be named Makefile.  Makefile is set up to
install into the INSTALLDIR location, defined in that file -- you should
probably change that to be appropriate for your use.

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
