*****************
@get.afni.version
*****************

.. _ahelp_@get.afni.version:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    This script downloads the source code for a particular AFNI version.
    
    To use this script requires that you have the 'git' software
    package installed, since the AFNI source code is maintained
    at https://github.com/afni/AFNI
    
    Usage: @get.afni.version YY.Q.MM
    
    where YY.Q.MM is the version number to get (e.g., 16.0.01)
    
    Note that the final part of the version number always has 2
    digits -- 16.0.1 is NOT a valid AFNI version number.
    
    If you enter an invalid version number, the latest source code
    version will be fetched, and then an error message of the form
      error: pathspec 'AFNI_16.0.1' did not match any file(s) known to git.
    will appear.  At that point, the output directory will contain
    the latest AFNI source code available on github (which may be
    slightly in advance of the latest build version).
    At that point, you can
     (a) accept this source code; or,
     (b) delete the output with '/bin/rm -rf AFNI_YY.Q.MM' and try again; or,
     (c) 'cd AFNI_YY.Q.MM/AFNI' and then type 'git tag' to see
         what version numbers are available, then 'cd ../..', remove
         the current output as in (b), and try again; or,
     (d) give up and ask for help on the AFNI message board.
    
    The results are put into directory AFNI_YY.Q.MM/AFNI/src
    
    To compile the corresponding binaries, 'cd' to that directory,
    choose a Makefile from the output of 'ls Makefile.*', perhaps
    edit it to change the INSTALLDIR macro, then 'make vastness'.
    
    To see how a particular source file differs from the current version,
    a command of the form
      git diff master 3dDeconvolve.c
    can be used (once you have cd-ed to the src directory).
    
