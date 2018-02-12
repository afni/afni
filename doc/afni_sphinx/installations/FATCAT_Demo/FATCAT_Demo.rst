.. _FATCAT_Demo_install:

=======================
**FATCAT Demo Install**
=======================

FATCAT Demo contains a set of diffusion weighted data and scripts to
analyze them. The scripts are intended to illustrate the various tools
of the FATCAT toolbox and some of the ways to visualize the results.

To install main FATCAT_DEMO, run::

   @Install_FATCAT_DEMO
   
The script will download and install the data into a directory called
*FATCAT_DEMO/*. In there you will find a README file and a set of Do_*
scripts that are meant to be run sequencially. If you would prefer to
run all the scripts in one swell foop, just do::

   cd FATCAT_DEMO
   tcsh Do_00_PRESTO_ALL_RUNS.tcsh
   
In the meanwhile we trust you will read the README file and go over
the comments in each of the scripts that provide context to the
different commands used. To get a sneak peek at the results see the
:ref:`FATCAT Demo tutorial<FATCAT_Demo>`.

To install demo about using multivariate modeling (MVM) on FATCAT
data, such as that from tracking with ``3dTrackID`` or correlation ROI
time series with ``3dNetCorr``, run::

   @Install_FATMVM_DEMO

The script will download and install the data into a directory called
*FAT_MVM_DEMO/*, which has a "FAT_MVM_README.txt" file to explain
things (what data is included, what commands to run, etc.).

