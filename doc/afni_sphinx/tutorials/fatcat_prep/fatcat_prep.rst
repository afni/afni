.. _FATCAT_prep:

=======================================
**FATCAT: DWI Pre- and Postprocessing**
=======================================

Overview S
----------

This page provides a preliminary description of using AFNI+FATCAT
tools in conjunction with `TORTOISE
<https://science.nichd.nih.gov/confluence/display/nihpd/TORTOISE>`_
for processing diffusion weighted (DW) MRI data.  It also uses ``dcm2nii``
from `mricron <http://people.cas.sc.edu/rorden/mricron/dcm2nii.html>`_
for converting DICOM files to NIFTI format.

A. First, we discuss converting DW and anatomical data sets from DICOM
   format, organizing them, preparing the anatomical as a reference
   volume, visually checking the DWIs and removing bad
   volumes/gradients. So, these are mainly a pre-preprocessing guide.

#. We then provide a brief set of steps for using the TORTOISE GUI to
   process your data. If you have an IDL license, you can actually
   script TORTOISE; otherwise, the GUI is your fate...

#. We then show simple steps for tensor fitting and parameter
   estimation, as well as basic whole brain tracking for quality
   control.  We might even discuss some further processing, for
   example integrating with FreeSurfer, FMRI data, or other things...
   but the branching points for analysis become pretty numerous pretty
   quickly.

It is really difficult to come up with one unwavering pipeline for
this preprocessing, particularly as people have very different data--
just a set with one phase encode direction, or a pair of oppositely
phase encoded (AP-PA, blip-up/blip-down data)?  Scans of squirmy kids,
or meditating adults? etc.

These scripts are examples, not dogma. They are bound to change
(improve?) over time. Additional options can happily be added if they
simplify your life and don't overly complicate mine.  And please let
me know if you have any problems, via posting on the `Message Board
<https://afni.nimh.nih.gov/afni/community/board>`_ or emailing.  

Overview I: Distortions and TORTOISE
------------------------------------

Diffusion weighted MRI data can be greatly affected by a large number
of artifacts.  The most (in)famous are probably subject motion, eddy
current distortion and EPI distortion (B0 inhomogeneity).  Trickiness
in dealing with their effects is compounded by the fact that their
distortions *themselves* are compounded and can't really be separated.
Trying to manage them is serious business, and a combination of
thoughtful study design and careful data acquisition is as important
(if not more so) than any computational processing afterwards.

`TORTOISE (Pierpaoli et al., 2010)
<https://science.nichd.nih.gov/confluence/display/nihpd/TORTOISE>`_ is
a very effective retrospective processing tool for acquired DWIs, and
it is publicly available from the NIH.  As noted on their `webpage
<https://science.nichd.nih.gov/confluence/display/nihpd/TORTOISE>`_,
there are two main tools in TORTOISE for corrective preprocessing of
diffusion data:

    "*DIFF_PREP* - software for image resampling, motion, eddy current
    distortion, and EPI distortion correction using a structural image
    as target, and for re-orientation of data to a common space.

    *DR-BUDDI* - software for EPI distortion correction using pairs of
    diffusion data sets acquired with opposite phase encoding (blip-up
    blip-down acquisitions)."

By default, these functions use an anatomical reference for distortion
correction/unwarping, and it is strongly recommended to have one when
using TORTOISE. The anatomical data set should match the contrast of a
subject's *b*\ =0 DWI volume, which generally means having a
T2-weighted (T2w) volume; the application of a T1w volume as a
reference if a T2w one is not available is discussed below. Finally,
we note that TORTOISE includes the DIFF_CALC module for tensor
fitting, visualization and other features, but here it is only used
for exporting DIFF_PREP or DR-BUDDI results.

Overview II: DICOMs and dcm2nii
-------------------------------

DICOM conversion requires reading the data header, parsing it for
desired information (i.e., how many volumes, what order, voxel
dimensions, etc.), and putting it all into a usable format such as
NIFTI plus supplementary text files.  Many things in the DICOM world
aren't standardized, vendors do things independently (and change their
own inner workings from time to time), and tailored sequences might
contain additional wrinkles.  Basically, it's a hard task.

There's a fair number of DICOM converters out there and even some "in
here" in AFNI (e.g., ``to3d``).  I use ``dcm2nii`` for conversion
because it is `free
<http://people.cas.sc.edu/rorden/mricron/dcm2nii.html>`_, I have used
it for a long time, it requires minimal user input, and I've generally
had quite good luck with it.  That is, most converted volumes (appear
to) have correctly translated data and header information.  However,
there are no guarantees in life, and the user will be expected to look
over her/his own data for sanity checking everything. AFNI's

It is important to note that the TORTOISE folks generally recommend
reading in DICOM files to TORTOISE directly.  They do this because
they feel that they stay up-to-date with vendor changes and it is
therefore the most stable way to go.  So, dear user, by all means skip
the DICOM conversion steps below guilt free, if you prefer (it may
even be recommended if you acquire data with special, non-standard
sequences).  I *do* like to convert DICOMS to NIFTI so that I can view
the data and kick out bad volumes pre-TORTOISEing, and I haven't had
the misfortune to have major formatting trouble whilst doing so (*he
writes asking for trouble*...).

So, on with the scripting show, then.

Pre-preprocessing
-----------------

The purpose of this set of scripts is to: 

    * convert DICOMs to NIFTIs;

    * to allow the DWIs to be viewed, quality-checked and filtered
      according to the user's judgment (e.g., remove dropout volumes
      or those with heavy motion distortion);

    * to process dual phase encoded DWI data sets (i.e., when AP-PA
      data are present) in parallel, in order to maintain matched
      volumes/gradients;

    * to make an imitation T2w-like contrast reference volume if only
      a T1w is available (NB: 'twould be better to have the real
      thing, probably);

    * to put a reference anatomical into "nice" alignment within a
      volume for slice viewing and WM/tracking coloration.

You can skip any steps that aren't applicable. I will assume that each
acquired volume is currently a set of unpacked DICOMs sitting in its
own directory.

1. 
