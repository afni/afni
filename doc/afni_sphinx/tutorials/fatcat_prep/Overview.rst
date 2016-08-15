.. _FATPREP_overview:

Background and additional software
==================================

.. contents::
   :depth: 3


Overview
--------

**For now**, the script files to execute are here:
:download:`SCRIPT_TARBALL <media/FAT_PRE_SCRIPTS.tar.gz>`

This page provides a preliminary description of using AFNI+FATCAT
tools in conjunction with `TORTOISE
<https://science.nichd.nih.gov/confluence/display/nihpd/TORTOISE>`_
for processing diffusion weighted (DW) MRI data.  It also uses ``dcm2nii``
from `mricron <http://people.cas.sc.edu/rorden/mricron/dcm2nii.html>`_
for converting DICOM files to NIFTI format.

*  :ref:`preTORTOISE`.
   First, we discuss converting DW and anatomical data sets from DICOM
   format, organizing them, preparing the anatomical as a reference
   volume, visually checking the DWIs and removing bad
   volumes/gradients. So, these are mainly a pre-preprocessing guide.

*  :ref:`TORTOISEing`. We then provide a brief set of steps for using
   the TORTOISE GUI to process your data. If you have an IDL license,
   you can actually script TORTOISE; otherwise, the GUI is your
   fate...

*  :ref:`postTORTOISEing`. We then show simple steps for tensor
   fitting and parameter estimation, as well as basic whole brain
   tracking for quality control.  We might even discuss some further
   processing, for example integrating with FreeSurfer, FMRI data, or
   other things...  but the branching points for analysis become
   pretty numerous pretty quickly.

It is pretty difficult to come up with one unwavering pipeline for
this preprocessing, particularly as people have very different
data. For example, were the DWIs acquired using a single phase encode
direction, or a pair of oppositely phase encoded (AP-PA,
blip-up/blip-down data)?  Scans of squirmy kids, or meditating adults?
Is there a T2w anatomical, or only a T1w volume?  Therefore, we
describe a possible sequence of tools, but mention that not all would
be necessary (or even appropriate, in some cases) in every single
pipeline.  The user will be required to do some decision making.

These scripts are examples, not dogma-- they are **a** way to go about
DWI processing with AFNI, FATCAT and TORTOISE (and dcm2nii). They are
bound to change (improve?) over time. Additional options can happily
be added if they simplify your life and don't overly complicate mine.
And please let me know if you have any problems, via posting on the
`Message Board <https://afni.nimh.nih.gov/afni/community/board>`_ or
emailing.


TORTOISE and DWIs
-----------------

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

    * *DIFF_PREP* - software for image resampling, motion, eddy
      current distortion, and EPI distortion correction using a
      structural image as target, and for re-orientation of data to a
      common space.

    * *DR-BUDDI* - software for EPI distortion correction using pairs
      of diffusion data sets acquired with opposite phase encoding
      (blip-up blip-down acquisitions).

By default, these functions use an anatomical reference for distortion
correction/unwarping, and it is strongly recommended to have one when
using TORTOISE. The anatomical data set should match the contrast of a
subject's *b*\ =0 DWI volume, which generally means having a
T2-weighted (T2w) volume; the application of a T1w volume as a
reference if a T2w one is not available is discussed below. Finally,
we note that TORTOISE includes the DIFF_CALC module for tensor
fitting, visualization and other features, but here it is only used
for exporting DIFF_PREP or DR-BUDDI results.  

As also noted on their webpage, it is not a speedy program.  It will
take upwards of a couple hours per data set to process with either
DIFF_PREP or DR-BUDDI, increasing with higher spatial resolution, with
selecting to upsample more, and with a larger number of
volumes/gradients. 


DICOMs and dcm2nii
------------------

DICOM conversion requires reading the data header, parsing it for
desired information (i.e., how many volumes, what order, voxel
dimensions, etc.), and putting it all into a usable format such as
NIFTI plus supplementary text files.  Many things in the DICOM world
aren't standardized, vendors do things independently (and change their
own inner workings from time to time), and tailored sequences might
contain additional wrinkles.  Basically, it's a hard task.

There's a fair number of DICOM converters out there and even some "in
here" in AFNI (e.g., ``to3d``, ``Dimon``).  I use ``dcm2nii`` for
conversion because it is `free
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
writes asking The Universe for trouble*...).

.. _DWI_distortions:

Distorted views of the DWI world
--------------------------------

What do distortions in DWI data look like?  Ugly stuff.

1. **EPI distortion**

   EPI distortions occur predominantly along the phase encode
   direction (often along the anterior-posterior orientation), and
   these cause both geometric distortions (brain warping: stretching
   and compressing) and signal intensity distortions (wrong signal
   value stored: signal pileup and attenuation).  These can effect
   both the reference *b* \=0 and gradient weighted volumes.  

   One can see the relative the locations of greatest distortion when
   comparing the oppositely phase encoded data.  TORTOISE uses
   registration between the oppositely encoded sets, as well as the
   anatomical reference, to reduce the warping distortions (see
   Irfanoglu et al., 2012).

   .. list-table:: 
      :header-rows: 1
      :widths: 50 50

      * - Identical slices, single subject DWI.
        -
      * - .. image:: media/Screenshot_from_2016-08-12_15:41:22.png
             :width: 100%
        - .. image:: media/Screenshot_from_2016-08-12_15:40:58.png
             :width: 100%
      * - *PA encoded b=0 volume.*
        - *AP encoded b=0 volume.*

   |

#. **Eddy current distortion**

   Rapid switching of the diffusion gradients causes distortions.
   These occur in the *b*\>0 volumes of a DWI data set.  They cause
   nonlinear distortions, and generally need nonlinear registration to
   reduce their effects.  The DIFF_PREP part of TORTOISE tries to undo
   some of these.

#. **Subject motion**

   When we talk about subjection motion, we can talk about two main
   types: motion occuring between volumes, and motion that occurs with
   a TR.  (And in practice, there is often a combination of the two.)
   If motion happened only between TRs, then we are in a better
   position to "correct" some of its effects, essentially by using a
   good volume registration procedure.  The assumption is that the
   signal value at a location is what it should be-- we just have to
   reorient the head to put that voxel back where it was
   pre-motion. (NB: this is a simplification-- motion has other
   knock-on effects on data acquisition, but we hope these are fairly
   small.)

   The within-TR motion is quite problematic, though.  Consider a
   standard DWI acquisition sequence that collects axial slices in an
   interleaved pattern.  That is, it collects slices #0, 2, 4, 6, 8,
   etc. and then slices #1, 3, 5, 7, etc.  What happens if a person
   moves during this?  Pre-motion slices might be fine, but those
   afterward are not properly measured, and a distinctive brightness
   pattern can be seen in a sagittal view.  This is often known as the
   "Venetian blind" effect, and it is very easy to spot when looking
   at data-- this would be a good candidate to filter out.

   .. list-table:: 
      :header-rows: 1
      :widths: 100

      * - .. image:: media/Screenshot_from_2016-08-12_15:09:20.png
             :width: 100%
      * - *Example of subject motion artifact in a DWI volume that was
          acquired with an interleaved sequence (which is common).* 

   |

#. **Signal dropout**

   Signal dropout can occur due to susceptibility and excitation
   problems, sometimes limiting problems to just one slice.  However,
   that slice is effectively useless, and one might consider filtering
   out this volume.  (NB: in some cases, the volume could be left in
   if using an outlier rejection algorithm on a voxelwise basis for
   tensor fitting.)

   .. list-table:: 
      :header-rows: 1
      :widths: 100

      * - .. image:: media/Screenshot_from_2016-08-12_10:21:09.png
             :width: 100%
      * - *Example of a dropout slice in a DWI volume.*


