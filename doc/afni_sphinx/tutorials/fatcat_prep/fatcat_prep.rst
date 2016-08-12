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

These scripts are examples, not dogma-- they are **a** way to go about
DWI processing with AFNI, FATCAT and TORTOISE (and dcm2nii). They are
bound to change (improve?) over time. Additional options can happily
be added if they simplify your life and don't overly complicate mine.
And please let me know if you have any problems, via posting on the
`Message Board <https://afni.nimh.nih.gov/afni/community/board>`_ or
emailing.

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
writes asking The Universe for trouble*...).

So, on with the scripting show, then.

Pre-preprocessing
-----------------

The purposes of this set of scripts are to: 

    * convert DICOMs to NIFTIs;

    * to allow the DWIs to be viewed, quality-checked and filtered
      according to the user's judgment (e.g., remove dropout volumes
      or those with heavy motion distortion);

    * to filter volume, gradient and b-value files of a given data set
      simultaneously;

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

Note that each function listed below has its own helpfile, describing
more details, defaults and available options.  Here, I will often use
default names and locations of things (such as output directories,
prefixes, etc.) in order to simplerify life.

.. note:: Have matched data sets with opposite phase encoding (e.g.,
          AP and PA) is useful for correcting EPI distortions.
          However, if you only have one, whether it is AP or PA
          doesn't really matter for this pre-processing-- I will refer
          to single phase encode data sets as 'AP' just for
          simplicity, but either encoding would get treated the same.

|

1. **Convert DWIs**

   Go from DICOMs to a NIFTI volume and supplementary text files (a
   '\*.bvec' file has the unit normal gradients, and a '\*.bval' file
   has the diffusion weighting b-values).

   * *Case A:* A single set of *N* DWIs acquired with a single phase
     encode direction (in SUB01/01_dicom_dir_AP/)::

        tcsh fat_pre_convert_dwis.tcsh                   \
            -indir_ap  SUB01/01_dicom_dir_AP

     -> produces a single directory called 'SUB01/UNFILT_AP/', which
     contains three files: AP.nii (*N* volumes), AP.bvec (3x\ *N*
     lines) and AP.bval (1x\ *N* lines).

   * *Case B:* Multiple sets each with *N* DWIs with a single phase
     encode direction (in SUB01/01_dicom_dir_AP/,
     SUB01/02_dicom_dir_AP/, SUB01/02_dicom_dir_AP/)::

        tcsh fat_pre_convert_dwis.tcsh                   \
            -indir_ap  SUB01/0*_dicom_dir_AP

     -> produces a single directory called 'SUB01/UNFILT_AP/', which
     contains three files: AP.nii (3\ *N* volumes), AP.bvec (3x3\ *N*
     lines) and AP.bval (1x3\ *N* lines).

   * *Case C:* A paired set of *N* DWIs with opposite phase encode
     directions (in SUB01/01_dicom_dir_AP/ and
     SUB01/01_dicom_dir_PA/)::

        tcsh fat_pre_convert_dwis.tcsh                   \
            -indir_ap  SUB01/01_dicom_dir_AP             \
            -indir_pa  SUB01/01_dicom_dir_PA

     -> produces two directories in 'SUB01/', one called 'UNFILT_AP/',
     which contains three files: AP.nii (*N* volumes), AP.bvec (3x\
     *N* lines) and AP.bval (1x\ *N* lines); and the other called
     'UNFILT_PA/', which contains three files: PA.nii (*N* volumes),
     PA.bvec (3x\ *N* lines) and PA.bval (1x\ *N* lines).

   Each data set will have 'RPI' orientation; the gradients in each
   case will not be flipped.  See the help file for changing these
   defaults, as well as output directories and file prefixes.

#. **Convert anatomical volume**

   Go from DICOMs to NIFTI. Sometimes ``dcm2nii`` creates multiple
   volumes from a single anatomical (one zoomed in on brain, etc.),
   but here we try to auto-select the basic one (file name typically
   starts with "2\*")

   * A single anatomical (in SUB01/01_dicom_dir_anat/)::

        tcsh fat_pre_convert_anat.tcsh                  \
            -indir  SUB01/01_dicom_dir_anat

     -> produces a single directory called 'SUB01/ANATOM/', which
     contains one file: anat.nii (there's also a subdirectory of
     SUB01/ANATOM/ containing intermediate files; should be
     ignorable).

   The anatomical will have 'RPI' orientation. You could change that,
   or rename it to reflect what kind of anatomical it is (e.g., T1w or
   T2w).

#. **Axialize the anatomical**

   It might be useful to have the standard slice planes of the brain
   be parallel with the sides of the volume.  That is, if a subject's
   head is strongly tilted in the volumetric field of view (FOV), then
   the display of slices might be awkward, anatomical definition might
   be tricky, and tract/structure coloration could be
   non-standard. 

   This program "rights the ship" by calculating an affine alignment
   to an a reference volume of the user's choice (e.g., a standard
   space Talairach volume), but only applying the rotation/translation
   part, so that the subject's brain doesn't warp/change shape.  This
   is essentially an automated version of AC-PC alignment.

   * A single anatomical volume (SUB01/ANATOM/anat.nii) and a
     similar-contrast anatomical reference (~/TEMPLATES/TT_N27+tlrc)::

       tcsh fat_pre_axialize_anat.tcsh                  \
           -inset   SUB01/ANATOM/anat.nii               \
           -refset  ~/TEMPLATES/TT_N27+tlrc

     -> produces a single file called 'SUB01/ANATOM/anat_axi.nii' (NB:
     default naming is not to add an appendix to the input, but right
     now is just generically 'anat_axi.nii'); there's also a working
     directory called 'SUB01/ANATOM/__WORK_prealign'; would be useful
     to look at if the auto-axializing fails.

   The alignment is done with 3dAllineate, and some options can be
   added to it from the command line; additionally, an option to
   resample the volume to a particular spatial resolution can be
   given.

#. **Make a T2w-like volume from a T1w one**

   For TORTOISEing, one should have a T2w anatomical, which is used as
   a reference volume to help unwarp things.  It has the useful
   properties of (hopefully) being relatively undistorted and of
   having similar contrast to the *b*\ =0 DWI volume.

   In the event that you *didn't* acquire such volumes as part of a
   study but that you *do* have T1w volumes, you can invert the
   brightness of the latter to estimate the relative tissue contrast
   of the former for use as a reference volume in TORTOISE.  You
   should probably *not* use the resulting imitation T2w volume for
   other applications, though.
   
   * A single T1w volume (SUB01/ANATOM/T1_axi.nii)::

       tcsh fat_pre_t2w_from_t1w.tcsh                   \
           -inset  SUB01/ANATOM/T1_axi.nii

     -> produces three files in SUB01/ANATOM/ called out_t2w.nii (the
     main output of interest), out_t1w.nii (a somewhat
     processed/polished T1w volume) and out_t1w_ss.nii (a
     skull-stripped version of the preceding file).  There is a bit of
     dim skull + noise outside the brain the first two files; it seems
     to matter for TORTOISE that there isn't zero-noise.

   This processing depends on skull-stripping in order to isolate the
   brain for inverting.  Skull-stripping is *really* a hard thing to
   do consistently algorithmically, so it is possible to do that
   separately and enter an isolated brain in as another option; see
   the help file for more about this and other minorly fun things.

   And always visually check to see that the output looks reasonable!

#. **Filter out (bad) DWIs**

   Say you have *N* DWIs in your data set; you will also have *N*
   gradient vectors and *N* b-values.  If you remove any DWI volume
   (e.g., perhaps it was corrupted by motion or had extreme dropout),
   then you also want to remove the corresponding gradient and b-value
   from their respective text files; and if you have AP-PA data, then
   you want to remove the corresponding DWI/grad/b-value from the
   opposite phase encoded set, so that every DWI has a partner.

   Here, we'll suppose that you look at each AP and/or PA DWIs (you
   can view the data in AFNI) and write down the indices of obviously
   bad/corrupted volumes.  Remember, AFNI indices start at '0'.  Then
   you enter the volumes and volume ranges **to be kept**, using
   standard AFNI notation for brick selection.

   * *Case A:* A single set of *N* DWIs acquired with a single phase
     encode direction (in SUB01/FILT/AP.nii, along with correponding
     '*.bvec' and '*.bval' files of matching length); assume you want
     to remove the volumes with index 4, 5 and 8, leaving *M*\ =\
     *N*\ -3 volumes/grads::

        tcsh fat_pre_filter_dwis.tcsh                      \
            -indir_ap  SUB01/UNFILT/AP.nii                 \
            -select    "[0..3,6,7,9..$]"

     -> produces a single directory called 'SUB01/FILT_AP/', which
     contains three files: AP.nii (*M* volumes), AP.bvec (3x\ *M*
     lines) and AP.bval (1x\ *M* lines). Note that the '..$' in the
     index selection represents 'to the last volume' in the data set.

|
     
Running TORTOISE
----------------

At present, if you don't have an IDL license, TORTOISE can only be run
through the GUI (i.e., 
