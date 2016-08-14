.. _FATCAT_prep:

=======================================
**FATCAT: DWI Pre- and Postprocessing**
=======================================

|

**FOR NOW**, the script files to execute are here:
:download:`SCRIPT_TARBALL <media/FAT_PRE_SCRIPTS.tar.gz>`

|

|

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

As also noted on their webpage, it is not a speedy program.  It will
take upwards of a couple hours per data set to process with either
DIFF_PREP or DR-BUDDI, increasing with higher spatial resolution, with
selecting to upsample more, and with a larger number of
volumes/gradients. 

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

Overview III: Distorted views of the DWI world
----------------------------------------------

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
   |


.. _preTORTOISE:

Pre-preprocessing
-----------------

Somehow, this ended up being a long section, but don't fret-- the
reality is that there are very few scripts to run (about 5), and each
has a pretty short syntax.

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
own directory. If a directory structure is set up well, it should be
possible to loop through all subjects with the same few commands. (The
filtering step, though, would likely require its own command per
subject, as motion/distortion will occur in different volumes for
different subjects.)

Note that each function listed below has its own helpfile, describing
more details, defaults and available options.  Here, default names and
locations of things (such as output directories, prefixes, etc.) are
often used in order to simplerify life.

.. note:: Have matched data sets with opposite phase encoding (e.g.,
          AP and PA) is useful for correcting EPI distortions.
          However, if you only have one, whether it is AP or PA
          doesn't really matter for this pre-processing-- I will refer
          to single phase encode data sets as 'AP' just for
          simplicity, but either encoding would get treated the same.

|

0. **Setup**

   Consider starting with the following directory structure: group
   directory for a study (e.g., DTI_GROUP_STUDY/) with a subdirectory
   for each subject.  Consider one subject's directory, which contains
   three sets of DICOM directories: one AP DWI scan, one PA DWI scan
   and one anatomical scan.

   .. list-table:: 
      :header-rows: 1
      :widths: 100
      
      * - .. image:: media/Screenshot_from_2016-08-12_09:31:58.png
             :width: 100%
      * - *Initial, basic subject directory layout.*
   |


#. **Convert DWIs**

   Go from DICOMs to a NIFTI volume and supplementary text files (a
   '\*.bvec' file has the unit normal gradients, and a '\*.bval' file
   has the diffusion weighting b-values).

   * *Case A:* A paired set of *N* DWIs with opposite phase encode
     directions (in SUB01/01_dicom_dir_AP/ and
     SUB01/01_dicom_dir_PA/)::

        fat_pre_convert_dwis.tcsh                        \
            -indir_ap  SUB01/01_dicom_dir_AP             \
            -indir_pa  SUB01/01_dicom_dir_PA

     -> produces two directories in 'SUB01/', one called 'UNFILT_AP/',
     which contains three files: AP.nii (*N* volumes), AP.bvec (3x\
     *N* lines) and AP.bval (1x\ *N* lines); and the other called
     'UNFILT_PA/', which contains three files: PA.nii (*N* volumes),
     PA.bvec (3x\ *N* lines) and PA.bval (1x\ *N* lines).

     .. list-table:: 
        :header-rows: 1
        :widths: 100

        * - .. image:: media/Screenshot_from_2016-08-12_09:33:47.png
               :width: 100%
        * - *End of 'DWI conversion' script message, and listing of
            directories afterwards.*
     |

   * *Case B:* A single set of *N* DWIs acquired with a single phase
     encode direction (in SUB01/01_dicom_dir_AP/)::

        fat_pre_convert_dwis.tcsh                        \
            -indir_ap  SUB01/01_dicom_dir_AP

     -> produces a single directory called 'SUB01/UNFILT_AP/', which
     contains three files: AP.nii (*N* volumes), AP.bvec (3x\ *N*
     lines) and AP.bval (1x\ *N* lines). Output would look similar to
     *Case A* but without the PA results.

   * *Case C:* Multiple sets each with *Q* DWIs with a single phase
     encode direction (in SUB01/01_dicom_dir_AP/,
     SUB01/02_dicom_dir_AP/, SUB01/02_dicom_dir_AP/)::

        fat_pre_convert_dwis.tcsh                        \
            -indir_ap  SUB01/0*_dicom_dir_AP

     -> produces a single directory called 'SUB01/UNFILT_AP/', which
     contains three files: AP.nii (*N*\=3\ *Q* volumes), AP.bvec (3x\ *N*
     lines) and AP.bval (1x\ *N* lines). Output would look similar to
     *Case A* but without the PA results.

   Each data set will have 'RPI' orientation; the gradients in each
   case will not be flipped.  See the help file for changing these
   defaults, as well as output directories and file prefixes.

#. **Convert anatomical volume**

   Go from DICOMs to NIFTI. Sometimes ``dcm2nii`` creates multiple
   volumes from a single anatomical (one zoomed in on brain, etc.),
   but here we try to auto-select the basic one (file name typically
   starts with "2\*")

   * A single anatomical (in SUB01/01_dicom_dir_anat/)::

        fat_pre_convert_anat.tcsh                       \
            -indir  SUB01/01_dicom_dir_anat

     -> produces a single directory called 'SUB01/ANATOM/', which
     contains one file: anat.nii (there's also a subdirectory of
     SUB01/ANATOM/ containing intermediate files; should be
     ignorable).

     .. list-table:: 
        :header-rows: 1
        :widths: 100

        * - .. image:: media/Screenshot_from_2016-08-12_09:43:26.png
               :width: 100%
        * - *End of 'anatomical conversion' script message, and
            listing of directories afterwards.*

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
     similar-contrast anatomical reference (e.g.,
     ~/TEMPLATES/TT_N27+tlrc, or wherever stored on your computer)::

       fat_pre_axialize_anat.tcsh                       \
           -inset   SUB01/ANATOM/anat.nii               \
           -refset  ~/TEMPLATES/TT_N27+tlrc

     -> produces a single file called 'SUB01/ANATOM/anat_axi.nii' (NB:
     default naming is to output a file called 'anat_axi.nii',
     independent of input name); there's also a working directory
     called 'SUB01/ANATOM/__WORK_prealign'; would be useful to look at
     if the auto-axializing fails.  There might be some warnings about
     converting standard space to orig space, but that should be OK if
     the inset is in 'orig' space.

     .. list-table:: 
        :header-rows: 1
        :widths: 100

        * - .. image:: media/Screenshot_from_2016-08-12_09:50:16.png
               :width: 100%
        * - *End of 'axializing' script message, and listing of
            directories afterwards.*

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
   
   * A single T1w volume (SUB01/ANATOM/anat_axi.nii)::

       fat_pre_t2w_from_t1w.tcsh                        \
           -inset  SUB01/ANATOM/anat_axi.nii

     -> produces three files in SUB01/ANATOM/ called out_t2w.nii (the
     main output of interest), out_t1w.nii (a somewhat
     processed/polished T1w volume) and out_t1w_ss.nii (a
     skull-stripped version of the preceding file).  There is a bit of
     dim skull + noise outside the brain the first two files; it seems
     to matter for TORTOISE that there isn't zero-noise.

     .. list-table:: 
        :header-rows: 1
        :widths: 100

        * - .. image:: media/Screenshot_from_2016-08-12_09:53:56.png
               :width: 100%
        * - *End of 'T1w inversion -> ~T2w' script message, and
            listing of directories afterwards.*

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

   * *Case A:* A paired set of *N* DWIs acquired with opposite phase
     encode directions (in SUB01/UNFILT_AP/AP.nii and
     SUB01/UNFILT_PA/PA.nii, each having correponding '\*.bvec' and
     '\*.bval' files of matching length in the respective directories);
     assume you want to remove the volumes with index 4, 5 and 8,
     leaving *M*\ =\ *N*\ -3 volumes/grads::

        fat_pre_filter_dwis.tcsh                           \
            -inset_ap  SUB01/UNFILT_AP/AP.nii              \
            -inset_pa  SUB01/UNFILT_PA/PA.nii              \
            -select    "[0..3,6,7,9..$]"

     -> produces a pair of directories called 'SUB01/FILT_AP/' and
     'SUB01/FILT_PA/', each of which contains three files: in the
     first, AP.nii (*M* volumes), AP.bvec (3x\ *M* lines) and AP.bval
     (1x\ *M* lines); and in the second, an analogously named set of
     identical dimensions.
       
     .. list-table:: 
        :header-rows: 1
        :widths: 100

        * - .. image:: media/Screenshot_from_2016-08-12_11:00:19.png
               :width: 100%
        * - *End of 'DWI filtering' script message, and listing of
            directories afterwards.*
        * - .. image:: media/Screenshot_from_2016-08-12_11:00:49.png
               :width: 100%
        * - *File listing within the filtered directories.*
        * - .. image:: media/Screenshot_from_2016-08-12_11:01:50.png
               :width: 100%
        * - *Command line checking of difference in number of volumes.*
        * - .. image:: media/Screenshot_from_2016-08-12_11:08:00.png
               :width: 100%
        * - *Command line checking of difference in number of entries
            in text files, bvals (top pair) and bvecs (bottom pair).
            Columns are: # of lines, # of total words or numbers, # of
            characters.*
     |

   * *Case B (and C, from above):* A single set of *N* DWIs acquired
     with a single phase encode direction (in SUB01/UNFILT_AP/AP.nii,
     along with correponding '\*.bvec' and '\*.bval' files of matching
     length); assume you want to remove the volumes with index 4, 5
     and 8, leaving *M*\ =\ *N*\ -3 volumes/grads::

        fat_pre_filter_dwis.tcsh                           \
            -inset_ap  SUB01/UNFILT_AP/AP.nii              \
            -select    "[0..3,6,7,9..$]"

     -> produces a single directory called 'SUB01/FILT_AP/', which
     contains three files: AP.nii (*M* volumes), AP.bvec (3x\ *M*
     lines) and AP.bval (1x\ *M* lines). 

   Other output directory names and prefixes can be chosen. It's
   important to note that TORTOISE will decide its own output
   directory names based on the prefix of the NIFTI file, so you don't
   want the paired phase encode files to have the same prefixes. In
   terms of the volume selection index rules, the '..$' represents 'to
   the last volume in the data set'; if this and other rules aren't
   familiar, check the AFNI docs, such as the help of ``3dcalc``.

|
     
Running TORTOISE
----------------

This stage describes performing the actual preprocessing, in terms of
reducing the effects of distortions due to subject motion, eddy
currents and B0 inhomogeneities.  This collective action is usually
called "distortion correction," although we should be clear that at
best we can only *reduce effects* of distortions retrospectively.  The
degree to which that desirable goal is possible depends on the
processing tools being used, but also on the study design on the
acquired data. In practice, as well, it also depends on the type of
analysis to be performed afterwards, because the question, "Are my
data clean enough to be appropriate for analysis *X*?" depends on what
the details and assumptions of that testing.

These major steps are performed using `TORTOISE
<https://science.nichd.nih.gov/confluence/display/nihpd/TORTOISE>`_.
At present, if you don't have an IDL license, TORTOISE can only be run
through the GUI (i.e., button clicking!).  A PDF describing **a**
system of steps for processing with TORTOISE v2.5.2 (though other
v2.\* are quite similar, mainly with just some of the GUI format
changing) is provided here:

:download:`Running_TORTOISE_v2.5.2.pdf
<Running_TORTOISE_v2.5.2.pdf>`.

This description is not official, but
it does tie in directly with the preceding steps in
:ref:`preTORTOISE`.

**INPUT** for TORTOISE: there is some flexibility here.  The above
notes describe taking a pair of DWI data sets with oppositely encoded
phases (AP-PA). The instructions can also be applied for when just a
single DWI data set (i.e., only one phase encoding direction-- either
AP or PA, or anything else for that matter) is obtained. In either
case a reference anatomical volume (either a real T2w volume, or an
imitation one as estimated above) should also be included.

**OUTPUT** from TORTOISE: again, there is flexibility here.  Default
for us will be to export a single 4D data set of DWIs (DWI.nii) and
their DWI gradient/*b*\-value information (BMTXT_AFNI.txt).  This is
the case even if we put in AP-PA data, or just a single phase encoded
set. The *b*\-value information is output as a *b*\-matrix text file,
and this includes information of any rotations made to volumes during
processing.

.. warning:: We note that versions of TORTOISE **before** v2.5.2
             contained a slightly different format of *b*\-matrix that
             wasn't



asdf
   


.. asdf

     .. figure:: media/ROIS/ROI_neigh_img.png
        :width: 80%
        :align: center
        :name: media/ROIS/ROI_neigh_img.png
   
        *Basic voxel terminology, and its use in defining three
        standard, symmetric (nearest-)neighborhoods for an individual
        voxel. The central voxel is darkened, with each type of
        neighborhood colored in a 3D, high-tec, separated image.*
        :ref:`(link)<media/ROIS/ROI_neigh_img.png>`

