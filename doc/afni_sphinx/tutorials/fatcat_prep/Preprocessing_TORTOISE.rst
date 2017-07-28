.. _fp_preproc_TORTOISE:


Preproc: TORTOISE
=================

.. contents::
   :depth: 3

Comments
--------

This stage describes performing the "actual" preprocessing of the DWIs
themselves, in terms of reducing the effects of distortions due to
subject motion, eddy currents and B0 inhomogeneities.  This collective
action is usually called "distortion correction," although we should
be clear that at best we can only *reduce effects* of distortions
retrospectively.  The degree to which that desirable goal is possible
depends on the processing tools being used, but also (heavily) on the
study design and on the acquired data. In practice, as well, the
practical effectiveness of processing also depends on the type of
analysis to be performed afterwards, because the answer to the
question, "Are my data clean enough to be appropriate for analysis
*X*?" depends on what the details and assumptions of that testing are.

These major steps are performed using `the TORTOISE tools
<https://science.nichd.nih.gov/confluence/display/nihpd/TORTOISE>`_,
which are also freely available from the NIH (thanks, taxpayers!).
Here, we describe briefly how we use ``DIFFPREP`` and ``DR_BUDDI`` in
TORTOISE v3.\* (no description of ``DIFF_CALC``; we perform fitting
with AFNI-FATCAT tools).  

The data set used here has the following
characteristics:

* pretty clean, not super distorted or noisy

* acquired on a 3T scanner

* from a human

* from an ostensibly healthy adult

**If your data are not so, then you should check with the TORTOISE
folks about options and flags to use in the commands.** Even if your
data *do* match these characteristics, you should probably be in touch
with them about finer points of this processing analysis.  Here, we
try to work closely with the TORTOISE folks, to keep in touch with the
latest-and-greatest updates and to represent those here, but it is
worth being veeery clear about fine details in processing.

.. note:: **Disclaimer:** while we work closely with the TORTOISE
          folks, trying to keep in touch with the latest-and-greatest
          updates and to represent those here, we are *not* full
          experts in it-- and it is worth being veeery clear about
          fine details in processing.  All TORTOISE-related questions
          about options or problems should be addressed to the FS
          gurus themselves.  We are happy to cc'ed or involved in
          discussions, and any feedback on things to do differently
          would be welcomed and gladly discussed on our end.

          As noted previously the TORTOISE folks have provided
          valuable feedback and input here, so we appreciate them for
          that.

          Finally, we note that there are some differences between the
          official TORTOISE gurus' recommendations for using TORTOISE
          tools and what we do here.  For example, they recommend
          loading DICOMs directly into TORTOISE, so that they can deal
          with header information directly; if weird things start
          happening in your data, please consider this. (*Most
          importantly, again-- please keep looking at your data to
          know what is happening at each step along the way!*)

.. note:: In the Dark Ages TORTOISE either required an IDL license to
          run in batchable mode or was run subject-by-subject by
          clicking through an IDL VM GUI (ick).  A PDF describing
          **a** system of steps for processing with TORTOISE v2.5.2 is
          provided here:

          :download:`Running_TORTOISE_v2.5.2.pdf
          <media/tort_v2.5.2/Running_TORTOISE_v2.5.2.pdf>`.

          for the time being, in part for the nostalgia of youth.
          Earlier versions of TORTOISE (v2.5.1 and previous) have
          greater subtlety in processing, and are entirely ignored.

          We don't comment any further about this version of
          processing here.  

|

.. _fp_preproc_tort_diffprep:

**DIFFPREP**
------------


**INPUT** for TORTOISE: there is some flexibility here.  The above
notes describe taking a pair of DWI data sets with oppositely encoded
phases (AP-PA). The instructions can also be applied for when just a
single DWI data set (i.e., only one phase encoding direction-- either
AP or PA, or anything else for that matter) is obtained. In either
case a reference anatomical volume (either a real T2w volume, or an
imitation one as described :ref:`here <IRCT_invert>`) should also be
included.

**OUTPUT** from TORTOISE: again, there is flexibility here.  Default
for us will be to export a single 4D data set of *N* DWIs (DWI.nii)
and their *N* DWI gradient/*b*\-value information (BMTXT_AFNI.txt).
This is the case even if we put in AP-PA data, or just a single phase
encoded set. The *b*\-value information is output as a *b*\-matrix
text file, and this includes information of any rotations made to
volumes during processing.

   

