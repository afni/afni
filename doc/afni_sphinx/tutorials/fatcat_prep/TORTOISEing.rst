.. _TORTOISEing:


Running TORTOISE
================

.. contents::
   :depth: 3


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
imitation one as described :ref:`here <IRCT_invert>`) should also be
included.

**OUTPUT** from TORTOISE: again, there is flexibility here.  Default
for us will be to export a single 4D data set of *N* DWIs (DWI.nii)
and their *N* DWI gradient/*b*\-value information (BMTXT_AFNI.txt).
This is the case even if we put in AP-PA data, or just a single phase
encoded set. The *b*\-value information is output as a *b*\-matrix
text file, and this includes information of any rotations made to
volumes during processing.

.. warning:: We note that versions of TORTOISE **before** v2.5.2
             (="old," here) contained a slightly different output
             formats from DIFF_CALC.

             1) The old *b*\-matrix that wasn't in the full AFNI
                format (though it could be converted to one fairly
                easily).  Those were called "BMTXT.txt", and you can
                read more about them `here
                <https://afni.nimh.nih.gov/afni/community/board/read.php?1,151518,151518#msg-151518>`_
                on the AFNI MB, particularly how to deal with the old
                format efficiently.

             2) The old DWI.nii file automatically had the b=0 volumes
                averaged together, and the resulting average was
                placed at the [0]th brick of the output
                file. Nowadays, this is not done (but it is
                straightforward to mimic the old behavior, as will be
                shown in :ref:`postTORTOISEing`).



   


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

