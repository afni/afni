.. _fp_postpre_II:

Post-preproc, II: mapping ROIs to DTI space
===========================================

.. contents::
   :depth: 3

Overview
--------

We might be interested in bringing other data into the processed
DWI/DT space.  For example, we could have FreeSurfer (FS) surfaces and
parcellations from an anatomical T1w volume (and we do!), which have
already been brought into AFNI+SUMAable formats by
``@SUMA_Make_Spec_FS``.  We can choose to bring just the volumetric
part of that (e.g., the segmentation+parcellation NIFTI files), or we
could bring both the volumes and surfaces. For the latter, this can
include the "\*.gii" surfaces themselves, as well as the supplementary
information in the "\*.niml.dset" files (such as the
annotation/labelling of nodes) and the "\*.spec" files (which describe
how all the surface states relate and which annotation files go with
each).

The above describes the role of ``fat_proc_map_to_dti``. In the
parlance of our times, we create a transformation map to the DWI's
[0]th volume from the FS-output T1w volume, and then we can
apply that mapping to the other "follower" volume, surface,
supplementary, etc. files.  The transformation is a 12 DOF linear
affine transform (made using ``3dAllineate``).  

One specifies the followers by their category. For volume files, one
can select whether the dset contains integers that should *stay*
integers (possibly with labeltables) with nearest neighbor ("NN")
resampling, or whether it has floating points values that can be
interpolated (with "wsinc5").  For surfaces, one specifies surface
files, supplementary NIML dsets and specification files separately;
however, we note that it only makes sense to map, say, \*.spec files
if one has matching surfaces and \*.niml.dset files\.\.\.

.. note:: In this example we map from a FS-processed T1w
          volume to DT space, bringing along FS parcellation
          maps and surfaces as followers.  

          However, one could also use this program to bring, say, FMRI
          data into DT space, since most FMRI processing pipelines
          involve aligning EPI to the subject's anatomical. One could
          make a map from the subject's T1w anatomical and have the
          FMRI maps be follower dsets.

.. _fp_postproc_map_to_dti:

**fat_proc_map_to_dti**
-----------------------

**Proc:** One specifies a source (here, the skull-stripped "brain.nii"
anatomical from FreeSurfer) and a base (the reference :math:`b_0` file
that is likely the [0]th brick in the post-TORTOISE DWI data set) for
making the transformation. 

One then can specify volumetric and surfacey follower data sets by
category.  Here, we select all the "renumbered" parcellation
volumetric files (see ``@SUMA_renumber_FS``, which is run as part of
``@SUMA_Make_Spec_FS`` for more info); many of these aren't likely to
be directly useful since they contain maps of "unknown" or "other"
regions from FS, but they don't take up much disk space when
compressed.  These will all have auto-snapshot images made of them in
the new space.

Finally, we chose to map the higher resolution standardized mesh
surfaces ("ld 141") from ``@SUMA_Make_Spec_FS``, as well as their
accompanying \*.niml.dsets and \*.spec files, for simpler viewing
later. At present, there is not auto-snapshot capability for SUMA
viewing; we describe some simplish ways to use the \*.spec files to
view the output, below.

This was all done by running (note the use of wildcards in selecting
many dsets per follower type)::

    # I/O path, same as above, following earlier steps
    set path_P_ss = data_proc/SUBJ_001

    fat_proc_map_to_dti                                                \
        -source          $path_P_ss/anat_02/SUMA/brain.nii             \
        -followers_NN    $path_P_ss/anat_02/SUMA/aparc*_REN_*.nii.gz   \
        -followers_surf  $path_P_ss/anat_02/SUMA/std.141.*.gii         \
        -followers_ndset $path_P_ss/anat_02/SUMA/std.141.*.niml.dset   \
        -followers_spec  $path_P_ss/anat_02/SUMA/std.141.*.spec        \
        -base            $path_P_ss/dwi_05/dwi_dwi.nii.gz'[0]'         \
        -prefix          $path_P_ss/dwi_05/indt

-> putting all of the outputs into the existing
'data_proc/SUBJ_001/dwi_05/' directory, since all the files there
should be in the same DT space:

.. list-table:: 
   :header-rows: 1
   :widths: 90

   * - Directory substructure for example data set
   * - .. image:: media/postpre_ii/fp_12_map_to_dti_files.png
          :width: 100%
          :align: center
   * - *Output from fat_proc_map_to_dti.*

|

!!!!!!!!!!!

.. list-table:: 
   :header-rows: 1
   :widths: 20 80
   :stub-columns: 0

   * - Outputs of
     - ``fat_proc_dwi_to_dt``
   * - **indt_cmd.txt**
     - textfile, copy of the command that was run, and location
   * - **indt.nii.gz**
     - volumetric NIFTI file, 3D; the "source" dset (here, the
       skull-stripped T1w output from FS) in the DT space; how well
       this aligns to the "base" diffusion volume determines the
       appropriateness of the transformation/mapping.
   * - **indt_map_allin.aff12.1D**
     - textfile, the 12 DOF linear affine registration parameters for
       the mapping process; gets applied to all the volumetric
       followers, and its inverse gets applied to the surface
       followers (AFNI and SUMA transforms work in opposite ways).
   * - **indt_aparc+aseg_REN_\*.nii.gz**
     - volumetric NIFTI files, 3D; each contains a parcellation and/or
       segmentation map from the FS "2000" parcellation, renumbered by
       ``@SUMA_renumber_FS`` into one of the following categories:
       all, csf, gm, othr, unkn, vent, wmat.
   * - **indt_aparc.a2009s+aseg_REN_\*.nii.gz**
     - volumetric NIFTI files, 3D; each contains a parcellation and/or
       segmentation map from the FS "2009" parcellation, renumbered by
       ``@SUMA_renumber_FS`` into one of the following categories:
       all, csf, gm, othr, unkn, vent, wmat.
   * - **indt_std.141.\*.gii**
     - surface GIFTI files; FS surfaces translated over, in this case
       the "ld 141" version of the standardized surface from
       ``@SUMA_Make_Spec_FS``.
   * - **indt_std.141.\*.niml.dset**
     - NIML format surface data sets; these contain things like
       parc/seg labeltables, etc. (made by ``@SUMA_Make_Spec_FS``).
   * - **indt_std.141.\*.spec**
     - specification files for defining relative states of surface
       dsets, as well as matching the \*.niml.dset label dsets with
       the appropriate surfaces (made by ``@SUMA_Make_Spec_FS``).
   * - **indt__qc00_base_u_esrc.\*.png**
     - autoimages, multiple slices within single volume; ulay =
       reference [0]th DWI volume (b/w); olay = FS structural file
       brain.nii, edgified (red); use these images to judge the
       quality of alignment.
   * - **indt__qc01_base_u_src.\*.png**
     - autoimages, multiple slices within single volume; ulay =
       reference [0]th DWI volume (b/w); olay = FS structural file
       brain.nii (translucent, "plasma" colorbar); use these images to
       judge the quality of alignment.
   * - **indt__qc_aparc+aseg_REN_\*.\*.png,
       indt__qc_aparc.a2009s+aseg_REN_\*.\*.png**
     - autoimages, multiple slices within single volume; ulay =
       reference [0]th DWI volume; olay = FS parcellation/segmentation
       maps for a given tissue grouping/classification (translucent,
       "ROI_i256" colorbar); can also use these images to judge the
       quality of alignment, as well as the parcellation/segmentation
       itself.

|

.. list-table:: 
   :header-rows: 1
   :widths: 50 50

   * - Autoimages of ``fat_proc_map_to_dti`` 
     - (just axi and sag views)
   * - .. image:: media/postpre_ii/indt__qc_aparc+aseg_REN_gm.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/postpre_ii/indt__qc_aparc+aseg_REN_gm.sag.png
          :width: 100%   
          :align: center

.. list-table:: 
   :header-rows: 0
   :widths: 100

   * - *FS "2000" parc/seg map: the GM ROIs from AFNI renumbering
       (translucent olay) [0]th DWI volume as (b/w ulay).*

.. list-table:: 
   :header-rows: 0
   :widths: 50 50

   * - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_gm.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_gm.sag.png
          :width: 100%   
          :align: center

.. list-table:: 
   :header-rows: 0
   :widths: 100

   * - *FS "2009" parc/seg map: the GM ROIs from AFNI renumbering
       (translucent olay) [0]th DWI volume as (b/w ulay).*

.. list-table:: 
   :header-rows: 0
   :widths: 50 50

   * - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_wmat.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_wmat.sag.png
          :width: 100%   
          :align: center

.. list-table:: 
   :header-rows: 0
   :widths: 100

   * - *FS "2009" parc/seg map: the WM ROIs from AFNI renumbering
       (translucent olay) [0]th DWI volume as (b/w ulay).*


.. list-table:: 
   :header-rows: 0
   :widths: 50 50

   * - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_csf.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_csf.sag.png
          :width: 100%   
          :align: center

.. list-table:: 
   :header-rows: 0
   :widths: 100

   * - *FS "2009" parc/seg map: the CSF ROIs from AFNI renumbering
       (translucent olay) [0]th DWI volume as (b/w ulay).*


.. list-table:: 
   :header-rows: 0
   :widths: 50 50

   * - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_vent.axi.png
          :width: 100%   
          :align: center
     - .. image:: media/postpre_ii/indt__qc_aparc.a2009s+aseg_REN_vent.sag.png
          :width: 100%   
          :align: center

.. list-table:: 
   :header-rows: 0
   :widths: 100

   * - *FS "2009" parc/seg map: the ventricle ROIs from AFNI
       renumbering (translucent olay) [0]th DWI volume as (b/w ulay).*
