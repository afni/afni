.. _FreeSurfering:


Preproc: FreeSurfer (+ @SUMA_Make_Spec_FS)
==================================================

.. contents::
   :depth: 3

.. highlight:: Tcsh

Re. recon-all (FS)
------------------

This stage describes preprocessing the (T1w) anatomical volume
primarily using `FreeSurfer (FS) <https://surfer.nmr.mgh.harvard.edu/>`_.
This provides information such as:

* whole brain segmentation+parcellation (two versions: '2000' and
  '2009'; user's choice on what to use)
* tissue maps
* skull-stripping
* surface mesh estimation (with parc+seg labels attached)

\.\.\. and probably some other useful data that I am forgetting.  For
DTI-related applications, we mainly make use of the parc+seg maps and
the surface meshes, created using their ``recon-all`` function with
their defaults.  This documentation was created using the current
version of FS, v6.0.

In preparation for running ``recon-all``, at present it seems like
there are some important properties for the volume to have, in terms
of spatial resolution and matrix dimensions.  Please see
":ref:`fp_align_anat_pair`" for a description, even if you decide not
to use that function to accomplish some of those things. 

.. note:: **Disclaimer:** while we like using FS and some functions
          therein, we are *not* experts in it-- all FS-related
          questions about options or problems should be addressed to
          the FS gurus themselves.  Any feedback on things to do
          differently would be welcomed and gladly discussed on our
          end.

          The FS folks have provided some useful feedback on questions
          that have come up related to this work, so thanks to them
          for that.  

          Finally, we note that there are some differences between FS
          and AFNI in grouping FS parc+seg ROI maps into tissue maps,
          which are described in the next section.

Re. @SUMA_Make_Spec_FS (AFNI-SUMA)
----------------------------------

After running ``recon-all``, then the SUMA function
``@SUMA_Make_Spec_FS`` in AFNI is used to translate all the volumetric
and surface data into formats usable in AFNI (i.e., to NIFTI, GIFTI,
etc.).  It also translates the FS-generated surface meshes into
"standard meshes."  Additionally, as a final step it executes the
``@SUMA_renumber_FS`` script that makes tissue-based maps from the FS
parc+seg labelling for each of the 2000- and 2009-map versions:

* \*_REN_gm.nii.gz    :gray matter
* \*_REN_wmat.nii.gz  :white matter
* \*_REN_csf.nii.gz   :cerebrospinal fluid
* \*_REN_vent.nii.gz  :ventricles and choroid plexus
* \*_REN_othr.nii.gz  :optic chiasm, non-WM-hypointens, etc.
* \*_REN_unkn.nii.gz  :FS-defined "unknown", with voxel value >0

The lists for renumbering and grouping can be viewed `here for the
2000
<https://afni.nimh.nih.gov/pub/dist/src/scripts_install/afni_fs_aparc+aseg_2000.txt>`_
and `here for the 2009
<https://afni.nimh.nih.gov/pub/dist/src/scripts_install/afni_fs_aparc+aseg_2009.txt>`_
FS parc+seg maps.  The main difference between these and the FS tissue
segmentations is that, at present, the ROIs classified as 'othr'
(Left-vessel, Right-vessel, non-WM-hypointensities, and Optic-Chiasm)
are not excluded from the FS gray matter map using ``mri_binarize
--gm``.

|

.. _fp_preproc_fs_@suma:

**recon-all** and **@SUMA_Make_Spec_FS**
----------------------------------------

**Proc:** Running FS's ``recon-all`` takes a fair amount of time (of
order several hours and upward, depending on machine), but the default
implementation is pretty straightforward, mainly requiring a T1w
volume as input.  After running that, the SUMA function
``@SUMA_Make_Spec_FS`` in AFNI is also quite direct to run, mainly
just needing to know where the FS-generated set of directories are for
a subject (though, not the inclusion of the ``-NIFTI`` flag, which is
recommended).  Therefore, the following can be run in succession::

    # I/O path, same as above, following earlier steps
    set path_P_ss = data_proc/SUBJ_001

    # ID of individual subj from "tail" of path, above
    set ss_id   = $path_P_ss:t

    # FS function
    recon-all                                     \
        -all                                      \
        -sd       $path_P_ss                      \
        -subjid   anat_02                         \
        -i        $path_P_ss/anat_01/t1w.nii.gz

    # AFNI-SUMA function: convert FS output
    @SUMA_Make_Spec_FS                            \
        -NIFTI                                    \
        -fspath $path_P_ss/anat_02                \
        -sid    $ss_id

-> produces a set of subdirectories in the new directory
'data_proc/SUBJ_001/anat_02/', as well as
'data_proc/SUBJ_001/fsaverage/'; the AFNI- and SUMA-compatible files
are contained in the subdirectory 'data_proc/SUBJ_001/anat_02/SUMA/'.

.. list-table:: 
   :header-rows: 1
   :widths: 90

   * - Directory substructure for example data set
   * - .. image:: media/fp_05_fs_@suma.png
          :width: 100%
          :align: center
   * - *Output subdirectories made by FS's recon-all and SUMA's
       @SUMA_Make_Spec_FS: the latter function makes the listed
       directory "SUMA", and the former function makes the others.*


.. list-table:: 
   :header-rows: 1
   :widths: 90

   * - Directory substructure for example data set
   * - .. image:: media/fp_05_@suma_files.png
          :width: 100%
          :align: center
   * - *Output files in the SUMA directory made by @SUMA_Make_Spec_FS.*
