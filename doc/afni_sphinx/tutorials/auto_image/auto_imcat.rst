.. _tut_auto_imcat:

Using imcat
===========

.. contents::
   :depth: 3

.. highlight:: Tcsh


``imcat`` is an "image concatenation" program.  This is a *very* useful
supplementary tool for glueing existing images together to make arrays
with separating lines, as well other features.  Combines quite
usefully with ``@chauffeur_afni``, for example to concatenate similar
images across a data set.

Examples
--------

We present some examples of auto-image making with ``imcat`` using
data that should be available in (modern) AFNI binary distributions,
the ``*_SSW.nii.gz`` template targets for ``@SSwarper``.  Each of
these dsets has multiple bricks.  We provide examples of combining: 

* multiple views and slices of the same dataset

* similar views across separate datasets

* and more

Each short script creates a subdirectory ("QC_imcat\*") for the both
the individual, intermediate images and the final concatenated matrix
of images.  

Typically, one could adapt these scripts by simply changing the file
name(s) in the uppermost input section.

|

**Ex. 0**: Each subject & all sliceviews;  each sliceview & all subjects
----------------------------------------------------------------

Two examples of using ``imcat`` on a set of datasets:

* **A**: for each dataset, concatenate different slice views (sagittal,
  coronal and axial) of a single volume

* **B**: for each slice view, show the data set at the same (x, y, z)
  location.

This example requires having the ``*_SSW.nii.gz`` template targets
copied into the present working directory.  Alternatively, one could
just include the path to them in the glob at the top of the script
(e.g., ``set ivol  = `\ls ~/abin/*${istr}.nii.gz```)

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

   #!/bin/tcsh

   # Auto-image-making example: imcat
   #
   # Here, concatenate (= glue together) images made with
   # @chauffeur_afni.  
   #
   # For an easily available set of images, here we use the
   # "*_SSW.nii.gz" reference templates distributed in AFNI, that are
   # used for @SSwarper (the combo-tool for performing nonlinear
   # registration and skull stripping insieme). 
   #
   # By changing the the list of files given to "$istr", this can be
   # directly adapted to other cases.
   #
   # =====================================================================

   set here = $PWD

   # Some set of volumes, and count how many there are
   set istr   = "SSW"
   set ivol  = `\ls *${istr}.nii.gz`
   set Nvol  = $#ivol

   # RGB color for a line separating images in the final matrix.  Here,
   # white.
   set lcol  = ( 255 255 255 )                           

   # Output directory to store images
   set odir  = $here/QC_imcat_00_${istr}
   \mkdir -p $odir

   # ----------- Make sets of individual images: @chauffeur_afni ----------

   # Make a set of sagittal, axial and coronal images; each "image"
   # output by chauffer here is actually an 8x1 montage.  These will
   # later be glued together.  

   set allbase = ()

   foreach ff ( $ivol )
       # base name of vol, and make a list of all prefixes for later
       set ibase = `3dinfo -prefix_noext "${ff}"`
       set allbase = ( $allbase $ibase )

       # Make a montage of the zeroth brick of each image
       @chauffeur_afni                                               \
           -ulay       "${ff}[0]"                                    \
           -prefix     $odir/img0_${ibase}                           \
           -montx 8 -monty 1                                         \
           -set_dicom_xyz   5 18 18                                  \
           -delta_slices   10 20 10                                  \
           -set_xhairs     OFF                                       \
           -label_mode 1 -label_size 3                               \
           -do_clean  
   end

   # ------------------- Glue together images: imcat ---------------------

   # Combine the individual images from above into a matrix of images.
   # Two examples are presented here, one "per subject" and one "per
   # sliceview". In both cases, we are just stacking the above images in
   # a single column.

   # Just the "gap color" between glued-together images
   set lcol  = ( 66 184 254 )

   # A) For each volume, concatenate images across all sliceviews.  The
   # order of contanenation will be that of globbing; could be specified
   # in different ways, too.
   foreach ff ( $allbase ) 
       imcat                                                               \
           -echo_edu                                                       \
           -gap 5                                                          \
           -gap_col $lcol                                                  \
           -nx 1                                                           \
           -ny 3                                                           \
           -prefix $odir/ALL_subj_${ff}.jpg                                \
           $odir/img0_*${ff}*
   end

   # Just the "gap color" between glued-together images
   set lcol  = ( 255 152 11 )

   # B) For each sliceview, concatenate images across all vols
   foreach ss ( "sag" "cor" "axi" ) 
       imcat                                                               \
           -echo_edu                                                       \
           -gap 5                                                          \
           -gap_col $lcol                                                  \
           -nx 1                                                           \
           -ny $Nvol                                                       \
           -prefix $odir/ALL_${istr}_sview_${ss}.jpg                       \
           $odir/img0_*${ss}*
   end

   # ---------------------------------------------------------------------

   echo "++ DONE!"

   # All fine
   exit 0

.. list-table:: 
   :header-rows: 1
   :widths: 100 

   * - Example 0-A
   * - HaskinsPeds_NL_template1.0_SSW:
   * - .. image:: media/QC_imcat/ALL_subj_HaskinsPeds_NL_template1.0_SSW.jpg
          :width: 100%   
          :align: center
   * - MNI152_2009_template_SSW:
   * - .. image:: media/QC_imcat/ALL_subj_MNI152_2009_template_SSW.jpg
          :width: 100%   
          :align: center
   * - TT_N27_SSW:
   * - .. image:: media/QC_imcat/ALL_subj_TT_N27_SSW.jpg
          :width: 100%   
          :align: center

|

.. list-table:: 
   :header-rows: 1
   :widths: 100 

   * - Example 0-B
   * - sagittal views:
   * - .. image:: media/QC_imcat/ALL_SSW_sview_sag.jpg
          :width: 100%   
          :align: center
   * - coronal views:
   * - .. image:: media/QC_imcat/ALL_SSW_sview_cor.jpg
          :width: 100%   
          :align: center
   * - axial views:
   * - .. image:: media/QC_imcat/ALL_SSW_sview_axi.jpg
          :width: 100%   
          :align: center

|

**Ex. 1**: Each volume of a 4D dataset & all sliceviews
-------------------------------------------------------

Take a multi-volume dataset, and look at each volume in each sliceview.

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

   #!/bin/tcsh

   # Auto-image-making example: imcat
   #
   # Here, concatenate (= glue together) images made with
   # @chauffeur_afni.  
   #
   # Another example using one of the "*_SSW.nii.gz" reference templates
   # distributed in AFNI.  Here, we view multiple subbricks of the dset.
   #
   # By changing the the file given to "$ivol", this can be directly
   # adapted to other cases.
   #
   # =====================================================================

   set here = $PWD

   set ivol  = MNI152_2009_template_SSW.nii.gz         # volume de choix
   set ibase = `3dinfo -prefix_noext "${ivol}"`        # base name of vol
   set nv    = `3dinfo -nv "${ivol}"`                  # number of vols
   set imax  = `3dinfo -nvi "${ivol}"`                 # max index

   # RGB color for a line separating images in the final matrix.  Here,
   # we use a nice green (but it could also be something boring, if you
   # prefer).
   set lcol  = ( 0 204 0 )                           

   # output directory to store images
   set odir  = $here/QC_imcat_01_${ibase}
   \mkdir -p $odir

   # ----------- Make sets of individual images: @chauffeur_afni ----------

   # 1) Make a set of sagittal, axial and coronal images; these will
   #    later be glued together.  Here, we are make a set of images per
   #    volume in a 4D data set.

   foreach ii ( `seq 0 1 $imax` )
       # zeropadded numbers, nicer to use in case we have a lot of images
       set iii = `printf "%03d" $ii`

       # This if-condition is a sidestep: we have two categories of data
       # in the input volume, masks and dsets, with very different
       # pertinent ranges, so we account for that here.
       if ( $ii > 2 ) then
           set UMIN = "0"
           set UMAX = "1"
       else
           set UMIN = "2%"
           set UMAX = "98%"
       endif

       @chauffeur_afni                                               \
           -ulay       "${ivol}[$ii]"                                \
           -ulay_range "$UMIN" "$UMAX"                               \
           -prefix     $odir/${ibase}_${iii}                         \
           -montx 1 -monty 1                                         \
           -set_dicom_xyz   2 18 18                                  \
           -delta_slices   25 25 25                                  \
           -set_xhairs     OFF                                       \
           -label_mode 1 -label_size 3                               \
           -do_clean  
   end

   # ------------------- glue together images: imcat ---------------------

   # 2) Combine the individual images from above into a matrix of images.
   #    Here we have three rows (i.e., three images along y-axis: one for
   #    sagittal, axial and coronal), and the number of columns is equal
   #    to the number of volumes in the 4D dset.

   imcat                                                               \
       -echo_edu                                                       \
       -gap 5                                                          \
       -gap_col $lcol                                                  \
       -nx $nv                                                         \
       -ny 3                                                           \
       -prefix $odir/ALL_vol_${ibase}.jpg                              \
       $odir/${ibase}*sag* $odir/${ibase}*cor* $odir/${ibase}*axi*

   # ---------------------------------------------------------------------

   echo "++ DONE!"

   # All fine
   exit 0

|

.. list-table:: 
   :header-rows: 1
   :widths: 100 

   * - Example 1
   * - HaskinsPeds_NL_template1.0_SSW:
   * - .. image:: media/QC_imcat/ALL_vol_MNI152_2009_template_SSW.jpg
          :width: 100%   
          :align: center

|

**Ex. 2**: View coef+stat info of each subject in a group
---------------------------------------------------------

Do you ever process a group of subjects?  If so, then the following
example might be for you!  

Here, we get an overview of all individual subject modeling results
from an FMRI study for a particular contrast.  As is good practice, we
display the effect estimates ("beta coefficients") from the models,
and just use the statistic for thresholding.  File name prefixes are
echoed into the text string at the top of each panel (in the
``@chauffeur_afni`` command), so subjects can be more easily
identified in the final image.

Additionally, for informational purposes we apply an overlay feature
whereby we can still see some sub-threshold data translucently, so we
a bit more information than the standard application of the
(reasonably arbitrary) thresholding would permit.

.. hidden-code-block:: Tcsh
   :starthidden: False
   :label: - show code y/n -

   #!/bin/tcsh

   # Auto-image-making example: imcat
   #
   # Here, concatenate (= glue together) images made with
   # @chauffeur_afni.  
   #
   # This example shows how to look at individual stat data together
   # across a group.  We use the freely available AFNI Bootcamp data in
   # the present script, in particular "AFNI_data6/group_results/REML*".
   #
   # By changing the the list of files given to "$ivol", this can be
   # directly adapted to other cases.  Depending on how you unpacked your
   # Bootcamp data, you might need to adjust the "$idir" variable, too.
   #
   # =====================================================================

   set here = $PWD

   # Some set of volumes, and count how many there are
   set istr   = "REML"
   set idir   = "~/AFNI_data6/group_results"
   set ivol   = `\ls ${idir}/${istr}*HEAD`
   set Nvol   = $#ivol
   set imask  = "${idir}/mask+tlrc.HEAD"
   set ianat  = "${idir}/FT_anat+tlrc.HEAD"

   # RGB color for a line separating images in the final matrix.  Here,
   # white.
   set lcol  = ( 192 192 192 )                           

   # Output directory to store images
   set odir  = $here/QC_imcat_02_${istr}
   set wdir  = $odir/__WORKDIR_${istr}
   \mkdir -p $wdir

   # ----------- Make sets of individual images: @chauffeur_afni ----------

   # Make a set of sagittal, axial and coronal images; each "image"
   # output by chauffer here is actually an 8x1 montage.  These will
   # later be glued together.  

   set allbase = ()

   foreach ff ( $ivol )
       # base name of vol, and make a list of all prefixes for later
       set ibase = `3dinfo -prefix_noext "${ff}"`
       set allbase = ( $allbase $ibase )

       ### Make a montage of the zeroth brick of each image.  
       # Some fun-ness here: part of each file's name is added to the
       # label string shown in each panel.
       # Note: these olay datasets are unclustered and unmasked.
       @chauffeur_afni                                               \
           -ulay       ${ianat}                                      \
           -ulay_range "2%" "130%"                                   \
           -olay       ${ff}                                         \
           -set_subbricks -1 0 1                                     \
           -func_range 3                                             \
           -thr_olay_p2stat 0.001                                    \
           -thr_olay_pside  bisided                                  \
           -cbar    Reds_and_Blues_Inv                               \
           -alpha_par Quadratic                                      \
           -opacity 7                                                \
           -prefix     $odir/img0_${ibase}                           \
           -montx 1 -monty 1                                         \
           -set_dicom_xyz  5 18 18                                   \
           -set_xhairs     OFF                                       \
           -label_string "::${ibase}"                                \
           -label_mode 1 -label_size 3                               \
           -do_clean  
   end

   # ------------------- Glue together images: imcat ---------------------

   # Combine the individual images from above into a matrix of images.
   # Here we combine similar slice views.  Note how we now have a nice
   # summary of subject modeling results across the group.

   foreach ss ( "sag" "cor" "axi" ) 
       imcat                                                               \
           -echo_edu                                                       \
           -gap 5                                                          \
           -gap_col $lcol                                                  \
           -nx 5                                                           \
           -ny 2                                                           \
           -prefix $odir/ALL_${istr}_sview_${ss}.jpg                       \
           $odir/img0_*${ss}*
   end

   # Note about above: the 'nx' and 'ny' values are hardcoded in, but
   # they needn't be, so this could be more flexible to match
   # adding/subtracting subjects.  Fancier things can be done-- feel free
   # to ask/discuss/recommend suggestions.

   # ---------------------------------------------------------------------

   echo "++ DONE!"

   # All fine
   exit 0

|

.. list-table:: 
   :header-rows: 1
   :widths: 100 

   * - Example 2
   * - sagittal views:
   * - .. image:: media/QC_imcat/ALL_REML_sview_sag.jpg
          :width: 100%   
          :align: center
   * - coronal views:
   * - .. image:: media/QC_imcat/ALL_REML_sview_cor.jpg
          :width: 100%   
          :align: center
   * - axial views:
   * - .. image:: media/QC_imcat/ALL_REML_sview_axi.jpg
          :width: 100%   
          :align: center

|

One could also leave out the ``-alpha_par Quadratic`` option above and
select just the subject-specific part of the filename from ``$ibase``
above, yielding something more like the following:

If the ``-alpha_par Quadratic`` option were left out of the above
``@chauffeur_afni`` calls, then the images would look more like the
following.

.. list-table:: 
   :header-rows: 1
   :widths: 100 

   * - Example 2 (tweaked views)
   * - sagittal views:
   * - .. image:: media/QC_imcat/ALL_REML_sview_sag_B.jpg
          :width: 100%   
          :align: center
   * - coronal views:
   * - .. image:: media/QC_imcat/ALL_REML_sview_cor_B.jpg
          :width: 100%   
          :align: center
   * - axial views:
   * - .. image:: media/QC_imcat/ALL_REML_sview_axi_B.jpg
          :width: 100%   
          :align: center

|
