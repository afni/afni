.. _tempatl_framework:

****************
**AFNI Atlases**
****************

A brief overview and description of how AFNI handles atlases,
templates, spaces and transformations.

.. contents::
   :depth: 3

Atlases and AFNI
----------------

As part of a large group of changes to the way AFNI deals with
templates and atlases, AFNI now includes expanded support for a
virtually unlimited selection of atlases, standard templates, template
spaces and transformations. The primary resource for defining how AFNI
handles the various aspects of atlas and template space usage is
determined by referencing a central file, AFNI_atlas_spaces.niml. This
scheme allows users to create atlases and templates relatively
easily. The descriptions below outline the configuration and usage of
this file for some common examples.

If you are interested in adding or making a new atlas, skim through
this page and then read :ref:`this webpage <tempatl_howto>`.

First, a few brief definitions are needed in order to avoid confusion
and to establish the terminology used throughout this document and
AFNI help.

**Atlas**
  Dataset that provides some level of segmentation, i.e. structures
  corresponding to a fixed intensity level or, for probabilistic
  atlases, to a particular volume. Some examples include the Talairach
  Daemon and the Eickhoff-Zilles cytoarchitectonic probabilistic
  atlases.

**Template**
  A reference dataset used as a basis for alignment. Examples are the
  N27 brain, the MNI-152, MNI-452 brains.

**Space**
  A correspondence with some template, loosely meaning fitting in the
  same "shoebox" as the template. Spaces may include TLRC, MNI,
  MNI_ANAT, ORIG.

**Transformation**
  The method used to convert data or coordinates from one space to
  another. Current transformation types include affine
  transformations, 12-piece transformations from manual Talairach
  procedures, two-piece transformations.

In the NIML file, four different kinds of entries may be entered
corresponding to the four definitions just introduced above. Each
entry is discussed in more detail below.



Re. **Spaces**
----------------------

#. *Space definition*

   Datasets are now marked with an attribute, TEMPLATE_SPACE, that
   marks them with the space to which they are aligned. To be clear,
   the "space" is a description of location associated with some
   template. The space is used to indicate a spatial correspondence
   among all datasets that share the same space. So any position in
   one space should be the same as the corresponding position in
   another dataset.

   Not only are the subject datasets marked with a space, the template
   and atlas datasets used by AFNI are also marked with a space. The
   space is either determined by the template itself or inherited from
   another dataset. AFNI is distributed with templates and atlases in
   TLRC, MNI and MNI_ANAT spaces. Each space is defined in the
   AFNI_ATLAS_SPACES.niml file as in the following example::

     <TEMPLATE_SPACE
      space_name="TT_N27"
      generic_space="TLRC"
      comment="The TLRC templates manually talairached from MNI group templates"
      ></TEMPLATE_SPACE>

   Note the definition of the space includes only three parts -- the
   name of the space, a generic equivalent along with a descriptive
   comment. In this case, the TT_N27 is the name of the space for
   datasets aligned with a manually Talairched version of the
   Colin_N27 dataset. Because it has been Talairached, it also has
   correspondence with other Talairach datasets, so the generic space
   may be assigned to be "TLRC".

#. *Aligning data to a template and spaces*

   Support for these spaces is included with @auto_tlrc so that
   alignment to a specific template will assign the template's space
   to the aligned dataset. Each of the templates distributed with AFNI
   that include "TT" in the prefix are in Talairach space
   (TLRC). "MNI" in the template prefix signifies MNI space. See the
   datasets included in the afni binary directory for the full
   list. 

   In usage mode 2, @auto_tlrc will carry over the parent dataset's
   space to the child dataset. Similarly, adwarp will also apply the
   space of the parent dataset to the child output. Other programs
   that do alignment and transformation, like align_epi_anat.py,
   3dAllineate, 3dvolreg, 3dWarpDrive, 3dWarp, 3drotate, 3dresample,
   3dfractionize, ..., do not update the space to a template
   explicitly. For these, the program may include options for a master
   dataset. In those cases, the output should match the space of the
   master dataset. In other cases, you may need to manually set the
   space of the output dataset.

#. *Seeing and changing the space of a dataset*

   A few utilities provide support for use of spaces. To find out the
   space of a dataset, use the 3dinfo command. 3dinfo will report the
   template space at the beginning of its output. To assign or reassign a
   space to a dataset, use the command 3drefit -space
   (ORIG/ACPC/TLRC/MNI/MNI_ANAT).

#. *Implicit use of space and inheritability*

   Other programs in AFNI should allow the automatic inheritance of
   the space attribute from the master or first dataset input. 3dcalc,
   3dcopy and others should follow this behavior. Not all programs
   have been verified to follow this behavior, so please check.

#. *NIFTI support*

   The NIFTI format includes support for Talairach and MNI space in
   the qform and s-form code fields. AFNI will assign the
   corresponding space to the dataset and inherited datasets. Note
   NIFTI does not include a code for MNI_ANAT space.

#. *"whereami" in the AFNI GUI*

   The AFNI GUI uses the template space of the dataset in the
   interactive ``whereami`` GUI when computing the current location by
   transforming the position in the dataset's space to the position in
   the space of each of the atlases defined in the
   AFNI_atlas_spaces.niml. The transformations between MNI and TLRC
   spaces are computed using the Brett transform, a two-step affine
   transformation procedure. The coordinates are reported in the
   ``whereami`` GUI for a selected group of spaces. By default, the
   coordinates are displayed in TLRC, MNI and MNI_ANAT spaces, but
   this selection of spaces may be set using the environment variable,
   AFNI_TEMPLATE_SPACE_LIST (see environment variable section for
   details).

#. *"whereami" on the command line*

   The ``whereami`` command has the option ``-space`` to include
   support for coordinate input in any of the allowed
   spaces. Alternatively, the ``-dset`` option supplied with a dataset
   name will use the space of the dataset to compute the
   transformations to use with its atlases.

#. *Environment variables and default behavior*

   Wherever the dataset has no space explicitly set, the space is
   assumed from the "view" part of the dataset name. For example, if
   the dataset, anat+tlrc, has no TEMPLATE_SPACE attribute, then the
   space will be assumed to be the default space of TLRC. If the
   environment variable, AFNI_DEFAULT_STD_SPACE, is set, then its
   value will be used as the name of the space for those datasets that
   are missing a space attribute.


Re. **Transformations**
---------------

#. *Transformation definition*

   The niml file contains definitions for the transformations among
   the template spaces. Transformations are defined between a source
   space and a destination space. The following example is defined in
   AFNI_atlas_spaces.niml::

     <XFORM
      ni_type="12*float"
      ni_dimen="1"
      xform_name="MNI::MNI_ANAT"
      source="MNI"
      dest="MNI_ANAT"
      distance = "1.0"
      comment="Eickhoff-Zilles"
      xform_type="Affine" >
      1 0 0 0
      0 1 0 4
      0 0 1 5
      ></XFORM>

   The transformation definition above has several elements that need
   explanation. First, the ``ni_type`` and ``ni_dimen`` describe the
   number of data elements. Next, the name describes the source and
   destination spaces with a double colon between the two although
   this is not required and may simply be any descriptive name. The
   ``source`` field is the name of the source space, and the "dest"
   field is the name of the destination space. The distance is a
   quantification of the "cost" to transform from one space to another
   that becomes important when considering the shortest path among a
   network of multiple spaces. By manipulating the distance costs of
   multiple transformations, certain paths can be preferred or
   restricted, and a specific path can be enforced. The comment is
   simply a short description of the transformation. The ``xform_type``
   is the type of transformation. In this example, the transformation
   is an affine type, but the xform_type may be "Identity", "Brett",
   "12-piece". Depending upon the type, the number of data elements
   that follow may vary. For the "Affine" transformation, 12 data
   elements follow. In this case, the affine transformation is a
   simple shift of 4 mm in the anterior to posterior direction and a 5
   mm shift in the inferior to superior direction.

#. *whereami command*

   The ``whereami`` command includes support for transformations among
   spaces even when there is no direct transformation but instead must
   go through intermediate spaces. Coordinates may be transformed from
   any defined space to any other as long as a connection may be
   found. A Dijkstra search method determines the shortest path
   between a source space and destination space. The ``whereami``
   command can show the chain of spaces, transformations
   (``-show_chain``) and concatenated transformations
   (``-calc_chain``) between a source and a destination space. The
   transformation from a dataset in TT_N27 space follows a
   transformation chain that first computes a 12-piece transformation
   from TT_N27 to MNI space and then an affine transformation from MNI
   space to MNI_ANAT space.


Re. **Templates**
-----------------------

#. *Template definition*

   Templates are datasets that provide a reference for
   alignment. These can be virtually any dataset. Several datasets are
   included with AFNI that are often used as templates. The niml file
   contains definitions for the template datasets::

     <TEMPLATE
      template_name="MNI152.nii"
      template_space="MNI"
     ></TEMPLATE>

   The template definition includes only the name of the dataset and
   the space of that dataset. A list of templates from the NIML file
   may be displayed using ``whereami --show_templates``, but an entry in
   the NIML datasets is not required for the @auto_tlrc script, the
   main tool for alignment to a template.

Re. **Atlases**
---------------------

#. *Atlas definition*

   Like templates, atlases are also datasets, but these provide
   segmentation describing the structures within some space for a
   template. They are usually poor choices for a template for
   alignment. Several atlases are now provided with AFNI:

   * TT_Daemon,

   * CA_N27_ML, CA_N27_MPM, CA_N27_PM, CA_N27_GW, CA_N27_LR,
     CA_ML_18_MNIA, CA_MPM_18_MNIA, CA_PM_18_MNIA, CA_GW_18_MNIA,
     CA_LR_18_MNIA

   * DD_Desai_PM, DKD_Desai_PM, FS_Desai_PM

   The AFNI_atlas_spaces.niml file contains definitions for each atlas
   provided with AFNI. The following atlas description is included for
   the definition of an atlas::

     <ATLAS
      atlas_name="DD_Desai_PM"
      dset_name="TT_desai_ddpmaps+tlrc"
      template_space="TT_N27"
      description="Probability maps of 75 cortical areas"
      comment="Described in Destrieux et al., Neuroimage 2010 (53) pp. 1-15."
     ></ATLAS>

   Here the entry is demarcated by the "<ATLAS" to "></ATLAS>" lines.
   The first field within the entry, atlas_name, gives the short name
   of the atlas that can be used to specify a specific atlas to
   ``whereami`` or to other AFNI programs for specific atlas regions. The
   dset_name contains the name of the atlas dataset. This name may
   contain the full or partial path. If no path is included, as in the
   above example, the current directory, the AFNI_PLUGINPATH and then
   all the directories in the user's path are searched until the
   dataset is found. Typically, the dataset will be kept in the AFNI
   binary directory, but it may be placed anywhere. The
   "template_space" tells AFNI for which space the atlas is
   defined. In this case, the atlas has been created for the TT_N27
   space, i.e. aligned to the Talairach-transformed Colin N27
   template.  The "description" shows a brief description of the atlas
   with the "comment" providing extra information, like citation
   references.

   The atlas dataset itself has additional information in the AFNI
   header that describes the atlas. The header contains the mapping of
   structures to intensities or to sub-brick labels. There will also
   be flags to mark the dataset as a probabilistic map or if the
   dataset requires integral or continuous color maps when it is
   displayed in the afni GUI. A probabilistic atlas dataset will have
   multiple sub-bricks with each sub-brick representing a probability
   of a particular structure. These datasets are typically quite
   large, so loading all of them takes up a large amount of memory,
   but the datasets compress well, so disk space is less of an issue.

Re. **Environment Variables**
--------------------------------------

#. Several variables control how AFNI uses the ``whereami``
   features. The most important ones are:

   **AFNI_ATLAS_LIST**
     This list contains the names of the atlases that should be
     queried when no specific atlas has been requested. For example,
     the afni GUI and ``whereami``, by default, do not load all the
     atlases specified in the AFNI_atlas_spaces.niml file. If this
     variable is not set, the TT_Daemon atlas and the
     cytoarchitectonic Eickhoff-Zilles in MNI_ANAT space are
     loaded. If the variable is set to a list like
     "TT_Daemon,DD_Desai_PM", then only these two atlases are
     loaded. The list of atlas names may be separated by commas or
     semicolons. A special case of "ALL" may be set, and all the
     available atlases will be loaded.

   **AFNI_TEMPLATE_SPACE_LIST**
     This list contains the names of the template spaces that are shown
     when ``whereami`` reports the coordinates among various spaces. By
     default, the list contain "TLRC,MNI,MNI_ANAT". As for the
     AFNI_ATLAS_LIST, this list may also be set to "ALL".

   **AFNI_ATLAS_COLORS**
     This variable controls which atlas will be used by default in the
     AFNI GUI for Atlas colors, Go to atlas location and Draw Dataset
     plugin menus. By default, the TT_Daemon is used.

#. Other environment variables are less important, but are included here
   for reference.

   **AFNI_PLUGINPATH**

     This variable sets the directory to load atlases and NIML files if
     not in the current directory. If this variable does not exist or the
     referred file does not exist, then the atlas is searched in the
     user's current PATH setting. If this is set, atlases will be found
     more quickly than searching all the directories of the entire PATH.

   **AFNI_WHEREAMI_DEC_PLACES**
     Sets precision for ``whereami`` output.  Higher field data and animal
     atlases require higher precision. The default value used for focus
     point among template spaces is still 0 decimal places (closest mm),
     but animal data requires three decimal places. Value may range from
     0 to 10.

   **AFNI_WAMI_DEBUG**
     This variable controls the output of detailed messages about various
     tasks involved in loading atlases, transformations and composing
     query results. By default, this information is not shown.

   **AFNI_TTATLAS_DATASET**
     This variable may also specify the default location of AFNI
     atlases. This variable is maintained mostly for backward
     compatibility. By default, this is not set.

   **AFNI_WHEREAMI_NO_WARN**
     Turns off warnings about various ``whereami`` features -- like
     queries that reached their limit of returned results. By default,
     warnings are displayed the first time a particular message is
     encountered.

   **AFNI_WHEREAMI_MAX_FIND**
     By default, only the first nine structures are displayed within a
     particular atlas. You may increase or decrease this to show more or
     fewer structures in the ``whereami`` results.

   **AFNI_WHEREAMI_MAX_SEARCH_RAD** 
     By default, ``whereami`` searches a radius of 7.5 mm. Set a
     radius up to 9.5 mm.

   **AFNI_DEFAULT_STD_SPACE**
     The default template space is assumed to be TLRC. This is used
     for coordinate input to ``whereami``, the ``whereami`` GUI and
     for TLRC view datasets without a template space explicitly set in
     the dataset header.

   **AFNI_SUPP_ATLAS, AFNI_LOCAL_ATLAS**
     These variables allow the addition of more atlas definitions to the
     global list of atlases, templates, spaces and transformations. The
     variable should be set to the name of a NIML file with the same
     format of the AFNI_atlas_spaces.niml file. These can be customized
     by site (supplemental) or by subject (local) and follow the same
     search order as the AFNI_atlas_spaces.niml file. In order to be
     included in default searches, additional atlases or template spaces
     would also need to be added to AFNI_ATLAS_SPACE_LIST and the
     AFNI_TEMPLATE_SPACE_LIST unless those are set to "ALL".

   **AFNI_GLOBAL_SESSION**
     This variable contains the name of a directory to include in every
     session of the AFNI GUI for display. Typically this would be only
     the atlases or templates. Caution warp-on-demand in the AFNI GUI may
     warp datasets that do not have their own warp transformation using a
     transformation from one of the templates datasets in this session
     directory.


Distributed Atlases
-------------------

AFNI is distributed with the Talairach Daemon, the 1.8 version of the
cytoarchitectonic atlases of the Eickhoff-Zilles group, the
probabilistic atlases provide by Rutvik Desai based on a typical AFNI
processing pipeline using @auto_tlrc and FreeSurfer.
