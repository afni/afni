
.. _Making ROIs:

*************************
Making (networks of) ROIs
*************************

.. contents::
   :depth: 3

Overview
========

There are many contexts for making one or more regions of interest
(ROIs) in MRI:

* forming functional (GM) networks,
* preparing targets (bordering on WM) for tractography,
* preparing theoretical studies,
* parcellating the whole brain or cortex for connectomics,

and more.  In each case, the details of the ROI-making procedure
depend on what inputs are available and on what the resulting analysis
will be.  However, the primary purpose of the process is typically the
same: to delineate brain regions which are as physiologically
meaningful and relevant as possible.

The program ``3dROIMaker`` is designed to be useful in extracting
regions from subject data itself, with the ability to combine
information from several modalities. Reference templates may also be
included for extra guidance.  Given the prominence of *networks* in
modern brain research, the focus of this program is to allow for
(hopefully) easy regionalization of data maps, such as listed
about. ``3dROIMaker`` can also be applied to multibrick data sets
simultaneously, as several other AFNI programs (e.g., ``3dNetCorr``
and ``3dTrackID``) can analyze networks simultaneously, with each one
defined as a separate brick.

|

Operation: 3dROIMaker
=====================

For historical reasons, the terminology of several ``3dROIMaker``
option names and descriptions comes from a scenario where the input
image is FMRI-related and one is looking to extract GM ROIs, while
perhaps using CSF and WM to eliminate voxels and/or inform ROI
inflation.  However, the functionality (masking, thresholding, etc.)
can be applied generally to any type of input and supporting
data. Hopefully this naming convention does not create confusion.

Broadly, ``3dROIMaker`` can be used to:

* threshold input images, in order to define a set of individual
  regions, which are sometimes colloquially referred to as *blobs*
  (``-thresh *``):

  * thresholding can be further refined to keep a set of
    maximal-valued voxels per ROI (``-only_some_top *``);

* subtract away other maps (e.g., tissue-defined skeletons) from the
  input images:

  * subtraction can be done before or after thresholding the input
    image (``-csf_skel *``, ``-wm_skel *``, ``-trim_off_wm``);
  * an other map can itself be thresholded from the commandline
    (``-skel_thr *``);

* eliminate blobs based on a volume threshold (``-volthr *``);
* uniformly inflate blobs (``-inflate *``):

  * for a given network (brick) of regions, the epansion is done layer
    by layer, aiming to limit biases among growing ROIs;
  * inflation can be controlled to stop at boundaries of guiding maps
    (``-skel_stop``).

* numbering of thresholded ROIs can be guided by a reference set, to
  keep similar regions in different data sets having the same integer
  or text labels (``-refset *``);

* perform 'preinflation', which was designed for a special case
  of*starting* with an input WM map, then expanding to GM and
  subtracting away the WM, and finally using the generated GM ROIs for
  further expansion, thresholding, etc. (``-preinfl_inset *``,
  ``-preinfl_inflate *``)

Each ROI is defined as a set of voxels with a given nonzero integer
value within dataset volume (typically, the integers are >0, though
for tractography purposes, negative voxel values show where
*anti-masks* that exclude tracts are located).  Additionally, this
information can be combined with (NIML-formatted) labeltables to allow
for text-labels to be associated with each ROI. These labeltable names
can track through to further analysis programs, such as ``3dNetCorr``
and ``3dTrackID``.


.. note:: In general, it should be possible to do much of the ROI
          processing (inflating, threshold, etc.) with all the network
          ROIs in a single brick, rather than individually.  This
          should hopefully lead to both greater time-saving, as well
          as consistency within the output. If the below notes,
          examples or existing `Message Board
          <http://afni.nimh.nih.gov/afni/community/board/>`_ posts
          don't help to keep analysis scripts short, then please
          enquire before spending ages on complicated pipelines.

**Important notes**
-------------------

#.  If mapping ``3dROIMaker``\-produced output files to another space
    (for example, from standard to DTI), then one can make sure that
    the integer-nature of the files is preserved when applying the
    transform.  One can use the ``-interp NN`` option when applying
    linear transforms (``3dAllineate``), or likely ``-ainterp NN``
    after nonlinear transforms (``3dNwarpApply`` following
    ``3dQwarp``).

#.  If the input dataset *already* has integer labels that the user
    wishes to preserve, then one should enter the same dataset as a
    reference set::

      3dROIMaker -inset FILE_A -refset FILE_A ...

    This action will preserve labels even if some of the ROIs are
    contiguous, such as is likely to occur in the case of a
    :ref:`ROI_Example_Connectome`.

|

Multiple bricks and multiple networks
-------------------------------------

Each input brick in an ``-inset *`` file is treated as a separate
network in terms of expansion, skeleton subtraction, and default
integer labeling.

If an input file has *N* bricks, then an input ``-refset *`` file (for
applying user-defined ROI integer labels) can have either 1 or *N*
bricks.  In the former case the same integer labels are applied to
each brick, and in the latter the *i*th brick in the reference set is
applied to the *i*th brick in the input set.

Currently, when an input dat set has a single NIML-formatted
labeltable (*.niml.lt) attached to it, the labels are applied to each
brick.  That is, there are not subsets of labels applicable to
different bricks.

.. _ROI_info_Neighbors:

Getting to know your neighbors
------------------------------

An important consideration in determining ROIs is how a *neighborhood*
is defined at a voxel level. For most applications, these are
symmetric around a given voxel (though, near the edge of a dataset or
mask it may be clipped).  The categories are typically described in
terms of what basic features must be shared in order to make two
voxels neighbors: nodes, edges or faces.

Different software packages have different default definitions of a
voxel neighborhood.  The three main categories are:

* face only (6 neighboring voxels);
* face+edge (18 neighboring voxels);
* face+edge+node (26 neighboring voxels).

Depictions of ways of defining voxel neighborhoods are shown below;
listed for each are examples of basic software distributions using the
given method as a typical default:

.. figure:: media/ROIS/ROI_neigh_img.png
   :width: 80%
   :align: center

   Basic voxel terminology, and its use in defining three standard,
   symmetric (nearest-)neighborhoods for an individual voxel. The
   central voxel is darkened, with each type of neighborhood depicted
   in a 3D, high-tec, separated image.

For example, the default in each of AFNI's ``3dClustSim`` and the
Clusterize function is a face-wise neighbor definition. The same is
currently true for ``3dROIMaker``, and one can use other methods by
implementing switches:

* for face+edge (18 neighbors), use ``-neigh_face_edge``;
* for face+edge+node (26 neighbors), use ``-neigh_upto_vert``.

.. note:: Even though an overall software distribution has a general
          method for defining voxel neighborhoods, individual programs
          themselves may differ or vary over time. For example
          ``3dROIMaker`` started life using a face+edge neighborhood
          default. Therefore, it is advisable to always check a given
          program for notes regarding neighborhoods.

As a slightly related appendix to this discussion, we note that some
programs define ROI neighborhoods in terms of a 'cluster radius'
(generally in units of 'mm').  In such a system, when measuring from
the center of the focal voxel, all voxels whose centers are within the
specified radial distance are included in the neighborhood. For
instance, AFNI's AlphaSim does this with the ``-rmm *`` option.  In
the case of isotropic voxels (all edges of the same length, *L*), this
system meshes with the above by setting the radius to be:

* 1.1\ *L*, for face only;
* 1.7\ *L*, for face+edge;
* 1.9\ *L*, for face+edge+node.

These values are not exclusive, but they should work fine.

|

Functional: GM ROIs
===================



Combining functional and DTI
============================

..
  ``3dROIMaker`` can be 


  If ``3dROIMaker`` is applied either in the space where the
  functional data is defined (e.g., FMRI native or standard template)


.. _ROI_Example_Connectome:

Connectome Parcellation
=======================

This is a case where the dataset being input to ``3dROIMaker`` likely
has the following properties:

#. it is already parcellated into integer-labelled ROIs;
#. its ROIs are contiguous;
#. a labeltable attached is attached.

The FATCAT_DEMO contains an example of such a set (output from
FreeSurfer) in the script ``Do_11_RUNdti_Connectome_Examp.tcsh``.

Such a dataset is shown here:

.. list-table:: 
   :header-rows: 1
   :widths: 70 30
   :stub-columns: 1

   * - FreeSurfer parcellation
     - Description
   * - .. image:: media/ROIS/aparc_sag85.png
          :width: 100%
     - (Sagittal) WB parcellation overlaid on T1w anatomical scan
   * - .. image:: media/ROIS/aparc_axi173.png
          :width: 100%
     - (Axial) WB parcellation overlaid on T1w anatomical scan

First, ``3dcalc`` was used to select ROIs with an integer above a
maximum to select only cortical GM regions.  The following images show
the remaining ROIs as colored ROIs; all the ROIs are in a single
brick.  In the first and second rows the individual ROIs are shown
overlaid on a T1w anatomical image and a FA>0.2 mask, respectively:

.. list-table:: 
   :header-rows: 1
   :widths: 70 30
   :stub-columns: 0

   *  - GM ROIs from FreeSurfer parcellation -> Inflation
      - Description
   *  - .. image:: media/ROIS/aparcGM_axi173.png
           :width: 100%
      - Cortical ROIs overlaid on T1w anatomical image.
   *  - .. image:: media/ROIS/aparcGM_onFA_axi48.png
            :width: 100%
      - Cortical ROIs (translucent) overlaid on a DTI parameter
        (FA>0.2) mask.
   *  - .. image:: media/ROIS/aparcGMI_onFA_axi48.png
            :width: 100%
      - Inflated ROIs (translucent; ``3dROIMaker`` output file name
        ``*_GMI*``) overlaid on a DTI parameter (FA>0.2) mask.
         
In the third row the input ROIs have been inflated by 1 voxel. Note
that the output contains several individual ROIs, even though the
input data contains several contiguous, nonzero voxels.  Moreover, the
output data set has retained the numerical labeling of the input (as
denoted by the local color consistency). Both of these features are a
result of utilizing the same ``-inset *`` file as a ``-refset *`` as
well::

  3dROIMaker                            \
       -inset  ROI_MAP                  \
       -refset ROI_MAP                  \
       -wm_skel FA_MAP                  \
       -skel_thr 0.2                    \
       -skel_stop                       \
       -inflate 1                       \
       -prefix o.ROIS

Volume thresholding was not necessary in this case.  Here, the WM
mask, defined as where the FA_MAP contained values were >0.2, was used
only for controlling expansion of the ROIs, and not subtracted away.


