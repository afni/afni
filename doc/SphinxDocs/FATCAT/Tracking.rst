
.. _Tracking:

*************
Making Tracts
*************

.. contents::
   :depth: 3

Overview
========

There are a few modes/varieties/flavors of fiber tracking which can be
performed with ``3dTrackID``.  Selecting which one to use depends a
bit on the available inputs (though, these are mostly the same for
each mode) and on the desired outcome ("with great power comes great
responsibility").

A general theme of all of them is to be able to deal with the
following scenario: *I have a network of target ROIs; where is the
most likely location of the white matter connecting any of them, and
what quantitative properties do those white matter ROIs have?*

Excited? Me, too.  Before continuing, though, let's agree on some
terminology with which these help pages will hopefully show
consistency:

#. The set of things among which we want to find connections are
   referred to as **target ROIs** or just **targets**, and
   collectively as the **network of targets**. There are usually *N>1*
   things, but sometimes using a wholebrain mask as a 'network of one'
   is useful for viewing tracts. Each target is a set of voxels of a
   particular nonzero integer, and we may refer to it either by the
   name of the integer or by a label associated with that integer
   (e.g., "1" is the "precuneus", etc.). The use of ``3dROIMaker`` in
   forming target networks is described in :ref:`Making_ROIs`.

#. The set of tracts connecting a given pair of targets is called a
   **bundle**. These are specifically the collection of linear
   structures themselves. Heck, sometimes we'll abandon all formality
   and just call them 'tracts', as well.

#. The volume containing a particular bundle is called a **WM
   ROI**. This refers to the *set of voxels* containing the particular
   tracts of interest between targets. We try to keep the terminology
   distinct from the set of target ROIs to avoid confusion.

#. Bundles and WM ROIs can be made use **AND**\ -logic and/or
   **OR**\ -logic (enough conjunctions in that statement for you?):

   * **AND**\ -logic means connecting two distinct targets, such as
     (target) ROI 4 and ROI 5; in output files, these types of
     connections are grouped under the name **PAIR** or **PAIRMAP**
     (for "pairwise" target bundles). NB: a PAIR tract could pass
     through a third target as well, but it doesn't matter at all for
     labeling or its definition as an AND-logic connection.
   * **OR**\ -logic means passing through at least one specific
     target, such as target ROI 2, with the possibility (but not the
     necessity) of passing through any other target(s); in output
     files, these types of connections are grouped under the name
     **INDI** or **INDIMAP** (for 'individual' target bundles). NB:
     for purely notational and labelling purposes, it's also
     convenient to think of an INDI bundle being the special case of
     connecting a target with itself-- this is useful for the
     matrix-style storing of information described in the next list
     item.
   * the PAIR tracts/bundles and WM ROIs are a subset of INDI ones.

#. Tracking to determine the *structural* connectivity among
   pairs/individual targets is analogous to using correlation to
   determine *functional* connectivity among a (target)
   network. Functional connectivity information is generally stored
   and viewed as correlation matrices, and we borrow the same to store
   ``3dTrackID`` output in **structural connectivity matrices**:

   * a given matrix can store properties such as the number of tracts
     between two targets, the average FA of a WM ROI, etc.
   * just as in a correlation matrix, each row and column is labeled
     by a particular target. Each **element** of the matrix contains
     the property of the WM ROI that connects those targets.
   * a diagonal matrix element, where the row number is the same as
     the wcolumn number, describes a property of an OR-logic (or INDI)
     WM ROI.
   * an off-diagonal element describes a property of an AND-logic (or
     PAIR) WM ROI.
   * the structural connectivity matrix is symmetric: the property
     between targets 1 and 7 is the same as that between targets 7 and
     1 (and similarly for any off-diagonal element). This is another
     way of stating that there is no directionality between the
     structural connectivity properties.
  

.. note:: "All targets are equal," meaning that tracking is performed
          amongst and within the network of targets, with no special
          preference.  Other programs may have an approach of tracking
          'to' a target 'from' a seed, but this directional approach
          (where "some targets are more equal than others") is not the
          philosphical choice here. Why should a tract result
          connecting targets 1 and 9 be different than that connecting
          9 and 1?

|

Modus Operandi: 3dTrackID
=========================

**Preface.** Once upon a time, there were two separate FATCAT programs
for performing deterministic and probabilistic tractography (3dTrackID
and 3dProbTrackID, respectively), as described in the initial paper
(Taylor and Saad, 2013). Soon after this introduction, it became
apparent that there could be only one.  After a brief quickening,
3dTrackID picked up the probabilistic attributes and now that program
is run in separate modes. Which brings us to the present.

**Current times.** There are three distinct **modes** for performing
 tractography, and each is denoted with a short string after the
 required switch ``-mode *``.  These are:

 #. deterministic (``DET``),
   
 #. mini-probabilistic (``MINIP``), 

 #. (full) probabilistic (``PROB``).

Thus, all FATCAT tracking commands will start with a selection of one
of these modes::
  
  3dTrackID -mode {DET|MINIP|PROB} ...

There is a lot of overlap in what kind of data are input and output
for these modes. First, we describe the default and optional outputs
of all; then, special outputs of some; finally, the differences in
inputs (and why they exist as such).

Outputs common to all modes
===========================

By default, each of the ``3dTrackID`` modes will output the following:

* volumes of WM ROIs, both a single **PAIRMAP** file of the AND-logic
  connections and a single **INDIMAP** file of the OR-logic ones.
  These can be viewed most easily using the AFNI viewer to get a
  visualization of:

  * all the locations where tracts went through the network ([0]th brick
    of either MAP file);

  * all the locations where tracts went through an individual target
    ([i]th brick of either MAP file, where *i>0*);

* a **GRID** file (ending with ``*.grid``), which contains all the
  structural connectivity matrices for the given network. Matrices in
  these files can be:

  * selected, viewed and saved to an image file using ``fat_mat_sel.py``;

  * used for group-based statistics with G. Chen's 3dMVM program, with
    some helper ``fat_mvm*.py`` functions available for putting
    everything together and building commands+models.

* a **DSET** file (ending with ``*.dset``), which also contains all of
  the structural connectivity matrices for a given network.  Matrices
  in these files can be:

  * loaded into SUMA (``$ suma -gdset NAME.niml.dset ...``);

  * viewed in SUMA as either a standard, colorful matrix, or as a
    graph-like network of nodes and edges throughout the 3D brain
    representation;

Additionally, each mode *can* also output:

* a set of maps/masks of each individual WM ROI. This is done using
  the option ``-dump_rois {AFNI|DUMP|BOTH|AFNI_MAP}``. The keyword
  options each produces a set of individual files of the following:

  * ``DUMP`` -> ``3dmaskdump``\-like text files of each WM ROI (which
    could take quite a lot of space and not be so useful;
    
  * ``AFNI`` -> binary masks of each WM ROI;
    
  * ``BOTH`` -> both the binary masks and text files (combined outputs
    of ``DUMP`` and ``AFNI``; the name reflects that it was developed
    when there were only two individual output formats);
    
  * ``AFNI_MAP`` --> non-binarized *maps* of each WM ROI, where the
    value of each voxel is the number of tracts that went through it
    for that given connection;

.. note:: Probably using one of the options ``-dump_rois
          {AFNI|AFNI_MAP}`` would be the most useful.  Some unnamed
          user(s) would even go so far as to recommend using it all
          the time, because either would provide the only unambiguous
          maps of individual WM ROIs output by ``3dTrackID``.

.. note:: A PAIRMAP is not output if the input network has only one
          target ROI, such as if one is doing a simple whole brain
          tracking.

.. note:: One can turn of INDIMAP and PAIRMAP output altogether, using
          the switch ``-no_indipair_out``.  This might be useful if
          you are tracking through a *large* network of targets, and
          don't want to risk having a single reaaally big output file
          wasting space or causing trouble.

.. note:: By default, all volumetric outputs (PAIRMAP, INDIMAP,
          ``-dump_rois *`` files, etc.) are in BRIK/HEAD file format.
          If you prefer NIFTI, you can use the switch ``-nifti`` to
          get all "\*.nii.gz" files.
