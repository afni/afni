
.. _Tracking:

************************
Making Tracts: 3dTrackID
************************

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
   collectively as the **network of targets**.  There are usually
   *N>1* targets in a network, but sometimes it is useful to use a
   wholebrain mask as a 'network of one' for viewing tracts. Each
   target is a set of voxels of a particular nonzero integer, and we
   may refer to it either by the name of the integer or by a label
   associated with that integer (e.g., "1" is the "precuneus",
   etc.). The use of ``3dROIMaker`` in forming target networks is
   described in :ref:`Making_ROIs`.

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

Modus operandi
==============

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

The outputs can be viewed variously and interactively in AFNI and SUMA
(such as for volume, tract, and dset files).  Additionally, matrices
of properties can be viewed and saved from the commandline with some
``fat_*.py`` functions. Finally, outputs can be used for quantitative
comparison and statistical modeling-- one method for doing the latter
exists using G. Chen's 3dMVM (see below for some description, and the
FATMVM demo introduced :ref:`DEMO_Definitions`).


Outputs common to all modes
===========================

#. By default, each of the ``3dTrackID`` modes will output the following:

   * volumes of WM ROIs, both a single **PAIRMAP** file of the AND-logic
     connections and a single **INDIMAP** file of the OR-logic ones.
     These can be viewed most easily using the AFNI viewer to get a
     visualization of:

     * all the locations where tracts went through the network ([0]th brick
       of either MAP file);

     * all the locations where tracts went through an individual target
       ([i]th brick of either MAP file, where *i>0*);

   * a **grid** file (ending with ``*.grid``), which contains all the
     structural connectivity matrices for the given network. Matrices
     in these files can be:

     * selected, viewed and saved to an image file using
       ``fat_mat_sel.py``;

     * used for group-based statistics with G. Chen's 3dMVM program,
       with some helper ``fat_mvm*.py`` functions available for
       putting everything together and building commands+models.

   * a **dset** file (ending with ``*.dset``), which also contains all
     of the structural connectivity matrices for a given network.
     Matrices in these files can be:

     * loaded into SUMA (``$ suma -gdset NAME.niml.dset ...``);

     * viewed in SUMA as either a standard, colorful matrix, or as a
       graph-like network of nodes and edges throughout the 3D brain
       representation;


   **Example 1.** For example, running ``3dTrackID`` with ``-prefix
   o.NETS`` (and ``-nifti``) will produce the output files::

     o.NETS_000.grid
     o.NETS_000.niml.dset
     o.NETS_000_INDIMAP.nii.gz
     o.NETS_000_PAIRMAP.nii.gz

   Comments on these outputs:

     * A PAIRMAP is not output if the input network has only one
       target ROI, such as if one is doing a simple whole brain
       tracking.

     * One can turn off INDIMAP and PAIRMAP output altogether, using
       the switch ``-no_indipair_out``.  This might be useful if you
       are tracking through a *large* network of targets (for example,
       something connectome-y) and don't want to risk having a single
       reaaally big output file wasting space or causing trouble.

     * By default, all volumetric outputs (PAIRMAP, INDIMAP,
       ``-dump_rois *`` files, etc.) are in BRIK/HEAD file format.  If
       you prefer NIFTI, you can use the switch ``-nifti`` to get all
       "\*.nii.gz" files.

   |

#. Additionally, each mode *can* also output:

   * a set of maps/masks of each individual WM ROI. This is done using
     the option ``-dump_rois {AFNI|DUMP|BOTH|AFNI_MAP}``. The keyword
     options each produces a set of individual files of the following:

     * ``DUMP`` -> ``3dmaskdump``\-like text files of each WM ROI
       (which could take quite a lot of space and not be so useful;
    
     * ``AFNI`` -> binary masks of each WM ROI;
    
     * ``BOTH`` -> both the binary masks and text files (combined
       outputs of ``DUMP`` and ``AFNI``; the name reflects that it was
       developed when there were only two individual output formats);
    
     * ``AFNI_MAP`` --> non-binarized *maps* of each WM ROI, where the
       value of each voxel is the number of tracts that went through
       it for that given connection;

     **Example 1 (continued).** Additionally, if one also included the
     command ``-dump_rois AFNI``, then the output would include a
     directory **o.NETS/** with the following files, such as::

       NET_000_ROI_001_001.nii.gz  
       NET_000_ROI_001_004.nii.gz  
       NET_000_ROI_002_002.nii.gz  
       NET_000_ROI_002_003.nii.gz  
       NET_000_ROI_002_004.nii.gz  
       NET_000_ROI_002_006.nii.gz  
       NET_000_ROI_002_007.nii.gz  
       ...

     With the specific dump option used here, each file would contain
     a binary mask of the given WM connection.  The file naming
     convention is: NET_X_ROI_Y_Z.nii.gz, where:

       * 'X' is the number of the network (because multiple ones can
         be tracked simultaneously

       * 'Y' is the number or label of a target ROI
     
       * 'Z' is the number or label of another target ROI

     The files where 'Y'=='Z' contain INDIMAP information of a target,
     and the others where not('Y'=='Z') contain PAIRMAPs.  It's
     important to note that tracts will not be found between every
     possible pair of targets, and so not every possible pairwise
     combination will have a file output.  |

     .. note:: Probably using one of the options ``-dump_rois
               {AFNI|AFNI_MAP}`` would be the most useful.  Some
               unnamed user(s) would even go so far as to recommend
               using it all the time, because either would provide the
               only unambiguous maps of individual WM ROIs output by
               ``3dTrackID``.

   * A labeltable file (``*.niml.lt``) will also be output if one has
     been attached to the input network file. While one might not view
     this on its own, having a labeltable set up can be very useful,
     for example in helping to discuss specific bundles by the
     anatomical locations they connect.

   |

Outputs specific to ``{DET|MINIP}`` modes
=========================================

#. The outputs in the previous section are output for all modes of
   ``3dTrackID``.  However, careful readers will note that none of
   those tractographic outputs actually contained the tracts
   themselves!  These are only output in ``{DET|MINIP}`` modes, as the
   following:

   * a **tract** file (ending with ``*.tract``), which contains all
     the individual tract sequences.  Additionally, it internally has
     the tracts organized into sets of bundles between targets, so
     that each bundle could be displayed as a separate color.  These
     files are viewable in SUMA, loading with::

       suma -tract PREFIX.niml.tract ...

     One can also load in the **dset** simultaneously and view the
     connectivity matrix elements as coloration of tract bundles, such
     as after::

       suma -tract PREFIX.niml.tract  -gdset PREFIX.niml.dset ...

     (In fact, the dset loaded in could be either one output by
     ``3dTrackID`` or by ``3dNetCorr``.)

   * a TRK-format file, ``*.trk``, legacy of when tractographic output
     had to be viewed with non-AFNI/SUMA options, which in this case
     were with TrackVis.  These are not output by default. To have
     these be output, use the the ``-do_trk_out`` switch.

#. When outputting tract files, one has to choose whether to use
   AND-logic or OR-logic within the network.  That is, whether to keep
   tracts that have a minimal requirement of going through one target
   (OR), or whether to require at tract to connect at least two
   targets (AND).  The choice is made using the (required) option
   ``-logic {AND|OR}``.

#. And, just to state explicitly, the full probabilistic tracking in
   ``-mode PROB`` does *not* (currently) produce tract file output.
   Such is life and also an impetus behind the mini-probabilistic
   methodology (described further below).
   
|

Viewing tracked outputs
=======================

#. **Volume files outputs.** PAIRMAP, INDIMAP and dumped volumes can
   all be viewed in either AFNI or in SUMA.  To load them into the
   latter for 3D visualization, use::
    
     suma -vol FILENAME ...
     
   By default, they are displayed as slices and not as surfaces, but
   you can select that capability (see description in
   :ref:`Volume_Viewing`).

   To view the volume files in the 2D afni slice viewer, one uses the
   standard, general call to open AFNI (assuming you're in a directory
   where those files are located; otherwise, include the path to
   them)::
     
     afni

#. **Matrix file outputs.** SUMA is used to view the matrix
   information in the ``*.dset`` file.  While one can view this
   information as a 'classic' connectivity matrix (for both
   ``3dTrackID`` and ``3dNetCorr`` outputs), it is also possible to
   view the data as coloration of graph edges and/or tract bundles in
   the brain volume. For more features, please see the help examples
   in SUMA: :ref:`Graph_Viewing`. To load the data into SUMA, use::

     suma -gdset FILE.niml.dset ...

   Additionally, one can select, view and save the matrices from the
   commandline with a Python-based tool, ``fat_mat_sel.py``.  This
   program can output several matrices from several subjects
   simultaneously, and the user can control several features of the
   plotting (font size, colorbar properties, ranges, DPI, etc.). It
   can be useful, for example, when making outputs for presentations
   and publications.  See the helpfile::
     
     fat_mat_sel.py -h

   for more information and list of the options.

#. **Tract files.** These are viewable in SUMA with *many, many*
   interactive features.  To load in the tracts::

     suma -tract FILE.niml.tract ...
     
   Default coloration is by local tract orientation, but one can also
   color, for example, by bundle (useful for connectomes) or by the
   connectivity matrix information (importing the ``-gdset
   FILE.niml.dset`` information, above). 

   Selection masks (either sphere or box) can be made for specifying
   subsets of tracts. One can have multiple selection masks, and use
   AND- and/or OR-logic with them. **Importantly**, these volumes are
   dragged along the tracts and bundles themselves, so that one can
   follow arbitrary trajectories through 3D (i.e., one is not
   constrained to manipulating them just in 2D slices).  

   For more information, please see the voluminous set of features,
   hints and examples in the SUMA help: :ref:`Tract_Viewing`.

#. **TRK files.** These ``NAME.trk`` files are generated using the
   TrackVis format, and as such can be viewed in the eponymous
   program. (They are not output by default.)

|

Inputs for tracking
===================

This will be an attempt to cluster sections of the input options
meaningfully.


Minimal inputs for each mode
----------------------------

Each option is briefly explained the first time it is mentioned; one
can assume that, unless explicitly noted, the initial definition still
holds.

#. Deterministic (DET) DTI::

     3dTrackID -mode DET            \
         -dti_in  DT_PREF           \
         -netrois TARGET_ROI_FILE   \
         -logic   {AND|OR}          \
         -prefix  OUT_PREF
   
   where:
   
   * ``-dti_in DT_PREF``: point to the set of DTI parameter files by
     their prefix.  The program will read in all scalar files with
     this prefix and output WM ROI statistics on them.

   * ``-netrois TARGET_ROI_FILE``: input the file of targets among
     which to find connections. This can be a file with multiple
     volumes/bricks, and each brick is treated like a separate
     network. Each target in a network is defined as a set of voxels
     with a given integer, and a labletable can be attached for
     further target naming with strings (with the labels also being
     attached to tracked outputs).

   * ``-logic {AND|OR}``: select whether the tracts output in the
     *.tract file connect targets using AND- or OR-logic. NB: in
     *either case, both INDI and PAIR map (volume) files are output.

   * ``-prefix OUT_PREF``: prefix for all output files, as described
     above. Additionally, a network number will be appended before the
     file extensions, starting with 000, 001, 002, etc. (in order to
     match the brick number of the ``-netrois`` file).

     |

#. Mini-probabilistic (MINIP) DTI::

     3dTrackID -mode MINIP          \
         -dti_in  DT_PREF           \
         -netrois TARGET_ROI_FILE   \
         -logic   {AND|OR}          \
         -uncert  UNCERT_FILE       \
         -mini_num NREP             \
         -prefix  OUT_PREF

   where:

   * ``-uncert UNCERT_FILE``: the file of uncertainty values output by
     3dDWUncert.
     
   * ``-mini_num NREP``: the number of perturbed Monte Carlo
     repetitions to perform.  Often 5-7 seems to be a good number.

     |

#. Fully probabilistic (PROB) DTI::

     3dTrackID -mode PROB           \
         -dti_in  DT_PREF           \
         -netrois TARGET_ROI_FILE   \
         -uncert  UNCERT_FILE       \
         -prefix  OUT_PREF

     




