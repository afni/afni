
.. _Tracking:

*************************
Making Tracts
*************************

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

* the set of things among which we want to find connections are
  referred to as **target ROIs** or just **targets**, and collectively
  as the **network of targets**. There are usually *N>1* things, but
  sometimes using a wholebrain mask as a 'network of one' is useful
  for viewing tracts. Each target is a set of voxels of a particular
  nonzero integer, and we may refer to it either by the name of the
  integer or by a label associated with that integer (e.g., "1" is the
  "precuneus", etc.).
* the set of tracts connecting a given pair of targets is called a
  **bundle**. These are specifically the collection of linear
  structures themselves.
* the volume containing a particular bundle is called a **WM
  ROI**. This refers to the *set of voxels* containing the particular
  tracts of interest between targets. We try to keep the terminology
  distinct from the set of target ROIs to avoid confusion.
* bundles and WM ROIs can be made use **AND**\ -logic and/or
  **OR**\ -logic (enough conjunctions in that statement for
  you?):

  * **AND**\ -logic means connecting two distinct targets, such as
    (target) ROI 4 and ROI 5; in output files, these types of
    connections are grouped under the name **PAIR** or **PAIRMAP**
    (for "pairwise" target bundles). NB: a PAIR tract could pass
    through a third target as well, but it doesn't matter at all for
    labeling or its definition as an AND-logic connection.
  * **OR**\ -logic means passing through at least one specific target,
    such as target ROI 2, with the possibility (but not the necessity)
    of passing through any other target(s); in output files, these
    types of connections are grouped under the name **INDI** or
    **INDIMAP** (for 'individual' target bundles). NB: for purely
    notational and labelling purposes, it's also convenient to think
    of an INDI bundle being the special case of connecting a target
    with itself-- this is useful for the matrix-style storing of
    information described in the next list item.

* tracking to determine the *structural* connectivity among
  pairs/individual targets is analogous to using correlation to
  determine *functional* connectivity among a (target)
  network. Functional connectivity information is generally stored and
  viewed as correlation matrices, and we borrow the same to store
  ``3dTrackID`` output in **structural connectivity matrices**:

  * a given matrix can store properties such as the number of tracts
    between two targets, the average FA of a WM ROI, etc.
  * just as in a correlation matrix, each row and column is labeled by
    a particular target. Each **element** of the matrix contains the
    property of the WM ROI that connects those targets.
  * a diagonal matrix element, where the row number is the same as the
    column number, describes a property of an OR-logic (or INDI) WM
    ROI.
  * an off-diagonal element describes a property of an AND-logic (or
    PAIR) WM ROI.
  * the structural connectivity matrix is symmetric: the property
    between target 1 and 7 is the same as that between target 7 and 1
    (and similarly for any off-diagonal element).
  


