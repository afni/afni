.. _tut_auto_@snapshot_volreg:

Using @snapshot_volreg
======================

.. contents::
   :depth: 3

.. highlight:: Tcsh

``@snapshot_volreg`` is an useful program for quickly evaluating
alignment between two datasets, for example between a subject's
anatomical and an EPI volume, or between an anatomical and a standard
space template. This program creates an edge-ified overlay of one data
on the underlayed other, allowing for quick checks of the similarity
of major brain features.

The output is a :math:`3\times3` montage: one row each of axial,
coronal and sagittal slices.
