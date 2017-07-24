.. _FreeSurfering:


Preprocessing: FreeSurfer (and @SUMA_Make_Spec_FS)
==================================================

.. contents::
   :depth: 3

Intro
-----

This stage describes preprocessing the (T1w) anatomical volume
primarily using `FreeSurfer (FS) <https://surfer.nmr.mgh.harvard.edu/>`_.
This provides information such as:

* whole brain segmentation+parcellation (2 versions: '2000' and
  '2009')
* tissue segmentation
* skull-stripping
* surface mesh estimation (with parc+seg labels attached)

\.\.\. and probably some other useful data that I am forgetting.  For
DTI-related applications, we mainly make use of the parc+seg maps and
the surface meshes, created using their ``recon-all`` function with
their defaults.

.. note:: **Disclaimer:** while we like using FS and some functions
          therein, we are *not* experts in it. All FS-related
          questions about options or problems should be addressed to
          the FS gurus themselves.  Any feedback on things to do
          differently would be welcomed and gladly discussed on our
          end.  

          The FS folks have provided some useful feedback on questions
          that have come up related to this work, so thanks to them
          for that.  

          Finally, we note that there are some differences in tissue
          maps based on their parc+seg labelling, which we describe
          below.

