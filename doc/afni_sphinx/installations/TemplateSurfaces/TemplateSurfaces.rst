.. _TemplateSurfaces:

==================
Template Surfaces:
==================

.. _MNI_N27:

MNI N27:
--------

You can easliy load template surfaces into SUMA using a syntax such as::

   suma -spec MNI_N27
   (or)
   suma -i lh:MNI_N27:ld60:smoothwm
   
Search the output of *suma -help* for *TEMPLATE* to get more information about the syntax. The convenience includes not having to specify the path to the spec file in question, or specifying the surface volume.

To install the template surfaces you can do the following::

   cd ~/
   \mkdir -p .afni/data
   cd .afni/data
   afni_open -aw suma_MNI_N27.tgz
   tar xvzf suma_MNI_N27.tgz
   
Repeat with suma_TT_N27.tgz to get the Talairach version. Test your installation by loading a low resolution standard mesh (for instance) with::

   cd ~/
   suma -spec MNI_N27:ld60   
   

