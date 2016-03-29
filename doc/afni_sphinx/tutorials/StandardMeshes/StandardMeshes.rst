.. _standard_meshes:

============================================
**Standard Meshes For Group-Level Analysis**
============================================

In volume-based as well as surface-based cross-subject analysis typically requires two processing steps prior to performing group statistics:
   #. Reduce anatomical variability with smoothing and volume-based alignment to standard (MNI or Talairach) space.
   #. Resample the data onto a common grid (Standard volume size and a common voxel resolution in volumetric space)

This allows for a voxel to voxel correspondence across volumes from different subjects

This section describes the process that allows us to establish node to node correspondence across surfaces from different subjects. Much of this has been lifted from [SR_2012]_.

Here is the process by which we take a set of surfaces and re-create them with a new standard mesh that is shared by all subjects in a group study. A standard-mesh version of a surface is virtually identical in 3D shape to the original one; however, each node of the new mesh encodes the same cortical location across subjects, within the accuracy of the warping-to-template step. We detail this procedure, which is particular to SUMA, because this simple process greatly facilitates the handling of group statistics when multiple subject surfaces are not created to be topologically isomorphic, as is the case with FreeSurfer and Caret (for example).

The :ref:`figure <std_mesh_flow>` below shows the common approach for performing group analysis in the surface domain. The first step (A) involves inflating a subject's surface to a sphere, then deforming the spherical mesh (B) so that sulcal depth patterns match those of a template (Fischl et al., 1999a). In volume-based analysis, the analogous step is transforming the brain to a standard-template space. Other approaches match selected sulcal patterns or patterns derived from functional data. To what extent warping improves the final results remains the subject of debate. However, it has been repeatedly shown that group analysis on the cortical surface, even without nonlinear registration, results in increased statistical power. Regardless of the warping approach, the procedure for collecting group data then involves mapping each subject's data from a 3D voxel grid to his individual surface model, and then interpolating into a common domain that is defined by a new standardmesh. This is necessary because data values are attached to the nodes forming the surface (topology) and not to a spatial location (geometry). When the discrete surface topology differs, it is necessary to performthe analysis on a common mesh. This double interpolation step onto the standard model must be repeated for each new dataset, as is done in the volume space; however, it is largely unnecessary on surfaces because the data domain is explicitly defined and not confined to a regular grid. 


.. _std_mesh_flow:

.. figure:: media/StdMeshes_S+R_F1.jpg
   :align: center
   :figwidth: 70%
   :name: media/StdMeshes_S+R_F1.jpg
   
   The process of transforming original- to standard-mesh surfaces. See above for details. a1, a2, and a3 are the barycentric coordinates of node n in the triangle formed by nodes p1,p2,p3. Node colors of original-mesh surfaces (top two rows) show FreeSurfer's cortical parcellations. Colors on standard-mesh surfaces (bottomrow) reflect each node's index. :ref:`Figure<media/StdMeshes_S+R_F1.jpg>` and text taken from [Saad_2011]_.


With SUMA, instead of mapping a subject's data value onto the template mesh,we recreate each subject's original surface using the mesh of the standard model (shown at a very coarse level in the illustration). In other words, instead of assigning to each node n of the standardmesh a data value interpolated from data on the subject's originalmesh (p.), we now assign to that node a new set of coordinates Xn based on the coordinates of the subject's original mesh Xp. (C). When the process is repeated for all the nodes of the standard mesh, its geometry becomes essentially identical to that of the original surface: 99.9% of nodes of the standardmesh surface are within 0.01mm of the original surface1 (Saad et al.,
2004). The spherical template coordinate system is now embedded in the mesh of the newly created standard surfaces. In the bottom-most row of the :ref:`figure <std_mesh_flow>`, each node of the standard-mesh surface of each subject is colorized based on its index. Nodes with similar numbers (colors) now correspond to the same location in template space regardless of their coordinates in subject space. Within the SUMA pipeline, we begin by creating standard-mesh surfaces of each of the subject's original surfaces, and we utilize the standard-mesh versions for all subsequent analyses. On such standard meshes, functional data mapped directly from the volume to node n on one subject's surface can be directly compared to data mapped to node n on another subject's surface. Performing group-level computations on data defined on these isomorphic meshes is
then readily carried out with any of the univariate analysis tools applicable in the volume.


 
Demo:
-----

.. note:: 
   
   Standard meshes are now automatically generated by :ref:`@SUMA_Make_Spec_FS<@SUMA_Make_Spec_FS>` so you do not need to make some yourself unless you want a different mesh density or for fun.
   
