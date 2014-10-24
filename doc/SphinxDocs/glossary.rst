.. _glossary:

=========
Glossary:
=========

.. glossary::

   SRC
   Shift+Right Click
      Cliking right mouse button while holding shift key
   
   RAI
      Coordinate axis convention where X grows from Right to Left, Y from Anterior to Posterior, and Z from Inferior to Superior. This is AFNI's preferred coordinate convention.     

   1D index
      Index {n} of a :term:`datum` in a one dimensional representation of the collection of elements forming an object or a dataset. See also :term:`3D index`.
         
         * For surfaces and surface-based datasets: This would be the index of the node in the surface's nodelist. The range of values would be from 0 to the  total number of nodes in the surface's nodelist minus one.
         
         * For volumes: This would be the 1D index of the voxel in the volume. The relationship between the 1D index n and :term:`3D index` is given by:
         
            n = i + j * Ni + k * Ni * Nj
            
            where Ni, and Nj are the number of voxels along the volume's first and second dimensions, respectively.
            
         * For graphs and matrices: The 1D index would be the index of the edge/cell of the graph. For full matrices, the relationship between 1D index and the row, column (r,c) in the matrix would be:
         
            n = r + c * Nr
            
            where Nr is the number of rows in the matrix.
            
            For triangular and sparse matrices, the relationship becomes more complex and is best documented in the source code. See function SUMA_GDSET_PointsToSegIndex() for a start.
   
   1D   
   1D file
   1D Dset
      A simple table of numbers. All lines must have the same number of values, and text following the '#' character all the way to the end of the line is ignored as comments. **In genreal** 1D files can be considered as 1 dimensional volumes of N voxels with N being the number of lines in the file, and K :term:`sub-bricks` for each column in the table. Some programs have their own exceptions to these rules. Try and you shall find out.  
      
   3D index
      {i,j,k} indices of datum in 3 dimensional array representing data or object. {i,j,k} triplets are mostly used for notational clarity, it is often the case that a 1D array is used to store and access array elements. 
      
   bundle
      A collection of tracts, in a network. Usually a bundle defines all tracts between a pair of target ROIs.
   
   network
      A collection of bundles of tracts.
      
   tract
   tracts
      A set of connected points.
   
   point
   points
      Building element of tracts. 
       
   sub-brick
   sub-bricks
   subbricks
   subbrick
   Dataset column
      Dataset in AFNI & SUMA land are loosely described as a collection of N values for each datum (voxel, node, point, graph edge, etc.). To take volumes as an example, each of these N values forms a sub-brick. A single anatomical volume such as a T1 weighted image has one value per voxel or one sub-brick. A dataset output by a statistical program will almost always have multiple sub-brick. A simple t-test for instance will produce a dataset of two sub-bricks one containing the effect size (e.g. contrast) and another containing the T statistic. The same goes for surface-based datasets, graph datasets, etc. For wonders of sub-brick selection see the output of suma -help, section "Selecting subsets of a dataset".
   
   datum
      A data carrying element. For the various types of data carrying/defining objects handled in suma, the elementary datum is the following:
      
      ========   =================
      Object     Elementary Datum
      ========   =================
      Surface    Node
      Graph      Edge (ident Cell)
      Matrix     Cell (ident Edge)
      Tracts     Point
      Volume     Voxel
      ========   =================
         
   I
   Intensity
      Dataset column that is used to map values (intensities) to the colormap.
   
   T
   Threshold
      Dataset column that is used to provide the values to be compared against the thresholding value. Data points that have a T value less than the thresholding value do not get colored regardless of their intensity value.
      
   B
   Brightness
      Dataset column providing values used to modulate the brightness of the data point colors (GET from surface controller definition...)

   Family of surfaces
      A collection of surfaces sharing the same parent mesh. The most common family is the set of surfaces for a particular hemisphere and a particular subject. This includes anatomically correct surfaces such as the pial and white matter models, the deformed ones such as the inflated surfaces, and partial ones such as cut surfaces.
      
      A set of surfaces can be grouped into one family, regardless of whether or not the subject and/or hemispheres match, as long as they are isotopic. All standard-mesh surfaces of the same number of nodes can be treated as belonging to the same family. *Note however* that for FreeSurfer-derived standard-mesh surfaces, the same index on the left hemisphere does not refer to the same anatomical location as that same index would on the right hemisphere. If you want node index correspondence across hemispheres, see the comment about *FreeSurfer's* option *-contrasurfreg* in the -help output of *@SUMA_Make_Spec_FS*.

   Mask Manipulation Mode
      A mode in which selecting a location (right-click) in SUMA, causes the tract mask to jump to that location. See :ref:`Mask_Manipulation_Mode` for details.
      
