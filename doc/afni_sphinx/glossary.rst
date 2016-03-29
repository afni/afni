.. _glossary:

========
Glossary
========

.. glossary::
   :sorted: 


   .afnirc
   afnirc
      File residing in your home directory and contanining AFNI's environment variable setttings, colormaps, and more.
      
   .sumarc
   sumarc
      File residing in your home directory and containing all of SUMA's environment variables, as long as you keep running :ref:`suma -update_env<suma--update_env>` with each update of your binaries. 
   
   ROI
      Region of interest.  In tractography, we try to keep the
      separate language of 'target ROIs', referring to the set of
      blobs among which one is looking for connections, and 'WM ROIs',
      referring to the white matter regions where those connections
      are.

   WB 
      Whole brain.

   index
   indexing
      A system of referring to an element in an ordered set, such as a
      time series or a particular volume in an MRI acquisition.  Some
      software/people start counting from 1 up to *N*, while others
      (most notably, AFNI) count from 0 up to *N-1*.  Even if
      unfamiliar, the latter has several benefits (well, that's what I
      read on the internet), so it's good to become familiar with
      it. One should always be clear about indexing systems at use
      when processing.

   row-first
   diagonal-first
      Two methods of reading (or flattening) a symmetric matrix, such
      as a DT or diffusion matrix. The names refer to the order in
      which components are read out from the matrix, which can be
      arbitrary (as long as one is consistent).  Different programs
      use different formats, so always check to make sure that you are
      using compatible notation in your own data whenever utilizing a
      new program. See :ref:`DealingWithGrads` for more discussion.

   reference set
      In DWI, a volume without an externally applied DW gradient,
      often referred to as an unweighted or *b*\=0 set.  Generally,
      more than one reference volume is acquired during scanning, to
      increase SNR.

   SNR 
      Signal-to-noise ratio.  Omnipresent in discussions of data
      quality.  In MRI discusions, increasing SNR usually involves
      trade-offs of longer acquisition time, lower resolution or some
      other factor.

   DW
   DWI
      Diffusion weighted (imaging), a method of acquiring MRI data
      which uses an extra magnetic field gradient to measure
      diffusivity along a given spatial orientation.  Typically,
      several DW images are acquired and used to estimate diffusion
      tensors (DTs) or higher order HARDI models.

   DT
   DTI
      Diffusion tensor (imaging), a particular model for fitting DWI
      data in order to more easily quantify local, relative
      anisotropy.  Mathematically, the DTs are 3 by 3, symmetric
      positive definite matrices, which happily also geometrically
      correspond to the surfaces of ellipsoids. From DTs, further
      parameters (such as FA, MD, eigenvalues and eigenvectors) can be
      derived.

   HARDI
      High angular resolution diffusion imaging, a higher order model
      than DTI for DWI data.  May require higher DW factors, many more
      acquisitions, and more computational processing than DT
      modelling, but the expected benefit is to be able to estimate
      more than one major direction of diffusion in a given voxel ->
      assumed to represent more complicated crossing fibers.  There
      are *many* HARDI models in existence.

   flip 
      Can refer both to the systematic mismatch between recorded
      gradient files and saved DWI datasets (where one gradient
      component's sign is not compatible with the data), or to the
      simple computational process of undoing said mismatch by
      multiplying a given component in an entire gradient set by
      ``-1``.  The need for flipping a data set can best be seen in an
      initial investigation of a whole brain, deterministic
      tractography run (where the wellknown features of the corpus
      callosum in a healthy subject would look very wrong).

   FA
      Fractional anisotropy, a scalar parameter derived from the DT
      that quantifies the relative *pointedness* of a tensor's
      ellipsoid shape. The minimum is 0, representing an isotropic
      sphere (i.e., spatially uniform structure), and the
      (theoretical) maximum is 1, representing something with highly
      spatially aligned structure. Essentially, it is a normalized
      standard deviation of the DT's eigenvalues.

   MD 
      Mean diffusivity, a scalar parameter derived from the DT that
      quantifies the average *magnitude* of a tensor's ellipsoid
      shape. Its values are always >0. It is the mean of the DT's
      eigenvalues.

   L1
   L2
   L3
      The eigenvalues of a DT (with the standard convention
      L1>L2>L3>0).  Geometrically, these scalars are the semiaxes of
      the DT.  They would be all equal for a sphere. They are
      sometimes written with the Greek letter, lambda:
      :math:`\lambda_1, \lambda_2, \lambda_3`. L1 is sometimes known
      as *parallel* or *axial* diffusivity.

   **e1**
   **e2**
   **e3**
      The eigenvectors of a DT (usually written with subscripts,
      :math:`\mathbf{e}_1, \mathbf{e}_2, \mathbf{e}_3`) with
      :math:`\mathbf{e}_i` associated with the *i*\ th eigenvalue,
      :math:`\lambda_i`.  These are mutually orthogonal (i.e.,
      perpendicular) and typically of unit magnitude. Geometrically
      they provide the orientation of the DT.

   RD
      Radial diffusivity (AKA perpendicular diffusivity).  It is the
      average of L2 and L3.

   tractography
      A computational process for estimating the likely location of WM
      associated with target regions.  There are *many* tractography
      algorithms in existence. There are also several styles of
      tracking, such as deterministic, probabilistic and a blended
      form called mini-probabilistic.  Deterministic can be
      particularly useful for initial investigations, and the latter
      two utilize the estimated uncertainty of DT parameters to
      provide more robust results.

   tractography coloration 

      In deterministic (and mini-probabilistic) tracking, default
      tract coloration is RGB (red-green-blue) for segment orientation
      as follows: **red** for left-right; **green** for
      anterior-posterior; **blue** for inferior-superior.  If non-RGB
      coloration is used, then probably the distinct colors refer to
      connections between different pairs of ROIs.

   WM
      White matter.

   GM
      Gray matter.

   CSF
      Cerebrospinal fluid.

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

      A collection of tracts, within a network. Usually a bundle defines all tracts between a pair of target ROIs.
   
   network
      A collection of bundles of tracts.
      
   tract
   tracts
      A sequence (or ordered set) of connected points.
   
   point
   points
      Building element of tracts. 
   
   node
   nodes
      For a *surface object*, a node is one of the elements in the point cloud over which surface data values are defined. A node has an :term:`RAI` coordinate and a set of first order neighboring nodes with which it is connected.
      
      For a *graph object*, a node is one of the connected graph regions, however unlike nodes on the surface, a graph node does not carry data. On graphs (connectivity matrices), data are defined over the edges, including the edge connecting a node to itself. You can also think of a node as being a row or column of the connectivity matrix.
   
   cell   
   edge
   edges
      For a *surface object*, an edge exists wherever two nodes are first order neighbors of one another. In the majority of surfaces used, nodes are connected as to form a triangular mesh. Edges of a surface object do not have data defined over them.
      
      For a *graph object*, an edge connects two regions (nodes) of the graph. Unlike for surface objects, edges here do carry the data. An edge on a graph is the same as a cell in the connectivity matrix. 
       
   sub-brick
   sub-bricks
   subbricks
   subbrick
   Datasets
   Dataset column
      Dataset in AFNI & SUMA land are loosely described as a collection of N values for each datum (voxel, node, point, graph edge, etc.). To take volumes as an example, each of these N values forms a sub-brick. A single anatomical volume such as a T1 weighted image has one value per voxel or one sub-brick. A dataset output by a statistical program will almost always have multiple sub-brick. A simple t-test for instance will produce a dataset of two sub-bricks one containing the effect size (e.g. contrast) and another containing the T statistic. The same goes for surface-based datasets, graph datasets, etc. For wonders of sub-brick selection see the output of suma -help, section "Selecting subsets of a dataset".
   
   color plane
   color planes
      A color plane, is the result of the colorization of a dataset according the the parameter settings in the object's controller. Each dataset gets its own color plane and the resultant color mapped onto the :term:`Displayable Object` depends on the stacking order of the color planes and their transparencies. It helps to think of a color plane as a stacked set of transparency sheets observed from above. See also :ref:`plane layering<Plane_Layering>`.
      
   data
   datum
      In the documentation, this refers to a value carrying element(s), or  the value itself. For the various types of data carrying/defining objects handled in suma, the elementary datum is the following:
      
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

   Draw ROI Mode
      A mode in which selecting a location (right-click) in SUMA, causes a modification of the current unfinished ROI being drawn, or creates a new ROI. When the viewer is in :ref:`Draw ROI Mode<Draw_ROI_Mode>`, the cursor changes shape to become a circular target.
   
   Record Mode
   Recording Mode
      When the :ref:`SUMA viewer<viewer>` is in record mode, any change to the rendered image is captured either directly to disk or to a recorder window. When the viewer is in recording mode, the title bar of the viewer displays the word *Rec* as part of the window name as shown in the figure below.
      
      .. figure:: SUMA/media/surfview_rec.jpg
         :align: center
         :name: SUMA/media/surfview_rec.jpg
         :target: ../_images/surfview_rec.jpg
         
         :ref:`Viewer in record mode.<SUMA/media/surfview_rec.jpg>`
         
   Spec
   Spec file
      A text file setting the specifications for a family of surfaces, including the relationships between them. The :ref:`spec file<Spec_File>` is usually created automatically by the likes of *@SUMA_Make_Spec_** such as :ref:`@SUMA_Make_Spec_FS<@SUMA_Make_Spec_FS>` or :ref:`@SUMA_Make_Spec_Caret<@SUMA_Make_Spec_Caret>`, or with :ref:`*quickspec*<quickspec>` or :ref:`inspec<inspec>`.
   
   Surface Volume
      Volume with which the surfaces are in alignment. This volume is usually created by scripts @SUMA_Make_Spec_* and is either the same as the volume from which the surfaces were created, or a spatially transformed version of it. Spatial transformations present in the header of the surface volume are applied on the fly to the surface coordinates when loaded into SUMA or any of the command-line programs that expect a surface volume. See also script :ref:`@SUMA_AlignToExperiment<@SUMA_AlignToExperiment>`
      
   State
   States
      For surfaces, state is shorthand for the deformation state. For instance,  lh.pial.gii and lh.inflated.gii surfaces are of two states, pial, and inflated, respectively. You can change the default state names by editing the :term:`spec file` manually. Surfaces of the same state are displayed together, otherwise you can switch between states with :ref:`,<LC_,>`, :ref:`.<LC_.>`, or :ref:`SPACE<SPACE>`. Some states are anatomically correct, like pial, and white. Some such as sphere or inflated are not.
      
      For the remaining objects, the previous definition of state no longer applies, but it is still used as a label for grouping what gets displayed together. For instance, a volume is internally labeled as having *ANY_ANATOMICAL* as its state, which is codestate to disply it along with any visualization state that is anatomically correct. This way, volumes are displayed whether you're looking at the pial surfaces or the smoothed white matter surfaces. The same goes for graphs that are displayed in 3D, however graphs are also displayed in matrix form which has its own state and is displayed without anatomically correct objects with it.

   DO
   DOs
   Displayable Object
   Displayable Objects
      A SUMA displayable object such as lines, spheres, text, images, planes, etc. See documentation under SUMA DO loading instructions with :ref:`Ctrl+Alt+s<LC_Ctrl+Alt+s>`, and NIML formatted DOs (nido) material from the output of :ref:`suma -help_nido <suma--help_nido>`   
