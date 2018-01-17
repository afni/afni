**************
SurfaceMetrics
**************

.. _SurfaceMetrics:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage: SurfaceMetrics <-Metric1> [[-Metric2] ...] 
                      <-SURF_1> 
                      [-tlrc] [<-prefix prefix>]
    
    Outputs information about a surface's mesh
    
       -Metric1: Replace -Metric1 with the following:
          -vol: calculates the volume of a surface.
                Volume unit is the cube of your surface's
                coordinates unit, obviously.
                Volume's sign depends on the orientation
                of the surface's mesh.
                Make sure your surface is a closed one
                and that winding is consistent.
                Use SurfQual to check the surface.
                If your surface's mesh has problems,
                the result is incorrect. 
                Volume is calculated using Gauss's theorem,
                see [Hughes, S.W. et al. 'Application of a new 
                discreet form of Gauss's theorem for measuring 
                volume' in Phys. Med. Biol. 1996].
          -conv: output surface convexity at each node.
             Output file is prefix.conv. Results in two columns:
             Col.0: Node Index
             Col.1: Convexity
             This is the measure used to shade sulci and gyri in SUMA.
             C[i] = Sum(dj/dij) over all neighbors j of i
             dj is the distance of neighboring node j to the tangent plane at i
             dij is the length of the segment ij
             Note: This option produces a .1D file, and a NIML dataset with 
             similar content.
          -closest_node XYZ_LIST.1D: Find the closest node on the surface
                                  to each XYZ triplet in XYZ_LIST.1D
                                  Note that it is assumed that the XYZ
                                  coordinates are in RAI (DICOM) per AFNI's
                                  coordinate convention. For correspondence
                                  with coordinates observed in SUMA and AFNI
                                  be sure to use the proper -sv parameter for
                                  the surface and XYZ coordinates in question.
             Output file is prefix.closest.1D. Results in 8 columns:
             Col.0: Index of closest node.
             Col.1: Distance of closest node to XYZ reference point.
             Col.2..4: XYZ of reference point (same as XYZ_LIST.1D, copied 
                       here for clarity).
             Col.5..7: XYZ of closest node (after proper surface coordinate
                       transformation, including SurfaceVolume transform.
          -area: output area of each triangle. 
             Output file is prefix.area. Results in two columns:
             Col.0: Triangle Index
             Col.1: Triangle Area
          -tri_sines/-tri_cosines: (co)sine of angles at nodes forming
                                       triangles.
             Output file is prefix.(co)sine. Results in 4 columns:
             Col.0: Triangle Index
             Col.1: (co)sine of angle at node 0
             Col.2: (co)sine of angle at node 1
             Col.3: (co)sine of angle at node 2
          -tri_CoSines: Both cosines and sines.
          -tri_angles: Unsigned angles in radians of triangles.
             Col.0: Triangle Index
             Col.1: angle at node 0
             Col.2: angle at node 1
             Col.3: angle at node 2
          -node_angles: Unsigned angles in radians at nodes of surface.
             Col.0: Node Index
             Col.1: minimum angle at node 
             Col.2: maximum angle at node 
             Col.3: average angle at node 
          -curv: output curvature at each node.
             Output file is prefix.curv. Results in nine columns:
             Col.0: Node Index
             Col.1-3: vector of 1st principal direction of surface
             Col.4-6: vector of 2nd principal direction of surface
             Col.7: Curvature along T1
             Col.8: Curvature along T2
             Col.9: Curvature magnitude sqrt(c7*c7+c8*c8)
             Curvature algorithm by G. Taubin from: 
             'Estimating the tensor of curvature of surface 
             from a polyhedral approximation.'
             Note: This option produces a .1D file, a NIML dataset with similar
             content, and Displayable Objects (DO) file containing 
             the principal directions at each node. You can load these objects
             with SUMA's 'Alt+Ctrl+s' option.
          -edges: outputs info on each edge. 
             Output file is prefix.edges. Results in five columns:
             Col.0: Edge Index (into a SUMA structure).
             Col.1: Index of the first node forming the edge
             Col.2: Index of the second node forming the edge
             Col.3: Number of triangles containing edge
             Col.4: Length of edge.
          -node_normals: Outputs segments along node normals.
                         Segments begin at node and have a default
                         magnitude of 1. See option 'Alt+Ctrl+s' in 
                         SUMA for visualization.
             Note: This option produces a .1D file and a Displayable Objects 
             file containing  the principal directions at each node. 
             You can load these objects with SUMA's 'Alt+Ctrl+s' option.
          -face_normals: Outputs segments along triangle normals.
                         Segments begin at centroid of triangles and 
                         have a default magnitude of 1. See option 
                         'Alt+Ctrl+s' in SUMA for visualization.
          -normals_scale SCALE: Scale the normals by SCALE (1.0 default)
                         For use with options -node_normals and -face_normals
          -coords: Output coords of each node after any transformation 
             that is normally carried out by SUMA on such a surface.
             Col. 0: Node Index
             Col. 1: X
             Col. 2: Y
             Col. 3: Z
          -sph_coords: Output spherical coords of each node.
          -sph_coords_center x y z: Shift each node by  x y z
                                    before calculating spherical
                                    coordinates. Default is the
                                    center of the surface.
              Both sph_coords options output the following:
              Col. 0: Node Index
              Col. 1: R (radius)
              Col. 2: T (azimuth)
              Col. 3: P (elevation)
          -boundary_nodes: Output nodes that form a boundary of a surface
                       i.e. they form edges that belong to one and only
                       one triangle.
          -boundary_triangles: Output triangles that form a boundary of a surface
                       i.e. they contain edges that belong to one and only
                       one triangle.
          -internal_nodes: Output nodes that are not a boundary.
                       i.e. they form edges that belong to more than
                       one triangle.
    
          You can use any or all of these metrics simultaneously.
    
         (-SURF_1):  An option for specifying the surface.
                     (For option's syntax, see 'Specifying input surfaces'
                     section below).
    
       -sv SurfaceVolume [VolParam for sf surfaces]: Specify a surface volume
                       for surface alignment. See ConvertSurface -help for 
                       more info.
    
       -tlrc: Apply Talairach transform to surface.
                       See ConvertSurface -help for more info.
    
       -prefix prefix: Use prefix for output files. 
                       (default is prefix of inSurf)
    
       -quiet: Quiet
    
        Options for applying arbitrary affine transform:
        [xyz_new] = [Mr] * [xyz_old - cen] + D + cen
        -xmat_1D mat: Apply transformation specified in 1D file mat.1D.
                      to the surface's coordinates.
                      [mat] = [Mr][D] is of the form:
                      r11 r12 r13 D1
                      r21 r22 r23 D2
                      r31 r32 r33 D3
                      or
                      r11 r12 r13 D1 r21 r22 r23 D2 r31 r32 r33 D3
        -ixmat_1D mat: Same as xmat_1D except that mat is replaced by inv(mat)
    
            NOTE: For both -xmat_1D and -ixmat_1D, you can replace mat with 
                  one of the special strings:
                  'RandShift', 'RandRigid', or 'RandAffine' which would create
                  a transform on the fly. 
        -seed SEED: Use SEED to seed the random number generator for random
                    matrix generation
    
        -xcenter x y z: Use vector cen = [x y z]' for rotation center.
                        Default is cen = [0 0 0]'
        -polar_decomp: Apply polar decomposition to mat and preserve
                       orthogonal component and shift only. 
                       For more information, see cat_matvec's -P option.
                       This option can only be used in conjunction with
                       -xmat_1D
        -h: Show most of the options
        -help: Show all of the options
    
     Specifying input surfaces using -i or -i_TYPE options: 
        -i_TYPE inSurf specifies the input surface,
                TYPE is one of the following:
           fs: FreeSurfer surface. 
               If surface name has .asc it is assumed to be
               in ASCII format. Otherwise it is assumed to be
               in BINARY_BE (Big Endian) format.
               Patches in Binary format cannot be read at the moment.
           sf: SureFit surface. 
               You must specify the .coord followed by the .topo file.
           vec (or 1D): Simple ascii matrix format. 
                You must specify the coord (NodeList) file followed by 
                the topo (FaceSetList) file.
                coord contains 3 floats per line, representing 
                X Y Z vertex coordinates.
                topo contains 3 ints per line, representing 
                v1 v2 v3 triangle vertices.
           ply: PLY format, ascii or binary.
                Only vertex and triangulation info is preserved.
           stl: STL format, ascii or binary.
                This format of no use for much of the surface-based
                analyses. Objects are defined as a soup of triangles
                with no information about which edges they share. STL is only
                useful for taking surface models to some 3D printing 
                software.
           mni: MNI .obj format, ascii only.
                Only vertex, triangulation, and node normals info is preserved.
           byu: BYU format, ascii.
                Polygons with more than 3 edges are turned into
                triangles.
           bv: BrainVoyager format. 
               Only vertex and triangulation info is preserved.
           dx: OpenDX ascii mesh format.
               Only vertex and triangulation info is preserved.
               Requires presence of 3 objects, the one of class 
               'field' should contain 2 components 'positions'
               and 'connections' that point to the two objects
               containing node coordinates and topology, respectively.
           gii: GIFTI XML surface format.
           obj: OBJ file format for triangular meshes only. The following
                primitives are preserved: v (vertices),  (faces, triangles
                only), and p (points)
     Note that if the surface filename has the proper extension, 
     it is enough to use the -i option and let the programs guess
     the type from the extension.
    
     You can also specify multiple surfaces after -i option. This makes
     it possible to use wildcards on the command line for reading in a bunch
     of surfaces at once.
    
         -onestate: Make all -i_* surfaces have the same state, i.e.
                    they all appear at the same time in the viewer.
                    By default, each -i_* surface has its own state. 
                    For -onestate to take effect, it must precede all -i
                    options with on the command line. 
         -anatomical: Label all -i surfaces as anatomically correct.
                    Again, this option should precede the -i_* options.
    
     More variants for option -i:
    -----------------------------
     You can also load standard-mesh spheres that are formed in memory
     with the following notation
         -i ldNUM:  Where NUM is the parameter controlling
                    the mesh density exactly as the parameter -ld linDepth
                    does in CreateIcosahedron. For example: 
                        suma -i ld60
                    create on the fly a surface that is identical to the
                    one produced by: CreateIcosahedron -ld 60 -tosphere
         -i rdNUM: Same as -i ldNUM but with NUM specifying the equivalent
                   of parameter -rd recDepth in CreateIcosahedron.
    
     To keep the option confusing enough, you can also use -i to load
     template surfaces. For example:
               suma -i lh:MNI_N27:ld60:smoothwm 
     will load the left hemisphere smoothwm surface for template MNI_N27 
     at standard mesh density ld60.
     The string following -i is formatted thusly:
         HEMI:TEMPLATE:DENSITY:SURF where:
         HEMI specifies a hemisphere. Choose from 'l', 'r', 'lh' or 'rh'.
              You must specify a hemisphere with option -i because it is 
              supposed to load one surface at a time. 
              You can load multiple surfaces with -spec which also supports 
              these features.
         TEMPLATE: Specify the template name. For now, choose from MNI_N27 if
                   you want to use the FreeSurfer reconstructed surfaces from
                   the MNI_N27 volume, or TT_N27
                   Those templates must be installed under this directory:
                     /Users/discoraj/.afni/data/
                   If you have no surface templates there, download
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz
                   and/or
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz
                   and untar them under directory /Users/discoraj/.afni/data/
         DENSITY: Use if you want to load standard-mesh versions of the template
                  surfaces. Note that only ld20, ld60, ld120, and ld141 are in
                  the current distributed templates. You can create other 
                  densities if you wish with MapIcosahedron, but follow the
                  same naming convention to enable SUMA to find them.
         SURF: Which surface do you want. The string matching is partial, as long
               as the match is unique. 
               So for example something like: suma -i l:MNI_N27:ld60:smooth
               is more than enough to get you the ld60 MNI_N27 left hemisphere
               smoothwm surface.
         The order in which you specify HEMI, TEMPLATE, DENSITY, and SURF, does
         not matter.
         For template surfaces, the -sv option is provided automatically, so you
         can have SUMA talking to AFNI with something like:
                 suma -i l:MNI_N27:ld60:smooth &
                 afni -niml /Users/discoraj/.afni/data/suma_MNI_N27 
    
     Specifying surfaces using -t* options: 
       -tn TYPE NAME: specify surface type and name.
                      See below for help on the parameters.
       -tsn TYPE STATE NAME: specify surface type state and name.
            TYPE: Choose from the following (case sensitive):
               1D: 1D format
               FS: FreeSurfer ascii format
               PLY: ply format
               MNI: MNI obj ascii format
               BYU: byu format
               SF: Caret/SureFit format
               BV: BrainVoyager format
               GII: GIFTI format
            NAME: Name of surface file. 
               For SF and 1D formats, NAME is composed of two names
               the coord file followed by the topo file
            STATE: State of the surface.
               Default is S1, S2.... for each surface.
     Specifying a Surface Volume:
        -sv SurfaceVolume [VolParam for sf surfaces]
           If you supply a surface volume, the coordinates of the input surface.
            are modified to SUMA's convention and aligned with SurfaceVolume.
            You must also specify a VolParam file for SureFit surfaces.
     Specifying a surface specification (spec) file:
        -spec SPEC: specify the name of the SPEC file.
         As with option -i, you can load template
         spec files with symbolic notation trickery as in:
                        suma -spec MNI_N27 
         which will load the all the surfaces from template MNI_N27
         at the original FreeSurfer mesh density.
      The string following -spec is formatted in the following manner:
         HEMI:TEMPLATE:DENSITY where:
         HEMI specifies a hemisphere. Choose from 'l', 'r', 'lh', 'rh', 'lr', or
              'both' which is the default if you do not specify a hemisphere.
         TEMPLATE: Specify the template name. For now, choose from MNI_N27 if
                   you want surfaces from the MNI_N27 volume, or TT_N27
                   for the Talairach version.
                   Those templates must be installed under this directory:
                     /Users/discoraj/.afni/data/
                   If you have no surface templates there, download
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz
                   and/or
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz
                   and untar them under directory /Users/discoraj/.afni/data/
         DENSITY: Use if you want to load standard-mesh versions of the template
                  surfaces. Note that only ld20, ld60, ld120, and ld141 are in
                  the current distributed templates. You can create other 
                  densities if you wish with MapIcosahedron, but follow the
                  same naming convention to enable SUMA to find them.
                  This parameter is optional.
         The order in which you specify HEMI, TEMPLATE, and DENSITY, does
         not matter.
         For template surfaces, the -sv option is provided automatically, so you
         can have SUMA talking to AFNI with something like:
                 suma -spec MNI_N27:ld60 &
                 afni -niml /Users/discoraj/.afni/data/suma_MNI_N27 
    
     Specifying a surface using -surf_? method:
        -surf_A SURFACE: specify the name of the first
                surface to load. If the program requires
                or allows multiple surfaces, use -surf_B
                ... -surf_Z .
                You need not use _A if only one surface is
                expected.
                SURFACE is the name of the surface as specified
                in the SPEC file. The use of -surf_ option 
                requires the use of -spec option.
    
       [-novolreg]: Ignore any Rotate, Volreg, Tagalign, 
                    or WarpDrive transformations present in 
                    the Surface Volume.
       [-noxform]: Same as -novolreg
       [-setenv "'ENVname=ENVvalue'"]: Set environment variable ENVname
                    to be ENVvalue. Quotes are necessary.
                 Example: suma -setenv "'SUMA_BackgroundColor = 1 0 1'"
                    See also options -update_env, -environment, etc
                    in the output of 'suma -help'
      Common Debugging Options:
       [-trace]: Turns on In/Out debug and Memory tracing.
                 For speeding up the tracing log, I recommend 
                 you redirect stdout to a file when using this option.
                 For example, if you were running suma you would use:
                 suma -spec lh.spec -sv ... > TraceFile
                 This option replaces the old -iodbg and -memdbg.
       [-TRACE]: Turns on extreme tracing.
       [-nomall]: Turn off memory tracing.
       [-yesmall]: Turn on memory tracing (default).
      NOTE: For programs that output results to stdout
        (that is to your shell/screen), the debugging info
        might get mixed up with your results.
    
    
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
    
    
    Compile Date:
       Nov  9 2017
    
