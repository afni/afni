.. _ahelp_Surf2VolCoord:

*************
Surf2VolCoord
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: Surf2VolCoord <-i_TYPE SURFACE> 
                          <-grid_parent GRID_VOL> 
                          [-grid_subbrick GSB]
                          [-sv SURF_VOL] 
                          [-one_node NODE]
                          [-closest_nodes XYZ.1D]
     
      Relates node indices to coordinates:
      ------------------------------------
      Given x y z coordinates, return the nodes closest to them.
      For example:
          Surf2VolCoord    -i SUMA/std60.lh.pial.asc \
                           -i SUMA/std60.rh.pial.asc \
                           -sv anat+tlrc. -qual LR   \
                           -closest_nodes XYZ.1D
          If you are not sure you have the proper -sv, verify with SUMA:
            suma -i SUMA/std60.lh.pial.asc  \
                 -i SUMA/std60.rh.pial.asc  \
                 -sv anat+tlrc. -niml &
            afni -niml &
          Then press 't' in SUMA to send surfaces to AFNI.
    
      Mandatory Parameters:
         -closest_nodes XYZ.1D: A coordinate file specifying coordinates
                               for which the closest nodes will be found.
                      Note: The coordinates in XYZ.1D are in RAI by default.
                            You can use -LPI if you need to.
         -closest_node 'X Y Z': An easier way to specify a single node's coords.
      Optional Parameters:
         -qual STRING: A string of characters that are used to indentify
                       the surface in which the closest node was found.
                       This is useful when you have two surfaces specified
                       like the left and right hemispheres for example.
                       In that case you can set STRING to LR if the first
                       surface is the left hemisphere and the second is the
                       right hemisphere. If you had node 12342 on the left hemi
                       and 7745 on the right, the output would look like this:
                            1342L 
                            7745R 
                       If qual is not set, no qualifying characters are added if 
                       up only have one surface on the command line. 
                       The sequence ABC... is used otherwise.
         -LPI: The coordinate axis direction for values in XYZ.1D are in LPI.
               As a result, the program will negate the sign of the X, and Y
               coordinates in XYZ.1D 
         -RAI: The coordinate axis direction for values in XYZ.1D are in RAI
               which is the default. No transformation is applied to values in
               XYZ.1D
         -verb LEVEL: Verbosity level, default is 0
         -prefix PREFIX: Output results to file PREFIX (will overwrite).
                         Default is stdout
    
      In Demo mode:
      -------------
      Illustrates how surface coordinates relate to voxel grid.
      The program outputs surface and equivalent volume coordinates
      for all nodes in the surface after it is aligned via its sv.
      The code is intended as a source code demo.
    
      Mandatory Parameters:
         -i_TYPE SURFACE: Specify input surface.
                 You can also use -t* and -spec and -surf
                 methods to input surfaces. See below
                 for more details.
         -prefix PREFIX: Prefix of output dataset.
         -grid_parent GRID_VOL: Specifies the grid for the
                      output volume.
      Optional Parameters:
         -grid_subbrick GSB: Sub-brick from which data are taken.
         -one_node NODE: Output results for node NODE only.
    
    The output is lots of text so you're better off redirecting to a file.
    Once you load a surface and its surface volume,,
    its node coordinates are transformed based on the
    surface format type and the transforms stored in
    the surface volume. At this stage, the node coordinates
    are in what we call RAImm DICOM where x coordinate is
    from right (negative) to left (positive) and y coordinate
    from anterior to posterior and z from inferior to superior
    This RAI coordinate corresponds to the mm coordinates
    displayed by AFNI in the top left corner of the controller
    when you have RAI=DICOM order set (right click on coordinate
    text are to see option. When you open the surface with the
    same sv in SUMA and view the sv volume in AFNI, the coordinate
    of a node on an anatomically correct surface should be close
    to the coordinate displayed in AFNI.
    In the output, RAImm is the coordinate just described for a 
    particular node.
    The next coordinate in the output is called 3dfind, which stands
    for three dimensional float index. 3dfind is a transformation 
    of the RAImm coordinates to a coordinate in the units of the
    voxel grid. The voxel with the closest center to a location
    at RAImm would then be at round(3dfind). In other terms, 
    RAImm is the coordinate closest to voxel  
     V(round(3dfind[0]), round(3dfind[1]), round(3dfind[2])
    To see index coordinates, rather than mm coordinates in 
    AFNI, set: Define Datamode --> Misc --> Voxel Coords?
    Note that the index coordinates would be different for the
    underlay and overlay because they are usually at different
    resolution and/or orientation. To see the overlay coordinates
    make sure you have 'See Overlay' turned on.
    The last value in the output is the value from the chosen
    sub-brick
    
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
                     /home/ptaylor/.afni/data/
                   If you have no surface templates there, download
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz
                   and/or
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz
                   and untar them under directory /home/ptaylor/.afni/data/
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
                 afni -niml /home/ptaylor/.afni/data/suma_MNI_N27 
    
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
                     /home/ptaylor/.afni/data/
                   If you have no surface templates there, download
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz
                   and/or
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz
                   and untar them under directory /home/ptaylor/.afni/data/
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
                 afni -niml /home/ptaylor/.afni/data/suma_MNI_N27 
    
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
      
    
