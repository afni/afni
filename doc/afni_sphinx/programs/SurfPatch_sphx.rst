.. contents:: 
    :depth: 4 

*********
SurfPatch
*********

.. code-block:: none

    
    Usage:
      SurfPatch <-spec SpecFile> <-surf_A insurf> <-surf_B insurf> ...
                <-input nodefile inode ilabel> <-prefix outpref>  
                [-hits min_hits] [-masklabel msk] [-vol] [-patch2surf]
                [-vol_only] [-coord_gain] [-check_bowtie] [-fix_bowtie] 
                [-ok_bowtie] [-adjust_contour] [-do-not-adjust_contour] 
                [-stiched_surface SURF]   
    
    Usage 1:
      The program creates a patch of surface formed by nodes 
      in nodefile.
      The program can also be used to calculate the volume between the same patch
      on two isotopic surfaces. See -vol option below.
    
         -spec SpecFile: Spec file containing input surfaces.
         -surf_X: Name of input surface X where X is a character
                  from A to Z. If surfaces are specified using two
                  files, use the name of the node coordinate file.
         -input nodefile inode ilabel: 
                nodefile is the file containing nodes defining the patch.
                inode is the index of the column containing the nodes
                ilabel is the index of the column containing labels of
                       the nodes in column inode. If you want to use
                       all the nodes in column indode, then set this 
                       parameter to -1 (default). 
                       If ilabel is not equal to 0 then the corresponding 
                       node is used in creating the patch.
                       See -masklabel option for one more variant.
         -prefix outpref: Prefix of output patch. If more than one surface
                          are entered, then the prefix will have _X added
                          to it, where X is a character from A to Z.
                          Output format depends on the input surface's.
                          With that setting, checking on pre-existing files
                          is only done before writing the new patch, which is
                          annoying. You can set the output type ahead of time
                          using -out_type option. This way checking for 
                          pre-existing output files can be done at the outset.
         -vol: Calculate the volume formed by the patch on surf_A and
                and surf_B. For this option, you must specify two and
                only two surfaces with surf_A and surf_B options.
         -vol_only: Only calculate the volume, don't write out patches.
                    See also -fix_bowtie option below.
    
      ** If you are more interested in the volume attributed to one node, or a 
         set of nodes, between two isotopic surfaces, you are much better off 
         using SurfMeasures' -node_volg option. SurfMeasures has an efficient 
         implementation of the Gauss Theorem based volume estimation.
    
         -out_type TYPE: Type of all output patches, regardless of input 
                         surface type.
                         Choose from: FreeSurfer, SureFit, 1D and Ply.
         -hits min_hits: Minimum number of nodes specified for a triangle
                         to be made a part of the patch (1 <= min_hits <= 3)
                         default is 2.
         -masklabel msk: If specified, then only nodes that are labeled with
                         with msk are considered for the patch.
                         This option is useful if you have an ROI dataset file
                         and whish to create a patch from one out of many ROIs
                         in that file. This option must be used with ilabel 
                         specified (not = -1)
         -patch2surf: Turn surface patch into a surface where only nodes used in
                      forming the mesh are preserved.
         -node_depth NODE_DEPTH: Compute depth of each node after projection onto
                      the 1st principal direction of the nodes making up the
                      surface. The results are written in a file with prefix
                      NODE_DEPTH.pcdepth.1D.dset. You must use -patch2surf 
                      in order to use this option. 
                      Option is similar to the one in program ConvertSurface.
         -check_bowtie: Check if the patch has a section hanging by one node to
                        the rest of the mesh. Think of a patch made of two 
                        triangles that are connected at one node only. 
                        Think Bowtie. Bowties should not occur if original 
                        surface is 2 manifold and min_hits == 1
                        -check_bowtie is the default when -vol or -vol_only 
                        are used. Volume computation will fail in the presence
                        of bowties.
         -fix_bowtie: Modify patch to eliminate bowties. This only works if 
                      min_hits is > 1. The repair is done by relaxing min_hits
                      at the node(s) where the bowtie happens.
         -ok_bowtie: Do not check for, or fix bowties. 
                     Default when -vol* are not used.
         -adjust_contour: Once the patch is created, shrink its contours at nodes
                          that were not in nodefile (non-selected).
                          Each non-selected node is moved to the center of mass
                          of itself and neighboring selected nodes.
                          This adjustment might make sense when min_hits < 3
                          
         -do-not-adjust_contour:  Do not adjust contrours.
                                  This is the default.
         -stiched_surface STICHED: Write out the stiched surface used to
                                   calculate the volume. 
                                   If -adjust_contour is used, this option also
                                   writes out a file that shows which 
                                   nodes on the original surface were adjusted.
                                   The first column in the node number. The 2nd
                                   contains the number of selected nodes that 
                                   neighbored non-selected nodes in the patch.
         -coord_gain GAIN: Multiply node coordinates by a GAIN.
                           That's useful if you have a tiny patch that needs
                           enlargement for easier viewing in SUMA.
                           Although you can zoon over very large ranges in SUMA
                           tiny tiny patches are hard to work with because
                           SUMA's parameters are optimized to work with objects
                           on the order of a brain, not on the order of 1 mm.
                           Gain is applied just before writing out patches.
         -flip_orientation: Change orientation of triangles before writing
                            surfaces.
         -verb VERB: Set verbosity level, 1 is the default.
    
       Example 1: Given an ROI, a white matter and a gray matter surface
                  calculate the volume of cortex enclosed by the roi on
                  both surfaces.
                  Assume you have the spec file and surfaces already. You can
                  get the same files from the SUMA directory in the AFNI 
                  workshop SUMA's archive which you can get with: 
             curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/SUMA_demo.tgz
    
                  Draw an ROI on the surface and save it as: lh.manualroi.1D.roi
    
             To calculate the volume and create a enclosing surface:
                 SurfPatch   -spec DemoSubj_lh.spec \
                             -sv DemoSubj_SurfVol+orig  \
                             -surf_A lh.smoothwm  \
                             -surf_B lh.pial   \
                             -prefix lh.patch \
                             -input lh.manualroi.1D.roi 0 -1  \
                             -out_type fs   \
                             -vol  \
                             -adjust_contour \
                             -stiched_surface lh.stiched   \
                             -flip_orientation 
    
       Example 2: If you want to voxelize the region between the two surfaces
                  you can run the following on the output.
                     3dSurfMask -i lh.stiched.ply \
                                -prefix lh.closed -fill_method SLOW \
                                -grid_parent DemoSubj_SurfVol+orig.HEAD 
                  3dSurfMask will output a dataset called lh.closed.d+orig which
                  contains the signed closest distance from each voxel to the 
                  surface. Negative distances are outside the surface.
    
                  To examine the results:
                     suma -npb 71 -i lh.stiched.ply -sv DemoSubj_SurfVol+orig. &
                     afni -npb 71 -niml -yesplugouts & 
                     DriveSuma -npb 71 -com viewer_cont -key 't' 
                     plugout_drive  -npb 71  \
                                    -com 'SET_OVERLAY lh.closed.d' \
                                    -com 'SET_FUNC_RANGE A.3' \
                                    -com 'SET_PBAR_NUMBER A.10' \
                                    -com 'SET_DICOM_XYZ A. 10 70 22 '\
                                    -quit
    
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
    
