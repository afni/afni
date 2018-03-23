.. _ahelp_ConvertSurface:

**************
ConvertSurface
**************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:  ConvertSurface <-i_TYPE inSurf> <-o_TYPE outSurf> 
                           [<-sv SurfaceVolume [VolParam for sf surfaces]>] 
                           [-tlrc] [-MNI_rai/-MNI_lpi][-xmat_1D XMAT]
        reads in a surface and writes it out in another format.
        Note: This is a not a general utility conversion program. 
        Only fields pertinent to SUMA are preserved.
    
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
    
        -ipar_TYPE ParentSurf specifies the parent surface. Only used
                when -o_fsp is used, see -o_TYPE options.
     Specifying a Surface Volume:
        -sv SurfaceVolume [VolParam for sf surfaces]
           If you supply a surface volume, the coordinates of the input surface.
            are modified to SUMA's convention and aligned with SurfaceVolume.
            You must also specify a VolParam file for SureFit surfaces.
     Specifying output surfaces using -o or -o_TYPE options: 
        -o_TYPE outSurf specifies the output surface, 
                TYPE is one of the following:
           fs: FreeSurfer ascii surface. 
           fsp: FeeSurfer ascii patch surface. 
                In addition to outSurf, you need to specify
                the name of the parent surface for the patch.
                using the -ipar_TYPE option.
                This option is only for ConvertSurface 
           sf: SureFit surface. 
               For most programs, you are expected to specify prefix:
               i.e. -o_sf brain. In some programs, you are allowed to 
               specify both .coord and .topo file names: 
               i.e. -o_sf XYZ.coord TRI.topo
               The program will determine your choice by examining 
               the first character of the second parameter following
               -o_sf. If that character is a '-' then you have supplied
               a prefix and the program will generate the coord and topo names.
           vec (or 1D): Simple ascii matrix format. 
                For most programs, you are expected to specify prefix:
                i.e. -o_1D brain. In some programs, you are allowed to 
                specify both coord and topo file names: 
                i.e. -o_1D brain.1D.coord brain.1D.topo
                coord contains 3 floats per line, representing 
                X Y Z vertex coordinates.
                topo contains 3 ints per line, representing 
                v1 v2 v3 triangle vertices.
           ply: PLY format, ascii or binary.
           stl: STL format, ascii or binary (see also STL under option -i_TYPE).
           byu: BYU format, ascii or binary.
           mni: MNI obj format, ascii only.
           gii: GIFTI format, ascii.
                You can also enforce the encoding of data arrays
                by using gii_asc, gii_b64, or gii_b64gz for 
                ASCII, Base64, or Base64 Gzipped. 
                If AFNI_NIML_TEXT_DATA environment variable is set to YES, the
                the default encoding is ASCII, otherwise it is Base64.
           obj: No support for writing OBJ format exists yet.
     Note that if the surface filename has the proper extension, 
     it is enough to use the -o option and let the programs guess
     the type from the extension.
    
      Alternate GIFTI output qualifiers:
         You can alternately set gifti data arrays encoding with:
            -xml_ascii: For ASCII  (human readable)
            -xml_b64:   For Base64 (more compact)
            -xml_b64gz: For Base64 GZIPPED (most compact, needs gzip libraries)
         If AFNI_NIML_TEXT_DATA environment variable is set to YES, the
         the default is -xml_ascii, otherwise it is -xml_b64
    
        -orient_out STR: Output coordinates in STR coordinate system. 
                          STR is a three character string following AFNI's 
                          naming convention. The program assumes that the   
                          native orientation of the surface is RAI, unless you 
                          use the -MNI_lpi option. The coordinate transformation
                          is carried out last, just before writing the surface 
                          to disk.
        -native: Write the output surface in the coordinate system native to its
                 format.
                 Option makes sense for BrainVoyager, Caret/SureFit and 
                 FreeSurfer surfaces.
                 But the implementation for Caret/Surefit is not finished yet 
                 (ask if needed).
        -make_consistent: Check the consistency of the surface's mesh (triangle
                          winding). This option will write out a new surface 
                          even if the mesh was consistent.
                          See SurfQual -help for mesh checks.
        -flip_orient: Flip the winding of the triangles
        -radial_to_sphere rad: Push each node along the center-->node direction
                               until |center-->node| = rad.
        -acpc: Apply acpc transform (which must be in acpc version of 
            SurfaceVolume) to the surface vertex coordinates. 
            This option must be used with the -sv option.
        -tlrc: Apply Talairach transform (which must be a talairach version of 
            SurfaceVolume) to the surface vertex coordinates. 
            This option must be used with the -sv option.
        -MNI_rai/-MNI_lpi: Apply Andreas Meyer Lindenberg's transform to turn 
            AFNI tlrc coordinates (RAI) into MNI coord space 
            in RAI (with -MNI_rai) or LPI (with -MNI_lpi)).
            NOTE: -MNI_lpi option has not been tested yet (I have no data
            to test it on. Verify alignment with AFNI and please report
            any bugs.
            This option can be used without the -tlrc option.
            But that assumes that surface nodes are already in
            AFNI RAI tlrc coordinates .
       NOTE: The vertex coordinates coordinates of the input surfaces are only
             transformed if -sv option is used. If you do transform surfaces, 
             take care not to load them into SUMA with another -sv option.
    
        -patch2surf: Change a patch, defined here as a surface with a mesh that
                     uses only a subset of the full nodelist, to a surface
                     where all the nodes in nodelist are used in the mesh.
                     Note that node indices will no longer correspond between
                     the input patch and the output surface.
        -merge_surfs: Merge multitudes of surfaces on the command line into one
                      big surface before doing anything else to the surface.
                      This is for the moment the only option for which you 
                      should specify more than one input surface on the command
                      line. For example:
                ConvertSurface -i lh.smoothwm.gii -i rh.smoothwm.gii \
                               -merge_surfs       -o_gii lrh.smoothwm.gii
    
       Options for coordinate projections:
       -node_depth DEPTHPREF: Project all coordinates onto the principal 
                              direction and output of depth/height of each
                              node relative to the outlying projection point.
                              This option is processed right before -pc_proj, 
                              should that option also be requested.
                              This option outputs file DEPTHPREF.pcdepth.1D.dset
                              which contains node index, followed by depth, then 
                              height of node. See also same option in SurfPatch
    
        -pc_proj ONTO PREFIX: Project coordinates onto ONTO, where ONTO is one 
                       of the parameters listed below.
                  ONTO values for plane projections along various normals:
                           PC0_plane = normal is 1st principal vector
                           PC1_plane = normal is  2nd principal vector
                           PC2_plane = normal is  3rd principal vector
                           PCZ_plane = normal is  component closest to Z axis
                           PCY_plane = normal is  component closest to Y axis
                           PCX_plane = normal is  component closest to X axis
                  ONTO values for line projections:
                           PC0_dir   = project along 1st principal vector
                           PC1_dir   = project along 2nd principal vector
                           PC2_dir   = project along 3rd principal vector
                           PCZ_dir   = project along component closest to Z axis
                           PCY_dir   = project along component closest to Y axis
                           PCX_dir   = project along component closest to X axis
                  PREFIX is used to form the name of the output file containing 
                           the projected coordinates. File PREFIX.xyzp.1D.coord
                           contains the projected coordinates.
        Note: This is the last operation to be performed by this program, 
              and no surfaces are written out in the end.
    
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
                  You can also use 'NegXY' to flip the sign of X and Y 
                  coordinates.
        -seed SEED: Use SEED to seed the random number generator for random
                    matrix generation
        -XYZscale sX sY sZ: Scale the coordinates by sX sY sZ.
                            This option essentially turns sX sY sZ.
                            into a -xmat_1D option. So you cannot mix
                            and match.
        -xcenter x y z: Use vector cen = [x y z]' for rotation center.
                        Default is cen = [0 0 0]'
        -polar_decomp: Apply polar decomposition to mat and preserve
                       orthogonal component and shift only. 
                       For more information, see cat_matvec's -P option.
                       This option can only be used in conjunction with
                       -xmat_1D
    
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
       Mar 22 2018
    
