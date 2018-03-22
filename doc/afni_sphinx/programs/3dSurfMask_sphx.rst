**********
3dSurfMask
**********

.. _ahelp_3dSurfMask:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dSurfMask <-i_TYPE SURFACE> <-prefix PREFIX> 
                    [<-fill_method METH>] 
                    <-grid_parent GRID_VOL> [-sv SURF_VOL] [-mask_only]
     
      Creates 2 volumetric datasets that mark voxel based on their
      location relative to the surface.
      Voxels in the first volume (named PREFIX.m) label voxel positions 
      relative to the surface. With -fill_method set to FAST, you get a 
      a CRUDE mask with voxel values set to the following:
         0: Voxel outside surface
         1: Voxel just outside the surface. This means the voxel
            center is outside the surface but inside the 
            bounding box of a triangle in the mesh. 
         2: Voxel intersects the surface (a triangle), 
            but center lies outside.
         3: Voxel contains a surface node.
         4: Voxel intersects the surface (a triangle), 
            center lies inside surface. 
         5: Voxel just inside the surface. This means the voxel
            center is inside the surface and inside the 
            bounding box of a triangle in the mesh. 
         6: Voxel inside the surface. 
      Masks obtained with -fill_method FAST could have holes in them.
      To decide on whether a voxel lies inside or outside the surface
      you should use the signed distances in PREFIX.d below, or use
      -fill_method slow.
    
      With -fill_method set to SLOW you get a better mask with voxels set
      to the following:
         0: Voxel outside surface
         1: Voxel outside the surface but in its bounding box
         2: Voxel inside the surface 
    
      Voxels values in the second volume (named PREFIX.d) reflect the 
      shortest distance of voxels in PREFIX.m to the surface.
      The distances are signed to reflect whether a voxel is inside 
      or outsider the surface. Voxels inside the surface have positive
      distances, voxels outside have a negative distance.
      If the signs appear reversed, use option -flip_orientation.
    
      Mandatory Parameters:
         -i_TYPE SURFACE: Specify input surface.
                 You can also use -t* and -spec and -surf
                 methods to input surfaces. See below
                 for more details.
         -prefix PREFIX: Prefix of output dataset.
         -grid_parent GRID_VOL: Specifies the grid for the
                      output volume.
      Other parameters:
         -mask_only: Produce an output dataset where voxels
                     are 1 inside the surface and 0 outside,
                     instead of the more nuanced output above.
         -flip_orientation: Flip triangle winding of surface mesh.
                            Use this option when the sign of the distances
                            in PREFIX.m comes out wrong. Voxels inside
                            the surface have a positive distance.
                            This can happen when the winding of the triangles
                            is reversed.
         -fill_method METH: METH can take two values; SLOW, and FAST[default].
                            FAST can produce holes under certain conditions.
         -no_dist: Do not compute the distances, just the mask from the first 
                   step.
    
     Example: (tcsh syntax)
      1- Find distance of voxels around and inside of toy surface:
    
      echo 'Create toy data' 
        @auto_tlrc -base TT_N27+tlrc -base_copy ToyVolume 
    
        CreateIcosahedron -rad 50 -ld 1
        sed 's/Anatomical = N/Anatomical = Y/' CreateIco.spec > __ttt 
        mv __ttt CreateIco.spec
    
      echo 'Do computations'
        3dSurfMask -i_fs CreateIco.asc -sv ToyVolume+tlrc \
                    -prefix ToyMasks -flip_orientation   \
                    -grid_parent ToyVolume+tlrc 
    
      echo 'Cut and paste commands below to show you the results'
        suma -npb 70 -niml -spec CreateIco.spec -sv ToyVolume+tlrc &
        afni -npb 70 -niml -yesplugouts & 
        DriveSuma -npb 70 -com viewer_cont -key 't'  
        plugout_drive -npb 70 -com 'SET_OVERLAY A ToyMasks.d' \
                              -com 'SET_THRESHOLD A.0'  \
                              -com 'SET_PBAR_NUMBER A.10'  \
                              -quit 
    
     See also examples in SurfPatch -help
    
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
    
