*************
SurfRetinoMap
*************

.. _SurfRetinoMap:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: SurfRetinoMap <SURFACE> <-input POLAR ECCENTRICITY>
                     [<-prefix PREFIX>] [<-node_dbg NODE>]
          
      <SURFACE> : Surface on which distances are computed.
                  (For option's syntax, see 
                  'Specifying input surfaces' section below).
      <-input POLAR ECCENTRICITY>: Retinotopic datasets.
                  POLAR is the polar angle dataset.
                  ECCENTRICITY is the eccentricity angle dataset.
         Those datasets are produced by 3dRetinoPhase.
         If the datasets are produced outside of 3dRetinoPhase, note that
         The angle data is to be in the [0] sub-brick, and a thresholding
         parameter, if any, is to be in the [2] sub-brick.
    
      [<-node_dbg NODE>]: Index of node number for which debugging
                        information is output.
      [<-prefix PREFIX>]: Prefix for output datasets.
                          The program outputs the Visual Field Ratio (VFR),
                          the sign of which is used to differentiate between
                          adjacent areas. 
    
         VFR computations based on paper by Warnking et al. Neuroimage 17, (2002)
                'FMRI Retinotopic Mapping - Step by Step
    
    A note on the output thresholding sub-brick:
      In addition to VFR, you get a maximum threshold
         sub-brick which retains the highest threshold at
         each node in input datasets. This thresholding 
         parameter is like a union mask of input data 
         thresholded at the same level.
      The signficance value is not provided on purpose.
         I don't know of a good way to compute it, but 
         it serves its function or wedding out low SNR nodes.
    
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
    
      SUMA dataset input options:
          -input DSET: Read DSET1 as input.
                       In programs accepting multiple input datasets
                       you can use -input DSET1 -input DSET2 or 
                       input DSET1 DSET2 ...
           NOTE: Selecting subsets of a dataset:
                 Much like in AFNI, you can select subsets of a dataset
                 by adding qualifiers to DSET.
               Append #SEL# to select certain nodes.
               Append [SEL] to select certain columns.
               Append {SEL} to select certain rows.
               The format of SEL is the same as in AFNI, see section:
               'INPUT DATASET NAMES' in 3dcalc -help for details.
               Append [i] to get the node index column from
                          a niml formatted dataset.
               *  SUMA does not preserve the selection order 
                  for any of the selectors.
                  For example:
                  dset[44,10..20] is the same as dset[10..20,44]
                  Also, duplicate values are not supported.
                  so dset[13, 13] is the same as dset[13].
                  I am not proud of these limitations, someday I'll get
                  around to fixing them.
    
    
    
     SUMA mask options:
          -n_mask INDEXMASK: Apply operations to nodes listed in
                                INDEXMASK  only. INDEXMASK is a 1D file.
          -b_mask BINARYMASK: Similar to -n_mask, except that the BINARYMASK
                              1D file contains 1 for nodes to filter and
                              0 for nodes to be ignored.
                              The number of rows in filter_binary_mask must be
                              equal to the number of nodes forming the
                              surface.
          -c_mask EXPR: Masking based on the result of EXPR. 
                        Use like afni's -cmask options. 
                        See explanation in 3dmaskdump -help 
                        and examples in output of 3dVol2Surf -help
          NOTE: Unless stated otherwise, if n_mask, b_mask and c_mask 
                are used simultaneously, the resultant mask is the intersection
                (AND operation) of all masks.
    
    
    
      SUMA communication options:
          -talk_suma: Send progress with each iteration to SUMA.
          -refresh_rate rps: Maximum number of updates to SUMA per second.
                             The default is the maximum speed.
          -send_kth kth: Send the kth element to SUMA (default is 1).
                         This allows you to cut down on the number of elements
                         being sent to SUMA.
          -sh <SumaHost>: Name (or IP address) of the computer running SUMA.
                          This parameter is optional, the default is 127.0.0.1 
          -ni_text: Use NI_TEXT_MODE for data transmission.
          -ni_binary: Use NI_BINARY_MODE for data transmission.
                      (default is ni_binary).
          -feed_afni: Send updates to AFNI via SUMA's talk.
       -np PORT_OFFSET: Provide a port offset to allow multiple instances of
                        AFNI <--> SUMA, AFNI <--> 3dGroupIncorr, or any other
                        programs that communicate together to operate on the same
                        machine. 
                        All ports are assigned numbers relative to PORT_OFFSET.
             The same PORT_OFFSET value must be used on all programs
               that are to talk together. PORT_OFFSET is an integer in
               the inclusive range [1025 to 65500]. 
             When you want to use multiple instances of communicating programs, 
               be sure the PORT_OFFSETS you use differ by about 50 or you may
               still have port conflicts. A BETTER approach is to use -npb below.
       -npq PORT_OFFSET: Like -np, but more quiet in the face of adversity.
       -npb PORT_OFFSET_BLOC: Similar to -np, except it is easier to use.
                              PORT_OFFSET_BLOC is an integer between 0 and
                              MAX_BLOC. MAX_BLOC is around 4000 for now, but
                              it might decrease as we use up more ports in AFNI.
                              You should be safe for the next 10 years if you 
                              stay under 2000.
                              Using this function reduces your chances of causing
                              port conflicts.
    
             See also afni and suma options: -list_ports and -port_number for 
                information about port number assignments.
    
             You can also provide a port offset with the environment variable
                AFNI_PORT_OFFSET. Using -np overrides AFNI_PORT_OFFSET.
    
       -max_port_bloc: Print the current value of MAX_BLOC and exit.
                       Remember this value can get smaller with future releases.
                       Stay under 2000.
       -max_port_bloc_quiet: Spit MAX_BLOC value only and exit.
       -num_assigned_ports: Print the number of assigned ports used by AFNI 
                            then quit.
       -num_assigned_ports_quiet: Do it quietly.
    
         Port Handling Examples:
         -----------------------
             Say you want to run three instances of AFNI <--> SUMA.
             For the first you just do: 
                suma -niml -spec ... -sv ...  &
                afni -niml &
             Then for the second instance pick an offset bloc, say 1 and run
                suma -niml -npb 1 -spec ... -sv ...  &
                afni -niml -npb 1 &
             And for yet another instance:
                suma -niml -npb 2 -spec ... -sv ...  &
                afni -niml -npb 2 &
             etc.
    
             Since you can launch many instances of communicating programs now,
                you need to know wich SUMA window, say, is talking to which AFNI.
                To sort this out, the titlebars now show the number of the bloc 
                of ports they are using. When the bloc is set either via 
                environment variables AFNI_PORT_OFFSET or AFNI_PORT_BLOC, or  
                with one of the -np* options, window title bars change from 
                [A] to [A#] with # being the resultant bloc number.
             In the examples above, both AFNI and SUMA windows will show [A2]
                when -npb is 2.
    
    
    
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
       Mar  7 2018
    
