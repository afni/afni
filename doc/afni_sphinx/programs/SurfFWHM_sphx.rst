.. _ahelp_SurfFWHM:

********
SurfFWHM
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: A program for calculating local and global FWHM.
    ------
     -input DSET = DSET is the dataset for which the FWHM is 
                   to be calculated. 
     (-SURF_1): An option for specifying the surface over which
                DSET is defined. (For option's syntax, see 
                'Specifying input surfaces' section below).
     (-MASK)  : An option to specify a node mask so that only
                nodes in the mask are used to obtain estimates.
                See section 'SUMA mask options' for details on
                the masking options.
     Clean output:
     -------------
      The results are written to stdout and the warnings or
      notices to stderr. You can capture the output to a file
      with the output redirection '>'. The output can be 
      further simplified for ease of parsing with -clean.
      -clean: Strip text from output to simplify parsing.
    
     For Datasets With Multiple Sub-Bricks (a time axis):
     ----------------------------------------------------
      For FWHM estimates, one is typically not interested
        in intrinsic spatial structure of the data but in 
        the smoothness of the noise. Usually, the residuals
        from linear regression are used for estimating FWHM.
      A lesser alternate would be to use a detrended version
        of the FMRI time series. 
      N.B.: Do not use catenated time series. Process one
            continuous run at a time.
    
      See note under 'INPUT FILE RECOMMENDATIONS' in 3dFWHMx -help : 
    
      -detrend [q]= Detrend to order 'q'.  If q is not given, 
                    the program picks q=NT/30.
            **N.B.: This is the same detrending as done in 3dDespike;
                    using 2*q+3 basis functions for q > 0.
         or 
      -detpoly p = Detrend with polynomials of order p.
      -detprefix d= Save the detrended file into a dataset with prefix 'd'.
                    Used mostly to figure out what the hell is going on,
                    when funky results transpire.
    
     Options for Local FWHM estimates:
     ---------------------------------
     (-SURF_SPHERE): The spherical version of SURF_1. This is 
                     necessary for Local FWHM estimates as the
                     neighborhoods are rapidly estimated via the
                     spherical surface.
                     (-SURF_SPHERE) is the second surface specified
                     on the command line. The syntax for specifying
                     it is the same as for -SURF_1.
                     If -SURF_1 happens to be a sphere, then there
                     is no need to specify -SURF_SPHERE
     -hood R     = Using this option indicates that you want local
     -nbhd_rad R = as well as global measures of FWHM. Local measurements
                   at node n are obtained using a neighborhood that 
                   consists of nodes within R distance from n 
                   as measured by an approximation of the shortest 
                   distance along the mesh.
                   The choice of R is important. R should be at least
                   twice as large as the FWHM. Otherwise you will be
                   underestimating the Local FWHM at most of the nodes.
                   The more FWHM/R exceeds 0.5, the more you will under-
                   estimate FWHM. Going for an excessive R however is not
                   very advantagious either. Large R is computationaly 
                   expensive and if it is much larger than FWHM estimates,
                   it will lead to a blurring of the local FWHM estimates.
                   Set R to -1 to allow the program
                   to set it automatically.
     -prefix PREFIX = Prefix of output data set. 
     -vox_size D = Specify the nominal voxel size in mm. This helps
                   in the selection of neighborhood size for local smoothness
                   estimation.
    
     -ok_warn
     -examples  = Show command line examples and quit.
     Options for no one to use:
     -slice : Use the contours from planar intersections to estimated
              gradients. This is for testing and development purposes
              only. Leave it alone.
     
     The program is rather slow when estimating Local FWHM. The speed gets
     worse with larger hoods. But I can do little to speed it up without
     making serious shortcuts on the estimates. It is possible however to make
     it faster when estimating the FWHM over multiple sub-bricks. If you find 
     yourself doing this often, let me know. I hestitate to implement the faster 
     method now because it is more complicated to program.
    
     Examples:
    1- Estimating the FWHM of smoothed noise:
         echo Create a simple surface, a sphere and feed it to SUMA.
    
         suma -niml &
         set Niso = `CreateIcosahedron -rad 100 -ld 80 -nums_quiet`; \
               set Niso = $Niso[1]
         CreateIcosahedron -tosphere   -rad 100 -ld 80 \
                           -prefix sphere_iso_$Niso
         DriveSuma  -com show_surf -label sphere_iso_$Niso \
                    -i_fs sphere_iso_${Niso}.asc
    
         echo Create some noise on the sphere.
         1deval -num $Niso -del 1 \
                -expr 'gran(0,1)*10000' > ${Niso}_rand.1D.dset
         DriveSuma  -com surf_cont -label sphere_iso_$Niso \
                    -load_dset ${Niso}_rand.1D.dset\
                    -switch_dset ${Niso}_rand.1D.dset -T_sb -1
    
         echo What is the global FWHM of the noise? -a sanity check-
         set randFWHM = `SurfFWHM -i_fs sphere_iso_${Niso}.asc \
                                  -input ${Niso}_rand.1D.dset` ; \
                                  echo $randFWHM 
    
         echo Now smooth the noise
         set opref_rand = ${Niso}_rand_sm10 && rm -f ${opref_rand}.1D.dset 
         SurfSmooth -spec sphere_iso_$Niso.spec -surf_A sphere_iso_$Niso \
                    -met HEAT_07  \
                    -input ${Niso}_rand.1D.dset -fwhm 10 \
                    -output ${opref_rand}.1D.dset
         DriveSuma  -com surf_cont -label sphere_iso_$Niso \
                    -load_dset ${opref_rand}.1D.dset \
                    -switch_dset ${opref_rand}.1D.dset -T_sb -1
    
         echo Let us find the FWHM both globally and locally
         echo Note:     echo Because the surface where the data are defined is itself
         echo a sphere, we need not specify it spherical version.
         echo If this were not the case, we would need to specify
         echo the spherical surface in the SurfFWHM command. This would be
         echo via an additional -i_fs spherical_version.asc . 
         set fwhmpref = FWHM_${opref_rand} && rm -f ${fwhmpref}.1D.dset
         set gFWHM = `SurfFWHM  -i_fs sphere_iso_${Niso}.asc \
                                -input ${opref_rand}.1D.dset \
                                -hood -1 -prefix ${fwhmpref}` 
         echo The global FWHM is $gFWHM
         echo The local FWHM are sent to SUMA next:     DriveSuma   -com surf_cont -label sphere_iso_$Niso \
                     -load_dset ${fwhmpref}.1D.dset \
                     -switch_dset ${fwhmpref}.1D.dset -T_sb -1
    
         echo Produce a histogram showing the distribution of local FWHM.
         3dhistog ${fwhmpref}.1D.dset > ${fwhmpref}_histog.1D
         set mFWHM = `3dBrickStat -slow -mean ${fwhmpref}.1D.dset`
         1dplot -ylabel 'number of nodes' \
                -x ${fwhmpref}_histog.1D'[0]' -xlabel 'Local FWHM'\
                -plabel "(Mean,Global) =($mFWHM, $gFWHM)" \
                ${fwhmpref}_histog.1D'[1]' & 
    
         echo Notice that these tests are for sanity checks. The smoothing 
         echo operation relies itself on smoothness estimates. You could  
         echo change the example to add a preset number of smoothing   
         echo iterations with a kernel width of your choosing.
    
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
       Mar 22 2018
    
