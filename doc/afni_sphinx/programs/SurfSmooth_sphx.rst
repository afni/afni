.. contents:: 
    :depth: 4 

**********
SurfSmooth
**********

.. code-block:: none

    
    Usage:  SurfSmooth <-SURF_1> <-met method> 
    
       Some methods require additional options detailed below.
       I recommend using the -talk_suma option to watch the 
       progression of the smoothing in real-time in suma.
    
       Method specific options:
          HEAT_07: <-input inData.1D> <-target_fwhm F>   
                This method is used to filter data
                on the surface. It is a significant
                improvement on HEAT_05.
          HEAT_05: <-input inData.1D> <-fwhm F>  
                Formerly known as HEAT, this method is used 
                to filter data on the surface. 
                Parameter choice is tricky however as one
                needs to take into account mesh dimensions,
                desired FWHM, and the data's starting FWHM in 
                order to make an appropriate selection.
                Consider using HEAT_07 if applicable.
                Note that this version will select the number
                of iterations to avoid precision errors.
          LM: [-kpb k] [-lm l m] [-surf_out surfname] [-iw weights]
              This method is used to filter the surface's
              geometry (node coordinates).
          NN_geom: smooth by averaging coordinates of 
                   nearest neighbors.
                   This method causes shrinkage of surface
                   and is meant for test purposes only.
    
       Common options:
          [-Niter N] [-output out.1D] [-h/-help] [-dbg_n node]
          [-add_index] [-ni_text|-ni_binary] [-talk_suma] [-MASK] 
    
    
       Detailed usage:
         (-SURF_1):  An option for specifying the surface to smooth or
                     the domain over which DSET is defined.
                     (For option's syntax, see 'Specifying input surfaces'
                     section below).
         (-MASK)  :  An option to specify a node mask so that only
                     nodes in the mask are used in the smoothing.
                     See section 'SUMA mask options' for details on
                     the masking options.
          -met method: name of smoothing method to use. Choose from:
                     HEAT_07: A significant improvement on HEAT_05.
                             This method is used for filtering 
                             data on the surface and not for smoothing 
                             the surface's geometry per se. 
                             This method makes more appropriate parameter
                             choices that take into account:
                             - Numerical precision issues
                             - Mesh resolution
                             - Starting and Target FWHM
                     HEAT_05: The newer method by Chung et al. [Ref. 3&4 below]
                             Consider using HEAT_07 if applicable.
                     LM: The smoothing method proposed by G. Taubin 2000
                         This method is used for smoothing
                         a surface's geometry. See References below.
                     NN_geom: A simple nearest neighbor coordinate smoothing.
                              This interpolation method causes surface shrinkage
                              that might need to be corrected with the -match_*
                              options below. 
    
       Options for HEAT_07 (see @SurfSmooth.HEAT_07.examples for examples):
          -input inData : file containing data (in 1D or NIML format)
                            Each column in inData is processed separately.
                            The number of rows must equal the number of
                            nodes in the surface. You can select certain
                            columns using the [] notation adopted by AFNI's
                      Note: The program will infer the format of the input
                            file from the extension of inData. 
                            programs.
          -fwhm F: Blur by a Gaussian filter that has a Full Width at Half 
                   Maximum in surface coordinate units (usuallly mm) of F.
                   For Gaussian filters, FWHM, SIGMA (STD-DEV) and RMS
                   FWHM = 2.354820 * SIGMA = 1.359556 * RMS
                   The program first estimates the initial dataset's smoothness
                   and determines the final FWHM (FF) that would result from 
                   the added blurring by the filter of width F.
                   The progression of FWHM is estimated with each iteration, 
                   and the program stops when the dataset's smoothness reaches
                   FF.
       or 
          -target_fwhm TF: Blur so that the final FWHM of the data is TF mm
                           This option avoids blurring already smooth data.
                           FWHM estimates are obtained from all the data
                           to be processed.
          -blurmaster BLURMASTER: Blur so that the final FWHM of dataset
                           BLURMASTER is TF mm, then use the same blurring
                           parameters on inData. In most cases, 
                           you ought to use the -blurmaster option in 
                           conjunction with options -fwhm and target_fwhm.
                           BLURMASTER is preferably the residual timeseries 
                           (errts)  from 3dDeconvolve. 
                           If using the residual is impractical, you can 
                           use the epi time series with detrending option below.
                           The two approaches give similar results for block 
                           design data  but we have not checked for randomised
                           event related designs.
                           After detrending (see option -detrend_master), a 
                           subset of sub-bricks will be selected for estimating 
                           the smoothness.
                           Using all the sub-bricks would slow the program down.
                           The selection is similar to what is done in 
                           3dBlurToFWHM.
                           At most 32 sub-bricks are used and they are selected 
                           to be scattered throughout the timeseries. You can
                           use -bmall to force the use of all sub-bricks.
                     N.B.: Blurmaster must be a time series with a continuous
                           time axis. No catenated time series should be used
                           here.
          -detrend_master [q]: Detrend blurmaster with 2*q+3 basis functions 
                               with q > 0.
                             default is -1 where q = NT/30.
                             This option should be used when BLURMASTER is an
                             epi time series.
                             There is no need for detrending when BLURMASTER 
                             is the residual
                             from a linear regression analysis.
          -no_detrend_master: Do not detrend the master. That would be used 
                              if you are using residuals for master.
          -detpoly_master p: Detrend blurmaster with polynomials of order p.
          -detprefix_master d: Save the detrended blurmaster into a dataset 
                               with prefix 'd'.
          -bmall: Use all sub-bricks in master for FWHM estimation.
          -detrend_in [q]: Detrend input before blurring it, then retrend 
                           it afterwards. Default is no detrending.
                           Detrending mode is similar to detrend_master.
          -detpoly_in p: Detrend input before blurring then retrend.
                         Detrending mode is similar to detpoly_master.
          -detprefix_in d Save the detrended input into a dataset with
                          prefix 'd'.
    
       and optionally, one of the following two parameters:
          -Niter N: Number of iterations (default is -1).
                    You can now set this parameter to -1 and have 
                    the program suggest a value based on the surface's
                    mesh density (average distance between nodes), 
                    the desired and starting FWHM. 
                    Too large or too small a number of iterations can affect 
                    smoothing results. 
          -sigma  S: Bandwidth of smoothing kernel (for a single iteration).
                     S should be small (< 1) but not too small.
                     If the program is taking forever to run, with final
                     numbers of iteration in the upper hundreds, you can
                     increase the value of -sigma somewhat.
          -c_mask or -b_mask or -n_mask (see below for details):
                     Restrict smoothing to nodes in mask.
                     You should not include nodes with no data in 
                     the smoothing. Note that the mask is also applied 
                     to -blurmaster dataset and all estimations of FWHM.
                     For example:
                        If masked nodes have 0 for value in the input 
                        dataset's first (0th) sub-brick, use: 
                        -cmask '-a inData[0] -expr bool(a)'
       Notes:
       1- For those of you who know what they are doing, you can also skip 
       specifying fwhm options and specify Niter and sigma directly.
    
       Options for HEAT_05  (Consider HEAT_07 method):
          -input inData : file containing data (in 1D or NIML format)
                            Each column in inData is processed separately.
                            The number of rows must equal the number of
                            nodes in the surface. You can select certain
                            columns using the [] notation adopted by AFNI's
                      Note: The program will infer the format of the input
                            file from the extension of inData. 
                            programs.
          -fwhm F: Effective Full Width at Half Maximum in surface 
                   coordinate units (usuallly mm) 
                   of an equivalent Gaussian filter had the surface been flat.
                   With curved surfaces, the equation used to estimate FWHM is 
                   an approximation. For Gaussian filters, FWHM, SIGMA 
                   (STD-DEV) and RMS are related by:
                   FWHM = 2.354820 * SIGMA = 1.359556 * RMS
                   Blurring on the surface depends on the geodesic instead 
                   of the Euclidean distances. 
                   Unlike with HEAT_07, no attempt is made here at direct
                   estimation of smoothness.
    
          Optionally, you can add one of the following two parameters:
                         (See Refs #3&4 for more details)
          -Niter N: Number of iterations (default is -1).
                    You can now set this parameter to -1 and have 
                    the program suggest a value based on the -fwhm value.
                    Too large or too small a number of iterations can affect 
                    smoothing results. Acceptable values depend on 
                    the average distance between nodes on the mesh and
                    the desired fwhm. 
          -sigma  S: Bandwidth of smoothing kernel (for a single iteration).
                     S should be small (< 1) and is related to the previous two
                     parameters by: F = sqrt(N) * S * 2.355
    
    
       Options for LM:
          -kpb k: Band pass frequency (default is 0.1).
                  values should be in the range 0 < k < 10
                  -lm and -kpb options are mutually exclusive.
          -lm l m: Lambda and Mu parameters. Sample values are:
                   0.6307 and -.6732
          NOTE: -lm and -kpb options are mutually exclusive.
          -surf_out surfname: Writes the surface with smoothed coordinates
                              to disk. For SureFit and 1D formats, only the
                              coord file is written out.
          NOTE: -surf_out and -output are mutually exclusive.
                Also, the -o_* options have not effect of the format of 
                the surfaces being written out. Surface file format is inferred
                from the filename.
          -iw wgt: Set interpolation weights to wgt. You can choose from:
                   Equal   : Equal weighting, fastest (default), 
                             tends to make edges equal.
                   Fujiwara: Weighting based on inverse edge length.
                             Would be a better preserver of geometry when
                             mesh has irregular edge lengths.
                   Desbrun : Weighting based on edge angles (slooow).
                             Removes tangential displacement during smoothing.
                             Might not be too useful for brain surfaces.
    
       Options for NN_geom:
          -match_size r: Adjust node coordinates of smoothed surface to 
                       approximates the original's size.
                       Node i on the filtered surface is repositioned such 
                       that |c i| = 1/N sum(|cr j|) where
                       c and cr are the centers of the smoothed and original
                       surfaces, respectively.
                       N is the number of nodes that are within r [surface 
                       coordinate units] along the surface (geodesic) from node i.
                       j is one of the nodes neighboring i.
          -match_vol tol: Adjust node coordinates of smoothed surface to 
                       approximates the original's volume.
                       Nodes on the filtered surface are repositioned such
                       that the volume of the filtered surface equals, 
                       within tolerance tol, that of the original surface. 
                       See option -vol in SurfaceMetrics for information about
                       and calculation of the volume of a closed surface.
          -match_area tol: Adjust node coordinates of smoothed surface to 
                       approximates the original's surface.
                       Nodes on the filtered surface are repositioned such
                       that the surface of the filtered surface equals, 
                       within tolerance tol, that of the original surface. 
          -match_sphere rad: Project nodes of smoothed surface to a sphere
                       of radius rad. Projection is carried out along the 
                       direction formed by the surface's center and the node.
          -match_center: Center the smoothed surface to match the original's
                         You can combine -match_center with any of the 
                         other -match_* options above.
          -surf_out surfname: Writes the surface with smoothed coordinates
                              to disk. For SureFit and 1D formats, only the
                              coord file is written out.
    
       Common options:
          -Niter N: Number of smoothing iterations (default is 100)
                    For practical reasons, this number must be a multiple of 2
              NOTE 1: For HEAT method, you can set Niter to -1, in conjunction
                      with -fwhm FWHM option, and the program
                      will pick an acceptable number for you.
              NOTE 2: For LB_FEM method, the number of iterations controls the
                    iteration steps (dt in Ref #1).
                    dt = fwhm*fwhm / (16*Niter*log(2));
                    dt must satisfy conditions that depend on the internodal
                    distance and the spatial derivatives of the signals being 
                    filtered on the surface.
                    As a rule of thumb, if increasing Niter does not alter
                    the results then your choice is fine (smoothing has
                    converged).
                    For an example of the artifact caused by small Niter see:
              https://afni.nimh.nih.gov/sscc/staff/ziad/SUMA/SuSmArt/DSart.html
                    To avoid this problem altogether, it is better that you use 
                    the newer method HEAT which does not suffer from this
                    problem.
          -output OUT: Name of output file. 
                       The default is inData_sm with LB_FEM and HEAT method
                       and NodeList_sm with LM method.
                 NOTE: For data smoothing methods like HEAT, If a format
                       extension, such as .1D.dset or .niml.dset is present 
                       in OUT, then the output will be written in that format.
                       Otherwise, the format is the same as the input's
          -overwrite : A flag to allow overwriting OUT
          -add_index : Output the node index in the first column.
                       This is not done by default.
          -dbg_n node : output debug information for node 'node'.
          -use_neighbors_outside_mask: When using -c_mask or -b_mask or -n_mask
                                       options, allow value from a node nj 
                                       neighboring node n to contribute to the 
                                       value at n even if nj is not in the mask.
                                       The default is to ignore all nodes not in
                                       the mask.
    
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
       -npb PORT_OFFSET_BLOC: Simliar to -np, except it is easier to use.
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
      
    
    -----------------------------------------------------------------------------
    
       Sample commands lines for using SurfSmooth:
             The surface used in this example had no spec file, so 
             a quick.spec was created using:
             quickspec -tn 1D NodeList.1D FaceSetList.1D 
    
       Sample commands lines for data smoothing:
     
          For HEAT_07 method, see multiple examples with data in script
                      @SurfSmooth.HEAT_07.examples
    
          SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met HEAT_05   \
                      -input in.1D -fwhm 8 -add_index         \
                      -output in_smh8.1D.dset 
    
             You can colorize the input and output data using ScaleToMap:
             ScaleToMap  -input in.1D 0 1 -cmap BGYR19       \
                         -clp MIN MAX > in.1D.col            \
             ScaleToMap  -input in_sm8.1D 0 1 -cmap BGYR19   \
                         -clp MIN MAX > in_sm8.1D.col        \
    
             For help on using ScaleToMap see ScaleToMap -help
             Note that the MIN MAX represent the minimum and maximum
             values in in.1D. You should keep them constant in both 
             commands in order to be able to compare the resultant colorfiles.
             You can import the .col files with the 'c' command in SUMA.
    
             You can send the data to SUMA with each iteration.
             To do so, start SUMA with these options:
             suma -spec quick.spec -niml &
             and add these options to SurfSmooth's command line above:
             -talk_suma -refresh_rate 5
    
       Sample commands lines for surface smoothing:
          SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met LM    \
                      -output NodeList_sm100.1D -Niter 100 -kpb 0.1   
             This command smoothes the surface's geometry. The smoothed
             node coordinates are written out to NodeList_sm100.1D. 
          A similar operation on a surface with a new surface for output:
          SurfSmooth -i rough_surf.gii -surf_out smooth_surf.gii \
                     -met LM -Niter 100 -kpb 0.1
    
       Sample command for considerable surface smoothing and inflation
       back to original volume:
           SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met NN_geom \
                       -output NodeList_inflated_mvol.1D -Niter 1500 \
                       -match_vol 0.01
       Sample command for considerable surface smoothing and inflation
       back to original area:
           SurfSmooth  -spec quick.spec -surf_A NodeList.1D -met NN_geom \
                       -output NodeList_inflated_marea.1D -Niter 1500 \
                       -match_area 0.01
    
       References: 
          (1) M.K. Chung et al.   Deformation-based surface morphometry
                                  applied to gray matter deformation. 
                                  Neuroimage 18 (2003) 198-213
              M.K. Chung   Statistical morphometry in computational
                           neuroanatomy. Ph.D. thesis, McGill Univ.,
                           Montreal, Canada
          (2) G. Taubin.       Mesh Signal Processing. 
                               Eurographics 2000.
          (3) M.K. Chung et al.  Cortical thickness analysis in autism 
                                 via heat kernel smoothing. NeuroImage, 
                                 submitted.(2005) 
                 http://www.stat.wisc.edu/~mchung/papers/ni_heatkernel.pdf
          (4) M.K. Chung,  Heat kernel smoothing and its application to 
                           cortical manifolds. Technical Report 1090. 
                           Department of Statististics, U.W.Madison
                 http://www.stat.wisc.edu/~mchung/papers/heatkernel_tech.pdf
       See Also:   
           ScaleToMap to colorize the output, however it is better
           to load surface datasets directly into SUMA and colorize
           them interactively.
    
    
    Compile Date:
       Nov  9 2017
    
