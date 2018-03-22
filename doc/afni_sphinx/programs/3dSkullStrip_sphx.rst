************
3dSkullStrip
************

.. _ahelp_3dSkullStrip:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: A program to extract the brain from surrounding.
      tissue from MRI T1-weighted images. 
      The simplest command would be:
            3dSkullStrip <-input DSET>
    
      Also consider the script @SSwarper, which combines the use of
      3dSkullStrip and nonlinear warping to an MNI template to produce
      a skull-stripped dataset in MNI space, plus the nonlinear warp
      that can used to transform other datasets from the same subject
      (e.g., EPI) to MNI space. (This script only applies to human brain
      images.)
    
      The fully automated process consists of three steps:
      1- Preprocessing of volume to remove gross spatial image 
      non-uniformity artifacts and reposition the brain in
      a reasonable manner for convenience.
        ** Note that in many cases, using 3dUnifize before **
        ** using 3dSkullStrip will give better results.    **
      2- Expand a spherical surface iteratively until it envelopes
      the brain. This is a modified version of the BET algorithm:
         Fast robust automated brain extraction, 
          by Stephen M. Smith, HBM 2002 v 17:3 pp 143-155
        Modifications include the use of:
         . outer brain surface
         . expansion driven by data inside and outside the surface
         . avoidance of eyes and ventricles
         . a set of operations to avoid the clipping of certain brain
           areas and reduce leakage into the skull in heavily shaded
           data
         . two additional processing stages to ensure convergence and
           reduction of clipped areas.
         . use of 3d edge detection, see Deriche and Monga references
           in 3dedge3 -help.
      3- The creation of various masks and surfaces modeling brain
         and portions of the skull
    
      Common examples of usage:
      -------------------------
      o 3dSkullStrip -input VOL -prefix VOL_PREFIX
         Vanilla mode, should work for most datasets.
      o 3dSkullStrip -input VOL -prefix VOL_PREFIX -push_to_edge
         Adds an agressive push to brain edges. Use this option
         when the chunks of gray matter are not included. This option
         might cause the mask to leak into non-brain areas.
      o 3dSkullStrip -input VOL -surface_coil -prefix VOL_PREFIX -monkey
         Vanilla mode, for use with monkey data.
      o 3dSkullStrip -input VOL -prefix VOL_PREFIX -ld 30
         Use a denser mesh, in the cases where you have lots of 
         csf between gyri. Also helps when some of the brain is clipped
         close to regions of high curvature.
    
      Tips:
      -----
         I ran the program with the default parameters on 200+ datasets.
         The results were quite good in all but a couple of instances, here
         are some tips on fixing trouble spots:
    
         Clipping in frontal areas, close to the eye balls:
            + Try -push_to_edge option first.
              Can also try -no_avoid_eyes option.
         Clipping in general:
            + Try -push_to_edge option first.
              Can also use lower -shrink_fac, start with 0.5 then 0.4
         Problems down below:
            + Piece of cerebellum missing, reduce -shrink_fac_bot_lim 
              from default value.
            + Leakage in lower areas, increase -shrink_fac_bot_lim 
              from default value.
         Some lobules are not included:
            + Use a denser mesh. Start with -ld 30. If that still fails,
            try even higher density (like -ld 50) and increase iterations 
            (say to -niter 750). 
            Expect the program to take much longer in that case.
            + Instead of using denser meshes, you could try blurring the data 
            before skull stripping. Something like -blur_fwhm 2 did
            wonders for some of my data with the default options of 3dSkullStrip
            Blurring is a lot faster than increasing mesh density.
            + Use also a smaller -shrink_fac is you have lots of CSF between
            gyri.
         Massive chunks missing:
            + If brain has very large ventricles and lots of CSF between gyri,
            the ventricles will keep attracting the surface inwards. 
            This often happens with older brains. In such 
            cases, use the -visual option to see what is happening.
            For example, the options below did the trick in various
            instances. 
                -blur_fwhm 2 -use_skull  
            or for more stubborn cases increase csf avoidance with this cocktail
                -blur_fwhm 2 -use_skull -avoid_vent -avoid_vent -init_radius 75 
            + Too much neck in the volume might throw off the initialization
              step. You can fix this by clipping tissue below the brain with 
                     @clip_volume -below ZZZ -input INPUT  
              where ZZZ is a Z coordinate somewhere below the brain.
    
         Large regions outside brain included:
           + Usually because noise level is high. Try @NoisySkullStrip.
    
      Make sure that brain orientation is correct. This means the image in 
      AFNI's axial slice viewer should be close to the brain's axial plane.
      The same goes for the other planes. Otherwise, the program might do a lousy
      job removing the skull.
    
      Eye Candy Mode: 
      ---------------
      You can run 3dSkullStrip and have it send successive iterations
     to SUMA and AFNI. This is very helpful in following the
     progression of the algorithm and determining the source
     of trouble, if any.
      Example:
         afni -niml -yesplugouts &
         suma -niml &
         3dSkullStrip -input Anat+orig -o_ply anat_brain -visual
    
      Help section for the intrepid:
      ------------------------------
      3dSkullStrip  < -input VOL >
                 [< -o_TYPE PREFIX >] [< -prefix VOL_PREFIX >] 
                 [< -spatnorm >] [< -no_spatnorm >] [< -write_spatnorm >]
                 [< -niter N_ITER >] [< -ld LD >] 
                 [< -shrink_fac SF >] [< -var_shrink_fac >] 
                 [< -no_var_shrink_fac >] [< -shrink_fac_bot_lim SFBL >]
                 [< -pushout >] [< -no_pushout >] [< -exp_frac FRAC]
                 [< -touchup >] [< -no_touchup >]
                 [< -fill_hole R >] [< -NN_smooth NN_SM >]
                 [< -smooth_final SM >] [< -avoid_vent >] [< -no_avoid_vent >]
                 [< -use_skull >] [< -no_use_skull >] 
                 [< -avoid_eyes >] [< -no_avoid_eyes >] 
                 [< -use_edge >] [< -no_use_edge >] 
                 [< -push_to_edge >] [<-no_push_to_edge>]
                 [< -perc_int PERC_INT >] 
                 [< -max_inter_iter MII >] [-mask_vol | -orig_vol | -norm_vol]
                 [< -debug DBG >] [< -node_debug NODE_DBG >]
                 [< -demo_pause >]
                 [< -monkey >] [< -marmoset >] [<-rat>]
    
      NOTE: Please report bugs and strange failures
            to saadz@mail.nih.gov
    
      Mandatory parameters:
         -input VOL: Input AFNI (or AFNI readable) volume.
                     
    
      Optional Parameters:
         -monkey: the brain of a monkey.
         -marmoset: the brain of a marmoset. 
                    this one was tested on one dataset
                    and may not work with non default
                    options. Check your results!
         -rat: the brain of a rat.
               By default, no_touchup is used with the rat.
         -surface_coil: Data acquired with a surface coil.
         -o_TYPE PREFIX: prefix of output surface.
            where TYPE specifies the format of the surface
            and PREFIX is, well, the prefix.
            TYPE is one of: fs, 1d (or vec), sf, ply.
            More on that below.
         -skulls: Output surface models of the skull.
         -4Tom:   The output surfaces are named based
                 on PREFIX following -o_TYPE option below.
         -prefix VOL_PREFIX: prefix of output volume.
            If not specified, the prefix is the same
            as the one used with -o_TYPE.
            The output volume is skull stripped version
            of the input volume. In the earlier version
            of the program, a mask volume was written out.
            You can still get that mask volume instead of the
            skull-stripped volume with the option -mask_vol . 
            NOTE: In the default setting, the output volume does not 
                  have values identical to those in the input. 
                  In particular, the range might be larger 
                  and some low-intensity values are set to 0.
                  If you insist on having the same range of values as in
                  the input, then either use option -orig_vol, or run:
             3dcalc -nscale -a VOL+VIEW -b VOL_PREFIX+VIEW \
                    -expr 'a*step(b)' -prefix VOL_SAME_RANGE
                  With the command above, you can preserve the range
                  of values of the input but some low-intensity voxels would
                  still be masked. If you want to preserve them, then use
                  -mask_vol in the 3dSkullStrip command that would produce 
                  VOL_MASK_PREFIX+VIEW. Then run 3dcalc masking with voxels
                  inside the brain surface envelope:
             3dcalc -nscale -a VOL+VIEW -b VOL_MASK_PREFIX+VIEW \
                    -expr 'a*step(b-3.01)' -prefix VOL_SAME_RANGE_KEEP_LOW
         -norm_vol: Output a masked and somewhat intensity normalized and 
                    thresholded version of the input. This is the default,
                    and you can use -orig_vol to override it.
         -orig_vol: Output a masked version of the input AND do not modify
                    the values inside the brain as -norm_vol would.
         -mask_vol: Output a mask volume instead of a skull-stripped
                    volume.
                    The mask volume containes:
                     0: Voxel outside surface
                     1: Voxel just outside the surface. This means the voxel
                        center is outside the surface but inside the 
                        bounding box of a triangle in the mesh. 
                     2: Voxel intersects the surface (a triangle), but center
                        lies outside.
                     3: Voxel contains a surface node.
                     4: Voxel intersects the surface (a triangle), center lies
                        inside surface. 
                     5: Voxel just inside the surface. This means the voxel
                        center is inside the surface and inside the 
                        bounding box of a triangle in the mesh. 
                     6: Voxel inside the surface. 
         -spat_norm: (Default) Perform spatial normalization first.
                     This is a necessary step unless the volume has
                     been 'spatnormed' already.
         -no_spatnorm: Do not perform spatial normalization.
                       Use this option only when the volume 
                       has been run through the 'spatnorm' process
         -spatnorm_dxyz DXYZ: Use DXY for the spatial resolution of the
                              spatially normalized volume. The default 
                              is the lowest of all three dimensions.
                              For human brains, use DXYZ of 1.0, for
                              primate brain, use the default setting.
         -write_spatnorm: Write the 'spatnormed' volume to disk.
         -niter N_ITER: Number of iterations. Default is 250
            For denser meshes, you need more iterations
            N_ITER of 750 works for LD of 50.
         -ld LD: Parameter to control the density of the surface.
                 Default is 20 if -no_use_edge is used,
                 30 with -use_edge. See CreateIcosahedron -help
                 for details on this option.
         -shrink_fac SF: Parameter controlling the brain vs non-brain
                 intensity threshold (tb). Default is 0.6.
                  tb = (Imax - t2) SF + t2 
                 where t2 is the 2 percentile value and Imax is the local
                 maximum, limited to the median intensity value.
                 For more information on tb, t2, etc. read the BET paper
                 mentioned above. Note that in 3dSkullStrip, SF can vary across 
                 iterations and might be automatically clipped in certain areas.
                 SF can vary between 0 and 1.
                 0: Intensities < median inensity are considered non-brain
                 1: Intensities < t2 are considered non-brain
         -var_shrink_fac: Vary the shrink factor with the number of
                 iterations. This reduces the likelihood of a surface
                 getting stuck on large pools of CSF before reaching
                 the outer surface of the brain. (Default)
         -no_var_shrink_fac: Do not use var_shrink_fac.
         -shrink_fac_bot_lim SFBL: Do not allow the varying SF to go
                 below SFBL . Default 0.65, 0.4 when edge detection is used. 
                 This option helps reduce potential for leakage below 
                 the cerebellum.
                 In certain cases where you have severe non-uniformity resulting
                 in low signal towards the bottom of the brain, you will need to
                 reduce this parameter.
         -pushout: Consider values above each node in addition to values
                   below the node when deciding on expansion. (Default)
         -no_pushout: Do not use -pushout.
         -exp_frac FRAC: Speed of expansion (see BET paper). Default is 0.1.
         -touchup: Perform touchup operations at end to include
                   areas not covered by surface expansion. 
                   Use -touchup -touchup for aggressive makeup.
                   (Default is -touchup)
         -no_touchup: Do not use -touchup
         -fill_hole R: Fill small holes that can result from small surface
                       intersections caused by the touchup operation.
                       R is the maximum number of pixels on the side of a hole
                       that can be filled. Big holes are not filled.
                       If you use -touchup, the default R is 10. Otherwise 
                       the default is 0.
                       This is a less than elegant solution to the small
                       intersections which are usually eliminated
                       automatically. 
         -NN_smooth NN_SM: Perform Nearest Neighbor coordinate interpolation
                           every few iterations. Default is 72
         -smooth_final SM: Perform final surface smoothing after all iterations.
                           Default is 20 smoothing iterations.
                           Smoothing is done using Taubin's method, 
                           see SurfSmooth -help for detail.
         -avoid_vent: avoid ventricles. Default.
                      Use this option twice to make the avoidance more
                      agressive. That is at times needed with old brains.
         -no_avoid_vent: Do not use -avoid_vent.
         -init_radius RAD: Use RAD for the initial sphere radius.
                           For the automatic setting, there is an
                           upper limit of 100mm for humans.
                           For older brains with lots of CSF, you
                           might benefit from forcing the radius 
                           to something like 75mm
         -avoid_eyes: avoid eyes. Default
         -no_avoid_eyes: Do not use -avoid_eyes.
         -use_edge: Use edge detection to reduce leakage into meninges and eyes.
                    Default.
         -no_use_edge: Do no use edges.
         -push_to_edge: Perform aggressive push to edge at the end.
                        This option might cause leakage.
         -no_push_to_edge: (Default).
         -use_skull: Use outer skull to limit expansion of surface into
                     the skull due to very strong shading artifacts.
                     This option is buggy at the moment, use it only 
                     if you have leakage into skull.
         -no_use_skull: Do not use -use_skull (Default).
         -send_no_skull: Do not send the skull surface to SUMA if you are
                         using  -talk_suma
         -perc_int PERC_INT: Percentage of segments allowed to intersect
                             surface. Ideally this should be 0 (Default). 
                             However, few surfaces might have small stubborn
                             intersections that produce a few holes.
                             PERC_INT should be a small number, typically
                             between 0 and 0.1. A -1 means do not do
                             any testing for intersection.
         -max_inter_iter N_II: Number of iteration to remove intersection
                               problems. With each iteration, the program
                               automatically increases the amount of smoothing
                               to get rid of intersections. Default is 4
         -blur_fwhm FWHM: Blur dset after spatial normalization.
                          Recommended when you have lots of CSF in brain
                          and when you have protruding gyri (finger like)
                          Recommended value is 2..4. 
         -interactive: Make the program stop at various stages in the 
                       segmentation process for a prompt from the user
                       to continue or skip that stage of processing.
                       This option is best used in conjunction with options
                       -talk_suma and -feed_afni
         -demo_pause: Pause at various step in the process to facilitate
                      interactive demo while 3dSkullStrip is communicating
                      with AFNI and SUMA. See 'Eye Candy' mode below and
                      -talk_suma option. 
         -fac FAC: Multiply input dataset by FAC if range of values is too
                   small.
    
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
    
    
    
         -visual: Equivalent to using -talk_suma -feed_afni -send_kth 5
    
         -debug DBG: debug levels of 0 (default), 1, 2, 3.
            This is no Rick Reynolds debug, which is oft nicer
            than the results, but it will do.
         -node_debug NODE_DBG: Output lots of parameters for node
                             NODE_DBG for each iteration.
         The next 3 options are for specifying surface coordinates
         to keep the program from having to recompute them.
         The options are only useful for saving time during debugging.
         -brain_contour_xyz_file BRAIN_CONTOUR_XYZ.1D
         -brain_hull_xyz_file BRAIN_HULL_XYZ.1D
         -skull_outer_xyz_file SKULL_OUTER_XYZ.1D
         -help: The help you need
    
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
    
