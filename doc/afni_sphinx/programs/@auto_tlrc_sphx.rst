**********
@auto_tlrc
**********

.. _@auto_tlrc:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage 1: A script to transform an antomical dataset
             to match a template in TLRC space. 
    
       @auto_tlrc [options] <-base template> <-input anat>
       Mandatory parameters:
          -base template :  Reference anatomical volume
                            Usually this volume is in some standard space like
                            TLRC or MNI space and with afni dataset view of
                            (+tlrc).
                            Preferably, this reference volume should have had
                            the skull removed but that is not mandatory.
                            AFNI's distribution contains several templates.
                            For a longer list, use "whereami -show_templates"
              TT_N27+tlrc --> Single subject, skull stripped volume.
                           This volume is also known as 
                           N27_SurfVol_NoSkull+tlrc elsewhere in 
                           AFNI and SUMA land.
                           (www.loni.ucla.edu, www.bic.mni.mcgill.ca)
                           This template has a full set of FreeSurfer
                           (surfer.nmr.mgh.harvard.edu)
                           surface models that can be used in SUMA. 
                           For details, see Talairach-related link:
                           https://afni.nimh.nih.gov/afni/suma
              TT_icbm452+tlrc --> Average volume of 452 normal brains.
                               Skull Stripped. (www.loni.ucla.edu)
              TT_avg152T1+tlrc --> Average volume of 152 normal brains.
                               Skull Stripped.(www.bic.mni.mcgill.ca)
              TT_EPI+tlrc --> EPI template from spm2, masked as TT_avg152T1
                              TT_avg152 and TT_EPI volume sources are from
                              SPM's distribution. (www.fil.ion.ucl.ac.uk/spm/)
    
              If you do not specify a path for the template, the script
              will attempt to locate the template AFNI's binaries directory.
    
              NOTE: These datasets have been slightly modified from
                    their original size to match the standard TLRC
                    dimensions (Jean Talairach and Pierre Tournoux
                    Co-Planar Stereotaxic Atlas of the Human Brain
                    Thieme Medical Publishers, New York, 1988). 
                    That was done for internal consistency in AFNI.
                    You may use the original form of these
                    volumes if you choose but your TLRC coordinates
                    will not be consistent with AFNI's TLRC database
                    (San Antonio Talairach Daemon database), for example.
          -input anat    :  Original anatomical volume (+orig).
                            The skull is removed by this script
                            unless instructed otherwise (-no_ss).
       Optional parameters:
          -no_ss         :  Do not strip skull of input data set
                            (because skull has already been removed
                            or because template still has the skull)
          NOTE: The -no_ss option is not all that optional.
             Here is a table of when you should and should not use -no_ss
       
                            Template          Template
                            WITH skull        WITHOUT skull
             Dset.
             WITH skull      -no_ss            xxx 
             
             WITHOUT skull   No Cigar          -no_ss
             
             Template means: Your template of choice
             Dset. means: Your anatomical dataset
             -no_ss means: Skull stripping should not be attempted on Dset
             xxx means: Don't put anything, the script will strip Dset
             No Cigar mean: Don't try that combination, it makes no sense.
                   
          -warp_orig_vol: Produce a TLRC version of the input volume, rather
                          than a TLRC version of the skull-stripped input.
                          This option is useful if you want the skull to be 
                          preserved in the +tlrc output. 
                          The default is to produce the skull-stripped version
                          of the input in +tlrc space.
          -dxyz MM          : Cubic voxel size of output DSET in TLRC
                              space. Default is the resolution of the 
                              template. If you do not want your output
                              voxels to be cubic, then use the 
                              -dx, -dy, -dz options below.
          -dx MX            : Size of voxel in the x direction
                              (Right-Left). Default is 1mm.
          -dy MY            : Size of voxel in the y direction
                              (Anterior-Posterior). Default is 1mm.
          -dz MZ            : Size of voxel in the z direction.
                              (Inferior-Superior). Default is 1mm.
          -pad_base  MM  :  Pad the base dset by MM mm in each directions.
                            That is needed to  make sure that datasets
                            requiring wild rotations do not get cropped.
                            Default is MM = 40.
                            If your output dataset is clipped, try increasing
                            MM to 50.000000 or 
                                  60.000000.
                            If that does not help, make sure
                            that the skull-stripped volume has no clipping.
                            If it does, then the skull stripping needs to
                            be corrected. Feel free to report such instances
                            to the script's authors.
          -keep_tmp      :  Keep temporary files.
          -clean         :  Clean all temp files, likely left from -keep_tmp
                            option then exit.
          -xform  XFORM  : Transform to use for warping:
                           Choose from affine_general or shift_rotate_scale
                           Default is affine_general but the script will
                           automatically try to use shift_rotate_scale 
                           if the alignment does not converge.
          -no_avoid_eyes : An option that gets passed to 3dSkullStrip.
                           Use it when parts of the frontal lobes get clipped
                           See 3dSkullStrip -help for more details.
          -ncr           : 3dWarpDrive option -coarserot is now a default.
                           It will cause no harm, only good shall come of it.
                           -ncr is there however, should you choose NOT TO
                           want coarserot used for some reason
          -onepass       : Turns off -twopass option for 3dWarpDrive. This will
                           speed up the registration but it might fail if the 
                           datasets are far apart.          
          -twopass       : Opposite of -onepass, default.
          -maxite NITER  : Maximum number of iterations for 3dWarpDrive.
                           Note that the script will try to increase the 
                           number of iterations if needed. 
                           When the maximum number of iterations is reached
                           without meeting the convergence criteria,
                           the script will double the number of iterations
                           and try again. If the second pass still fails,
                           the script will stop unless the user specifies the
                           -OK_maxite option.
          -OK_maxite     : See -maxite option.
          -inweight      : Apply -weight INPUT (in 3dWarpDrive).
                           By default, 3dWarpDrive uses the BASE dataset to
                           weight the alignment cost.  Use this option to
                           weight via the INPUT dataset, instead.
                           This might be useful for partial coverage cases.
          -rigid_equiv   : Also output a the rigid-body version of the 
                           alignment. This would align the brain with
                           TLRC axis without any distortion. Note that
                           the resultant .Xrigid volume is NOT in TLRC
                           space. Do not use this option if you do not
                           know what to do with it!
                           For more information on how the rigid-body
                           equivalent transformation is obtained, see
                           cat_matvec -help 's output for the -P option. 
          -init_xform XFORM0.1D: Apply affine transform in XFORM0.1D before
                           beginning registration and then include XFORM0.1D
                           in the final xform.
                           To verify that XFORM0.1D does what you think
                           it should be doing, try:
                     3dWarp    -matvec_out2in XFORM0.1D \
                               -prefix pre.anat anat+orig
                           and verify that 'pre.anat+orig' is
                           transformed by XFORM0.1D as you expected it to be.
    
                        XFORM0.1D can be obtained in a variety of ways. 
                        One of which involves extracting it from a transformed
                        volume.
                        For example, say you want to perform an initial
                        rotation that is equivalent to: 
                     3drotate -matvec_order RotMat.1D \
                              -prefix struct.r struct+orig 
                        The equivalent XFORM0.1D is obtained with:
    
                     cat_matvec 'struct.r+orig::ROTATE_MATVEC_000000' -I \
                               > XFORM0.1D  
    
                        See cat_matvec -help for more details on extracting
                        appropriate affine transforms from dataset headers.
    
              Note: You can also use -init_xform AUTO_CENTER to automatically
                    run @Align_Centers if the centers are off by more than 
                    40 mm. 
                    AUTO_CENTER_CM would do the centering based on the
                    center of mass rather than the center of the volume grids.
    
                    You can force centering with -init_xform CENTER
                    or with -init_xform CENTER_CM regardless of the center
                    distance between volumes
    
          -no_pre: Delete temporary dataset created by -init_xform
    
          -out_space spacename: Set output to a particular space
                           Usually, output space is determined by the space
                           of the input template and does not need to set
                           explicitly here
    
          -overwrite: Overwrite existing output. 
                      With this option, 3dSkullstrip will get rerun even
                      if skull stripped volume is found on disk, unless of
                      course you use the -no_ss option.
                      This option has not been fully tested under the myriad
                      combinations possible. So check closely the first
                      time you use it, if use it you must
    
    Note on the subject of transforms:
       The script will output the final transform in a 1D file with the
       extension Xat.1D, say THAT_NAME.1D
       Call this transform Mt and let Xt and Xo be the 4x1 column vectors
       coordinates of the same voxel in standard (t) and original (o)
       space, respectively. The transform is such that Xo = Mt Xt 
       You can use this transform to manually warp a volume in orig
       space to the standard space with:
    
          3dWarp -matvec_out2in THAT_NAME.Xat.1D -prefix PPP SOME_VOL+orig.
          3drefit -view +tlrc PPP+orig
    
       Example:
       @auto_tlrc -base TT_N27+tlrc. -input SubjectHighRes+orig.
        (the output is named SubjectHighRes+TLRC, by default.
         See -suffix for more info.)
    
    Usage 2: A script to transform any dataset by the same TLRC 
             transform obtained with @auto_tlrc in Usage 1 mode
    
             Note: You can now also use adwarp instead.
    
       @auto_tlrc [options] <-apar TLRC_parent> <-input DSET>
       Mandatory parameters:
          -apar TLRC_parent : An anatomical dataset in tlrc space
                              created using Usage 1 of @auto_tlrc
                              From the example for usage 1, TLRC_parent
                              would be: SubjectHighRes+TLRC
          -input DSET       : Dataset (typically EPI time series or
                              statistical datset) to transform to
                              tlrc space per the xform in TLRC_parent
          -dxyz MM          : Cubic voxel size of output DSET in TLRC
                              space Default MM is 1. If you do not
                              want your output voxels to be cubic
                              Then use the -dx, -dy, -dz options below.
          -dx MX            : Size of voxel in the x direction
                              (Right-Left). Default is 1mm.
          -dy MY            : Size of voxel in the y direction
                              (Anterior-Posterior). Default is 1mm.
          -dz MZ            : Size of voxel in the z direction.
                              (Inferior-Superior).Default is 1mm.
       Optional parameters:
          -pad_input  MM    :  Pad the input DSET by MM mm in each direction.
                            That is needed to  make sure that datasets
                            requiring wild rotations do not get cropped.
                            Default is MM = 40.
                            If your output dataset is clipped, try increasing
                            MM to 50.000000 or 
                                  60.000000.
                            If that does not help, report the
                            problem to the script's authors.
          -onewarp          : Create follower data with one interpolation
                              step, instead of two. 
                              This option reduces blurring of the output data.
                              It is not used by default for backward
                              compatibility.
    
       Example:
       @auto_tlrc  -apar SubjectHighRes+tlrc. \
                      -input Subject_EPI+orig. -dxyz 3
        (the output is named Subject_EPI_at+TLRC, by default.
    
    Common Optional parameters:
       -rmode     MODE:  Resampling mode. Choose from:
                         linear, cubic, NN or quintic .
                         Default for 'Usage 1' is Linear.
                   Notice: Prior to 07/2010 -help output incorrecly stated
                         that rmode controlled interpolation in usage 1
                         and that it was 'quintic' by default. 
                         In fact, until this version, rmode did not affect
                         'Usage 1', and interpolation was linear
                         Default for 'Usage 2' is quintic for 3dWarp,
                          followed by Bk for the 3dresample step.
                         See option -onewarp for avoiding two interpolations
       -suffix    SUF :  Name the output dataset by append SUF 
                         to the prefix of the input data for the output.
                         Default for SUF is NONE (see below)
                  NOTE:  You can now set SUF to 'none' or 'NONE' and enable
                         afni's warp on demand features.
                         With NIFTI input volumes -suffix defaults to _at
       -keep_view     :  Do not mark output dataset as +tlrc
       -base_copy COPY_PREFIX: Copy base (template) dataset into COPY_PREFIX.
                               You can use ./ for COPY_PREFIX if you
                               want the copy to have the same name as the
                               template.
       -base_list     : List the full path of the base dataset
       -verb          :  Yakiti yak yak
    
    
    When you're down and troubled and you need a helping hand:
       1- Oh my God! The brain is horribly distorted (by Jason Stein):
          The probable cause is a failure of 3dWarpDrive to converge.
          In that case, rerun the script with the option 
          -xform shift_rotate_scale. That usually takes care of it.
          Update:
          The script now has a mechanism for detecting cases 
          where convergence is not reached and it will automatically
          change -xform to fix the problem. So you should see very 
          few such cases. If you do, check the skull stripping
          step for major errors and if none are found send the
          authors a copy of the command you used, the input and base
          data and they'll look into it.
       2- Parts of the frontal cortex are clipped in the output:
          That is likely caused by aggressive skull stripping.
          When that happens, use the -no_avoid_eyes option.
       3- Other parts of the brain are missing:
          Examine the skull stripped version of the brain
          If the source of the problem is with the stripping,
          then you'll need to run 3dSkullStrip manually and 
          select the proper options for that dataset.
          Once you have a satisfactorily stripped brain, use that
          version as input to @auto_tlrc along with the -no_ss option.
       4- Skull stripped dataset looks OK, but TLRC output is clipped.
          Increase the padding from the default value by little more 
          than the size of the clipping observed. (see -pad_* 
          options above)
       5- The high-res anatomical ends up at a lower resolution: 
          That is because your template is at a lower resolution.
          To preserve (or control) the resolution of your input,
          run @auto_tlrc in usage 2 mode and set the resolution
          of the output with the -d* options.
       6- I want the skulled anatomical, not just the stripped
          anatomical in TLRC space:
          Use @auto_tlrc in usage 2 mode.
       7- What if I want to warp EPI data directly into TLRC space?
          If you have an EPI template in TLRC space you can use it
          as the base in @auto_tlrc, usage 1 mode. You can use whatever
          you want as a template. Just make sure you are warping
          apples to oranges, not apples to bananas for example.
       8- Bad alignment still:
          Check that the center of your input data set is not too
          far off from that of the template. Centers (not origins)
          of the templates we have are close to 0, 0, 0. If your
          input dataset is 100s of mm off center then the alignment
          will fail. 
          The easiest way around this is to add -init_xform AUTO_CENTER
          to your command. If that still fails you can try to manually
          shift all of the input data in your session by an equal amount
          to get the centers closer to zero.
          For example, say the center of your subject's volumes
          is around 100, 100, 100. To shift the centers close to 0, 0, 0 do:
          3drefit -dxorigin -100 -dyorigin -100 -dzorigin -100 Data+orig
          Then use @auto_tlrc on the shifted datasets.
          Take care not to shift datasets from the same session by differing
          amounts as they will no longer be in alignment.
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
    
    Written by Ziad S. Saad (saadz@mail.nih.gov)
                            SSCC/NIMH/NIH/DHHS
