******************
@SUMA_Make_Spec_FS
******************

.. _@SUMA_Make_Spec_FS:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    @SUMA_Make_Spec_FS - prepare for surface viewing in SUMA
    
        This script goes through the following steps:
          - verify existence of necessary programs 
            (afni, to3d, suma, mris_convert)
          - determine the location of surface and COR files
          - creation of ascii surface files via 'mris_convert'
          - creation of left and right hemisphere SUMA spec files
          - creation of an AFNI dataset from the COR files via 'to3d'
          - creation of AFNI datasets from various .mgz volumes created
            by FreeSurfer. The segmentation volumes with aseg in the 
            name are best viewed in AFNI with the FreeSurfer_Seg_255
            colormap. See bottom of @SUMA_FSvolToBRIK -help for more
            info.
          - [new: Dec 30, 2016] new renumbered data sets output, to
            replace old '*rank*' file data sets. Also new tissue maps
            based on FS functions and labels. Collectively, these are
            the '*REN*' dsets in the output directory.
    
          - all created files are stored in a new SUMA directory
    
      Usage: @SUMA_Make_Spec_FS [options] -sid SUBJECT_ID
    
      examples ('-NIFTI' is really useful-- see below!):
    
        @SUMA_Make_Spec_FS -help
        @SUMA_Make_Spec_FS -NIFTI -sid subject1
        @SUMA_Make_Spec_FS -NIFTI -fspath subject1/surface_stuff -sid subject1
        @SUMA_Make_Spec_FS -NIFTI -sid 3.14159265 -debug 1
    
      options:
    
        -help    : show this help information
    
        -debug LEVEL    : print debug information along the way
              e.g. -debug 1
              the default level is 0, max is 2
    
        -fspath PATH    : path to 'surf' and 'orig' directories
              e.g. -fspath subject1/surface_info
              the default PATH value is './', the current directory
    
              This is generally the location of the 'surf' directory,
              though having PATH end in surf is OK.  The mri/orig
              directory should also be located here.
    
              Note: when this option is provided, all file/path
              messages will be with respect to this directory.
    
        -use_mgz        : use MGZ volumes even if COR volumes are there
    
        -neuro          : use neurological orientation
              e.g. -neuro
              the default is radiological orientation
    
              In the default radiological orientation, the subject's
              right is on the left side of the image.  In the
              neurological orientation, left is really left.
    
            * This is not compatible with -NIFTI.
    
        -nocor: This option is no longer supported because it created
                GIFTI surfaces with coordinates in RAI, rather than LPI
                which is the GIFTI standard. While using RAI surfaces
                within AFNI/SUMA is not problematic, the resultant GIFTI
                surfaces do not port well to other software.
                The replacement option for -nocor is -GNIFTI but the
                surfaces will have negated coordinates along the x and y
                compared to those with -nocor.
                GIFTI surfaces produced with SUMA programs compiled before
                August 1st 2013 will have their X and Y coordinates 
                negated and will no longer line up with the anatomy. 
                Correcting such surfaces can be done with ConvertSurface
                with the following command:
                  ConvertSurface  -i lh.smoothwm.gii -o_gii lh.smoothwm \\
                                  -overwrite -xmat_1D NegXY
                or for an entire SUMA directory:
                  cd SUMA
                  tcsh
                  foreach ss (*.gii)
                     ConvertSurface  -i $ss -o_gii $ss \\
                                     -overwrite -xmat_1D NegXY
                  end
    
        -GNIFTI/-GIFTI/-IFTI: same as -NIFTI
        -NIFTI :Produce files in exchangeable formats. With this option
               :COR volumes are no longer used and output volumes
               :and surfaces are in alignment with the original 
               :volume used to create the surface. All volumes are
                written out NIFTI format, and all surfaces are
                in GIFTI format.
    
                This option is incompatible with -neuro or -use_mgz
    
             ** Note: from 22 Feb 2013 through 20 Mar 2017, use of -NIFTI
                      would distort standard mesh surfaces.  To evaluate
                      effects of this, consider: MapIcosahedron -write_dist.
    
        NOTE for -NIFTI:
            If you really care that the volumes in SUMA/ are in exact
            register with the volume you passed to FreeSurfer, you 
            should be sure that the volume passed to FreeSurfer has 
            an even number of slices in all directions and a voxel
            resolution of 1x1x1, otherwise the resultant volumes in
            SUMA/ might be off by half a voxel or less in directions
            with odd number of slices. The reason has to do (I think)
            with FreeSurfer's resampling and volume centering approach.
            In either case, surfaces and volumes under SUMA/ will be in
            proper register.
            For example, when creating a surface model of the TT_N27 brain
            I padded the TT_N27+tlrc volume before submitting it to 
            FreeSurfer with the following command:
               3dZeropad -L 1 -P 1 -S 1 -prefix anat.nii TT_N27+tlrc.HEAD
            After zeropadding, anat.nii remains in perfect register with
            TT_N27+tlrc by it has an even number of slices in all
            directions: 3dinfo -n4  -d3 -prefix anat.nii
              162  192  152   1    1.0     1.0     1.0     anat.nii
    
        -inflate INF: Create modereately inflated surfaces using
                      SurfSmooth. INF controls the amount of smoothness
                      in the final image. It is the number of iterations
                      in the command such as: 
                 SurfSmooth  -i lh.white.asc    -met NN_geom \\
                        -Niter 200  -o_gii  -surf_out lh.inf_200 \\
                        -match_vol 0.01
                      You can use multiple instances of -inflate to create
                      inflations of various levels.
        -set_space  SPACE: Set the space flag of all volumes to
                SPACE (orig, MNI, TLRC, MNIa). The default is 
                orig space.
                You should only use this option when the volume you
                passed to FreeSurfer was not in 'orig' space.
                Use '3dinfo -space YOUR_DATASET' to find the space 
                of a certain dataset.
    
        -sid SUBJECT_ID : required subject ID for file naming
    
        -ld LD : Create standard mesh surfaces with mesh density
                 linear depth (see MapIcosahedron -help, option -ld)
                 set to LD. You can use multiple -ld options.
                 By default the script will run ld values of 141 and
                 60.
        -ldpref LDpref: Supply what ends up being the -prefix option
                        for MapIcosahedron. By default it is std.LD.
                        You need as many -ldpref as you have -ld
        -no_ld: Do not run MapIcosahedron.
    
      Making use of FreeSurfer's -contrasurfreg output with MapIcosahedron:
      This script will create SUMA versions of lh.rh.sphere.reg and 
      rh.lh.sphere.reg but in this current state, MapIcosahedron does
      not attempt to use them for backward compatibility.
      Should you want to create standard mesh surfaces with node
      index correspondence across the hemispheres you will need to run
      MapIcosahedron manually in the output SUMA/ directory. \n
      For example:
          MapIcosahedron    -spec SUBJ_rh.spec -ld 60 \
                            -dset_map rh.thickness.gii.dset \
                            -dset_map rh.curv.gii.dset \
                            -dset_map rh.sulc.gii.dset \
                            -morph rh.lh.sphere.reg.gii \
                            -prefix std.60.lhreg.
          This command is very similar to the one use to create the
          default output spec file std.60.SUBJ_rh.spec (look at the 
          top of the spec file for a record of the command that created it),
          except for the last two options -morph and -prefix.
          By using -morph rh.lh.sphere.reg.gii the resultant standard-mesh 
          right hemispheres (std.60.lhreg.rh.*.gii) will have node index 
          correspondence with std.60.lh.*.gii surfaces.
          To verify visually the correspondence, run the following:
             count -column 0 36001 > std.60.lh.rh.nodeindex.1D.dset
             suma -noniml -spec std.60.SUBJ_lh.spec &
             suma -noniml -spec std.60.SUBJ_rh.spec &
             suma -noniml -spec std.60.lhreg.SUBJ_rh.spec &
           Then load std.60.lh.rh.nodeindex.1D.dset into each of the three
           SUMA windows. Note how the color pattern (node indices) matches
           between SUBJ_lh and lhreg.SUBJ_rh surfaces, 
           but NOT between SUBJ_lh and SUBJ_rh surfaces.
    
      notes:
    
        0. More help may be found at:
               https://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm
        1. Surface file names should look like 'lh.smoothwm'.
        2. Patches of surfaces need the word patch in their name, in
           order to use the correct option for 'mris_convert'.
        3. Flat surfaces must have .flat in their name.
        4. You can tailor the script to your needs. Just make sure you
           rename it or risk having your modifications overwritten with
           the next SUMA version you install.
    
         R. Reynolds (reynoldr@mail.nih.gov)
         Z. Saad (saadz@mail.nih.gov)
         M. Beauchamp (Michael.S.Beauchamp@uth.tmc.edu)
