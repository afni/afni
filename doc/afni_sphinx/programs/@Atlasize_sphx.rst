.. _ahelp_@Atlasize:

*********
@Atlasize
*********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Script to turn a volumetric dataset into an AFNI atlas. 
    To make an atlas available for 'whereami' queries, AFNI needs both 
    an atlas dataset and an entry for that atlas in an atlas file.
    This script will tag the dataset as an atlas by adding the necessary 
    header information to the dataset and create an entry in the atlas file.
    
    Note:
        For labeling surface-based datasets you should use programs
        MakeColorMap and ConvertDset. For details, see ConvertDest's -labelize
        and MakeColorMap's -usercolutfile and -suma_cmap options.
    
    Usage: @Atlasize <-dset DSET> 
    
       -dset DSET: Make DSET an atlas
       -space SPACE: Mark DSET as being in space SPACE
       -lab_file FILE cLAB cVAL: Labels and keys are in text file FILE.
                              cLAB is the index of column containing labels
                              vVAL is the index of column containing keys
                              (1st column is indexed at 0)
       -lab_file_delim COL_DELIM: Set column delimiter for -lab_file option
                                  Default is ' ' (space), but you can set
                                  your own. ';' for example. Note that the 
                                  delimiter is passed directly to awk's -F
       -atlas_type TP: Set the atlas type where TP is 'S' for subject-based
                    and 'G' for group-based atlases, respectively.
                       A subject-based atlas will remain in the current
                    directory. Its entry is added to the atlas file 
                    SessionAtlases.niml.
                       A group atlas will get copied to your custom atlas
                    directory. If you do not have one, the script will
                    help you create it. The entry for a group atlas is
                    made in CustomAtlases.niml which will reside in your
                    custom atlases directory specified by environment
                    variable AFNI_SUPP_ATLAS_DIR which, if not set already
                    can easily be added with something like:
                      @AfniEnv -set AFNI_SUPP_ATLAS_DIR ~/CustomAtlases
       -atlas_description DESCRP: Something with which to describe atlas
                             Default is 'My Atlas'
       -atlas_name NAME: Something by which to call for the atlas.
                         Default name is based on prefix of DSET.
       -auto_backup: When using -atlas_type G, a copy of dset is made in
                     your custom atlas directory. If the same dset with the
                     same name exists already, this option will back it up
                     and allow an overwrite. You could endup with a lot of
                     backed volumes and niml files, so you might want to
                     to cleanup now and then.
       -centers:    Add center of mass coordinates to atlas
       -skip_novoxels:  Skip regions without any voxels in the dataset
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
    
     Examples:
        Say you have a dataset DSET with ROIs in it and that a text file
        named KEYS.txt contains the assignment of labels to integer keys:
          1   Amygda
          2   Hippo
          5   Cerebellum
          ....
        You can turn DSET into an atlas which gets handled in a special
        manner in AFNI's interactive GUI and in whereami.
    
        There are two classes of atlases:
        Single-subject atlases are ROIs dsets or parcellations like those
        created by freesurfer and handled in @SUMA_Make_Spec_FS, or perhaps
        ones you would create by drawing regions on the anatomy.
        Single-subject datasets and their accompanying SessionAtlases.niml
        file usually reside in that subject's directory.
    
          Case 1, single-subject atlas:
                @Atlasize -space MNI -dset atlas_for_joe.nii \
                          -lab_file keys.txt 1 0 
    
        Launching afni in that directory will now show atlas_for_joe.nii as
        an atlas: Special colors, labels appear next to voxel values, and
        in slice windows if you turn labels on (right click on gray scale, 
        and set Labels menu) Whereami queries will also return results from
        the new atlas.
    
          Case 1.1, dset is already an atlas but it is not in an atlas file
                    and therefore is not visible from whereami.
                 @Atlasize -dset atlas_for_joe.nii
    
        Note: For NIFTI volumes, all changes are made in the header  
        extension, so non-AFNI programs should not be bothered by this.
    
          Case 2, Group-level atlases:
        These atlases are stored in your custom atlas directory (the
        scipt will help you create it), along with the CustomAtlases.niml
        file.
        If you have not set up your custom atlas directory, just run:
    
             @AfniEnv -set AFNI_SUPP_ATLAS_DIR ~/MyCustomAtlases/
    
        Then:
             @Atlasize -space MNI -dset atlas_for_all.nii \
                       -lab_file keys.txt 1 0 -atlas_type G
    
        In ~/MyCustomAtlases/ you will now find  atlas_for_all.nii along 
        along with a modified CustomAtlases.niml file.
    
        Launching afni from any directory will make atlas_for_all.nii 
        available, in addition to the other atlases in afni's bin
        directory.
