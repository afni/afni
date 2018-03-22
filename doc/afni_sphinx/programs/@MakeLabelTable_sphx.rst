***************
@MakeLabelTable
***************

.. _ahelp_@MakeLabelTable:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Script used to create a label table 
    
    Usage: @MakeLabelTable <-labeltable LABELTABLE> 
                         [-atlas_pointlist ATLAS_POINTLIST>
                         [<-lab_r LAB MIN MAX> <-lab_r LAB MIN MAX> <...>]
                         [<-lab_v LAB KEY> <-lab_v LAB VAL> <...>]
                         [<-lab_file FILE cLAB cVAL>]
                         [<-dset DSET>]
       -labeltable LABELTABLE: Name of output label table
       -atlas_pointlist ATLAS_POINTLIST: Instead of a label table
                                         produce an atlas point list
       -lab_r LAB MIN MAX: Define a label LAB its minimum key MIN
                           and its maximum value MAX. 
                  For example: -lab_r GM 5 7 generates in the labeltable:
                               "5" "GM01"
                               "6" "GM02"
                               "7" "GM03"
       -lab_v LAB KEY: Define a label LAB and its value KEY
                  For example: -lab_v  WM 8 generates in the labeltable:
                               "8" "WM"
       -lab_file FILE cLAB cVAL: Labels and keys are in text file FILE.
                              cLAB is the index of column containing labels
                              vVAL is the index of column containing keys
                              (1st column is indexed at 0)
       -lab_file_delim COL_DELIM: Set column delimiter for -lab_file option
                                  Default is ' ' (space), but you can set
                                  your own. ';' for example. Note that the 
                                  delimiter is passed directly to awk's -F
                     Note: This option must be set BEFORE -lab_file option
                           on the command line.
       -dset DSET: Attach the label table (or atlas point list) to 
                   dataset DSET
       -centers : Compute center of mass location for each ROI
                  requires simple blobbish ROIs to work
       -skip_novoxels : Skip regions without voxels
    
       Note that you cannot use the same key for multiple labels.
       When redundancies occur, the script preserves just one entry.
    
       Example:
          @MakeLabelTable   -lab_r CSF 1 3 -lab_r GM 5 7 -lab_v WM 8  \
                            -labeltable example1
    
    Usage mode 2: Get information about an existing labeltable
        @MakeLabelTable <-labeltable LABELTABLE> 
            <[-lkeys LABEL] | [-rkeys LABEL] | [-all_labels] | [-all_keys]>
    
        -all_labels: Return a listing of the labels
        -all_keys:  Return a listing of all keys
        -lkeys LABEL: Return the keys whose labels match LABEL
        -rkeys LABEL: Return the range (min max) of keys whose 
                      labels match LABEL
        -klabel KEY: Return the label associated with KEY
        -match_label LABEL: Return labels matching LABEL
        -labeltable_of_dset DSET: Dump the labeltable from DSET
        -word_label_match: Use word matching (grep's -w )
                           With this option, 'Out' matches 'Out.l1'
                           but no longer matches 'OutSk'
        -quiet_death: Do not give error messages when failing
       Options in usage 2 are mutually exclusive
    
    Usage mode 3: Transform Label Tables to Atlas Point Lists and exit
        @MakeLabelTable [<-LT_to_atlas_PL LABELTABLE>] 
                      [<-dset_LT_to_atlas_PL DSET POINTLIST]
        -LT_to_atlas_PL LABELTABLE: Transform Label Table LABELTABLE to 
                                    Atlas Point List
        -dset_LT_to_atlas_PL DSET POINTLIST: Get Label Table in
                                    DSET and write it out as an
                                    Atlas Point List to POINTLIST
    
       Example:
          @MakeLabelTable -LT_to_atlas_PL aparc.a2009s+aseg_rank.niml.lt
    
    Usage mode 4: Turn a labeled dataset or an integral valued dset 
                  into an ATLAS
        @MakeLabelTable [<-atlasize_labeled_dset SOME_LABELED_ROI_DSET>] 
                      [<-atlas_file ATLAS_NIML_FILE] 
                      [<-atlas_name ATLAS_NAME>] [-replace]
    
         -atlasize_labeled_dset SOME_LABELED_ROI_DSET: Change a labeled
                                ROI dataset into an atlas
         -atlas_file ATLAS_NIML_FILE: Specify the name of the NIML file
                                where atlas attributes are stored.
                                Default is ./SessionAtlases.niml which
                                is a good choice for single subject atlases
         -atlas_name ATLAS_NAME: Atlas is named based on the prefix, by 
                                 default. You can override that with this
                                 option.
         -atlas_description ATLAS_DESC: Atlas description, which appears
                                 in afni's whereami window. Default is
                                 'My Atlas'
         -replace: If ATLAS_NAME is already in ATLAS_NIML_FILE, the script
                   will fail to proceed unless you instruct it to do so 
                   this option
         -add_atlas_dset ATLAS_DSET: Use if you have an atlas that you want
                                     added to an atlas_file.
    
       Example:
          @MakeLabelTable -atlasize_labeled_dset SOME_LABELED_ROI_DSET
     or you can specify the keys in a file:
          @MakeLabelTable -lab_file FILE cLAB cVAL \
                          -atlas_pointlist apl \
                          -dset target_dset \
                          -atlas_file SessionAtlases.niml
    
                -------------------------
                See also @Atlasize script
                -------------------------
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
                     output. Seach is approximate.
