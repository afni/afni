*************
@FS_roi_label
*************

.. _ahelp_@FS_roi_label:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    This script is used to get labels associated with 
    FreeSurfer's parcellation and annotation files.
    
    
    For volumetric FreeSurfer Parcellations:
    ----------------------------------------
    Usage: @FS_roi_label <int>
       Return the name of an integer labeled area in FreeSurfer's
       parcellation. Lookup is based on your installed 
       FreeSurferColorLUT.txt 
       Example: 
          @FS_roi_label  2180 
    
     or 
    
    Usage: @FS_roi_label <-lab int>
       Return the name of an integer labeled area in FreeSurfer's
       parcellation
       Example: 
          @FS_roi_label  -lab 2180 
    
     or 
    
    Usage: @FS_roi_label <-rank R> <-rankmap M.1D>
       Return the name of ranked integer labeled area from the output 
       of 3dRank or 3dmerge -1rank on a FreeSurfer parcellation file.
       Example: 
          @FS_roi_label  -rank 198 \
                         -rankmap  SUMA/aparc.a2005s+aseg_rank.rankmap.1D
    
     or 
    
    Usage: @FS_roi_label <-name NAME> 
       Return the entries matching NAME (case insensitive, partial match)
       from FreeSurfer's FreeSurferColorLUT.txt 
       Example: 
          @FS_roi_label  -name cerebra
    
       You can use ALL for NAME to get all entries in FreeSurferColorLUT.txt 
    
     or 
    
    Usage: @FS_roi_label <-name NAME> <-rankmap M.1D>
       Return the entries matching NAME and their rank per M.1D
       Unavailable ranks are flagged with the # sign
       Example: 
          @FS_roi_label  -name cerebra  \
                         -rankmap SUMA/aparc.a2005s+aseg_rank.rankmap.1D
    
     or 
    
    Usage: @FS_roi_label <-name ALL> \
                         <-labeltable LABELTABLE> <-rankmap M.1D>
       Build a label table that can be added to the ranked volume
       so that AFNI can display labels of parcellated volumes.
       Use 3drefit -labeltable LABELTABLE DSET to add the table
       to the rank dataset DSET
       Example: 3drefit -labeltable LABELTABLE SUMA/aparc.a2005s+aseg_rank
    
    For Surface-Based FreeSurfer Annotations  :
    ------------------------------------------
    Usage: @FS_roi_label <-surf_annot_cmap CMAP> <-slab int>
       Return the name of an integer labeled area in FreeSurfer's
       surface-based annotation. 
    
       CMAP is the file output by FSread_annot's -roi_1D option.
       It should sit by default in the SUMA/ directory. 
       The script will search a little for a CMAP under the path from where
       it is launched. However, if the script cannot find a CMAP 
       on its own, you'll need to specify it with -surf_annot_cmap on the 
       command line.
    
       Example:
          @FS_roi_label  -slab 42 \
                         -surf_annot_cmap lh.aparc.a2005s.annot.1D.cmap 
    
    Usage: @FS_roi_label <-surf_annot_cmap CMAP> <-sname SNAME>
       Return the entries matching NAME (case insensitive, partial match)
       from the CMAP file
       Example:
          @FS_roi_label  -sname occi \
                         -surf_annot_cmap lh.aparc.a2005s.annot.1D.cmap 
    
