******************
parse_fs_lt_log.py
******************

.. _parse_fs_lt_log.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    =============================================================================
    parse_fs_lt_log.py      - parse FreeSurfer labeltable log file
    
       Get labeltable indices from a rank log file, such as:
    
            aparc+aseg_rank.niml.lt.log
    
       usage: parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \
                                 -labels CC_Posterior CC_Mid_Posterior
    
    
    ------------------------------------------
    examples:
    
       Example 0: common usage - simply get original indices for aparc+aseg.nii
    
          parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log \
                             -labels FS_white_matter -verb 0 -show_orig
    
       Example 1: get known FreeSurfer labels
    
          parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \
                             -labels FS_white_matter
    
          parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \
                             -labels FS_ventricles
    
       Example 2: get a specific list of list labels
    
          parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log  \
                             -labels CC_Posterior CC_Mid_Posterior
    
       Example 3: get known plus extra labels
    
          parse_fs_lt_log.py -logfile aparc+aseg_rank.niml.lt.log             \
                             -labels FS_white_matter Left-Cerebellum-Exterior \
                             -show_all_orig
    
    
    ------------------------------------------
    terminal options:
    
       -help                : show this help
       -hist                : show the revision history
       -ver                 : show the version number
    
    ------------------------------------------
    process options:
    
       -labels              : specify a list of labels to search for
    
         e.g. -labels Left-Cerebral-White-Matter  Left-Cerebellum-White-Matter  \
                      Right-Cerebral-White-Matter Right-Cerebellum-White-Matter \
                      CC_Posterior CC_Mid_Posterior CC_Central CC_Mid_Anterior  \
                      CC_Anterior Brain-Stem
    
         e.g. -labels FS_white_matter
    
         For convenience, there are 2 label groups:
    
            FS_white_matter (as in the example):
    
                 Left-Cerebral-White-Matter  Left-Cerebellum-White-Matter
                 Right-Cerebral-White-Matter Right-Cerebellum-White-Matter
                 CC_Posterior CC_Mid_Posterior CC_Central CC_Mid_Anterior
                 CC_Anterior Brain-Stem
    
            FS_ventricles
    
                 Left-Lateral-Ventricle Left-Inf-Lat-Vent
                 3rd-Ventricle 4th-Ventricle CSF
                 Right-Lateral-Ventricle Right-Inf-Lat-Vent 5th-Ventricle
    
       -logfile             : specify rank log file
    
          e.g. -logfile aparc+aseg_rank.niml.lt.log
    
    ------------------------------------------
    R Reynolds    May, 2016
    =============================================================================
