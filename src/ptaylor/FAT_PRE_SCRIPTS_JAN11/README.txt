++ v2.5 (Jan, 2017):
   o fat_*tcsh                    :renamed to drop the *.tcsh (all files
                                   are executable, anyways)
                                  :renamed "fat_pre_*" -> "fat_proc_*"
-------------------------------------------------------------------------
++ v2.4 (Sept, 2016c):
   o fat_pre_axialize_anat.tcsh   :added command copy placed in text file
                                   in the WORKDIR;
                                  :use 3dcalc instead of 3dcopy at end
                                   to keep history in file.
-------------------------------------------------------------------------
++ v2.3 (Sept, 2016b):
   o fat_pre_axialize_anat.tcsh   :added ability to warp easily to refset
-------------------------------------------------------------------------
++ v2.2.1 (Sept, 2016):
   o fat_proc_decmap.tcsh         :new program;
                                   calculate RGB-coloration maps of DT
                                   directionality, weighted by FA.
-------------------------------------------------------------------------
++ v2.2 (Sept, 2016):
   o fat_pre_axialize_anat.tcsh   :added L-R symmetry parts (pre-symm as 
                                   ON by default);
                                   can specify working dir;
                                   cmass->(0,0,0) as def for output.
-------------------------------------------------------------------------
++ v2.1 (Aug, 2016):
   o fat_pre_convert*.tcsh        :Make volume center of mass (0,0,0) 
                                   coordinate.
   o fat_pre_axialize_anat.tcsh   :Update axializing, new options.
-------------------------------------------------------------------------
++ v2.0 (Aug, 2016):
   o Original online version!
   o These are still in testing phase, as is the online documentation.
-------------------------------------------------------------------------
