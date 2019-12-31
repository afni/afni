#!/usr/bin/env python

#ver='1.0' ; date='Oct 8, 2019'
# + [PT] 
#
##########################################################################

import sys, os

import afni_base      as ab
import afni_util      as UTIL
import lib_fs         as lf

# =============================================================================

if __name__ == "__main__" : 

    iopts = lf.parse_args_this_prog(sys.argv)

    IS_FS_SAFE = iopts.is_fs_safe()

    if iopts.is_verbose :
        print("{:30s} : {}".format( 'AFNI ver and package',
                                    iopts.afni_ver))
        if iopts.rep_vox_iso or iopts.rep_vox_1mm_max or iopts.rep_vox_05mm_min:
            print("{:30s} : {}".format( 'Voxel dims (mm)',
                                        iopts.vox_dim_str))
        if iopts.rep_mat_even :
            print("{:30s} : {}".format( 'Matrix dims',
                                        iopts.mat_dim_str))
        if iopts.rep_vox_iso :
            print("{:30s} : {}".format( 'Voxel isotropy tolerance (mm)',
                                        iopts.eps_iso))
        if iopts.rep_vox_1mm_max or iopts.rep_vox_05mm_min:
            print("{:30s} : {}".format( 'Voxel size tolerance (mm)',
                                        iopts.eps_size))
            
        print("")

        if iopts.rep_mat_even :
            print("{:30s} : {}".format( 'Are matrix dims even', 
                                        iopts.stat_mat_even ))
        if iopts.rep_vox_iso :
            print("{:30s} : {}".format( 'Are voxels isotropic', 
                                        iopts.stat_vox_iso ))
        if iopts.rep_vox_1mm_max :
            print("{:30s} : {}".format( 'Are voxels 1.0 mm (max)', 
                                        iopts.stat_vox_1mm_max ))
        if iopts.rep_vox_05mm_min :
            print("{:30s} : {}".format( 'Are voxels 0.5 mm (min)', 
                                        iopts.stat_vox_05mm_min ))
        # always output an omnibus result!
        print("{:30s} : {}".format( 'Is dset safe for FS (omnibus)', 
                                    iopts.stat_fs_safe ))

    else:
        print( iopts.stat_fs_safe )

    if iopts.fix_all :
        iopts.run_fix_all()
        
    # and then a bit more output
    if iopts.is_verbose :
        print("")
        print("{:30s} : {}".format( 'Do try to fix all',
                                    iopts.fix_all))
        if iopts.fix_make_changes :
            print("{:30s} : {}".format( 'Do need changes', 
                                        iopts.fix_make_changes ))
        if iopts.fix_out_prefix :
            print("{:30s} : {}".format( 'Output (fixed) dset', 
                                        iopts.fix_out_prefix ))
        if iopts.fix_out_vox_dim_str :
            print("{:30s} : {}".format( 'Output voxel dims (mm)', 
                                        iopts.fix_out_vox_dim_str ))
        if iopts.fix_out_mat_dim_str :
            print("{:30s} : {}".format( 'Output matrix dims', 
                                        iopts.fix_out_mat_dim_str ))

    sys.exit(0)



