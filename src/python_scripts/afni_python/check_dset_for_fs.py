#!/usr/bin/env python

#ver='1.0' ; date='Oct 22, 2019'
# + [PT] start
#
#ver='1.2' ; date='Oct 23, 2019'
# + [PT] helpful helpfile
#      - also updating way prog works: can have finer-grained criteria
#        calling
#
#ver='1.3' ; date='Dec 26, 2019'
# + [PT] fix "-is_mat_even" test
#      - thanks, S. Torrisi, for pointing this out!
#
#ver='1.4' ; date='Dec 27, 2019'
# + [PT] new options for '-fix_all' functionality
#      - can output new/fixed dsets.  Bonne idee, D Glen!
#
ver='2.0' ; date='Feb 23, 2020'
# + [PT] Major changes, made in consultation with RCR, based on looking
#        at lots of resamplings/dsets output by FS:
#      + change criteria: size criteria given new defaults
#        - still check min/max, but now have vals that can change (e.g., if 
#          FS changes things
#        - new default size range is [1.0, 1.0], instead of [0.5, 1.0]
#          ... so, yes, this is basically just the isotropy criterion
#      + report differently, a bit; state what min/max are (b/c might change
#        over time)
#      + new default 'fix all' output size:  1mm iso (no longer smallest
#        vox dim in allowed range)
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
        print("{:35s} : {}".format( 'AFNI ver and package',
                                    iopts.afni_ver))
        if iopts.rep_vox_iso or iopts.rep_vox_sub_max or iopts.rep_vox_supra_min :
            print("{:35s} : {}".format( 'Input dset voxel dims (mm)',
                                        iopts.vox_dim_str))
        if iopts.rep_mat_even :
            print("{:35s} : {}".format( 'Input dset matrix dims',
                                        iopts.mat_dim_str))

        print("")

        if iopts.rep_vox_sub_max or iopts.rep_vox_supra_min :
            print("{:35s} : {}".format( 'Voxel dim max (mm)',
                                        lf.ddefs['DEF_vox_max_size']))
            print("{:35s} : {}".format( 'Voxel dim min (mm)',
                                        lf.ddefs['DEF_vox_min_size']))
            print("{:35s} : {}".format( 'Voxel size tolerance (mm)',
                                        iopts.eps_size))
        if iopts.rep_vox_iso :
            print("{:35s} : {}".format( 'Voxel isotropy tolerance (mm)',
                                        iopts.eps_iso))

        print("")

        if iopts.rep_mat_even :
            print("{:35s} : {}".format( 'Are matrix dims even', 
                                        iopts.stat_mat_even ))
        if iopts.rep_vox_iso :
            print("{:35s} : {}".format( 'Are voxel dims isotropic', 
                                        iopts.stat_vox_iso ))
        if iopts.rep_vox_sub_max :
            print("{:35s} : {}".format( 'Are voxel dims below max', 
                                        iopts.stat_vox_sub_max ))
        if iopts.rep_vox_supra_min :
            print("{:35s} : {}".format( 'Are voxel dims above min', 
                                        iopts.stat_vox_supra_min ))
        # always output an omnibus result!
        print("{:35s} : {}".format( 'FINAL--is input dset safe for FS', 
                                    iopts.stat_fs_safe ))

    else:
        print( iopts.stat_fs_safe )

    if iopts.fix_all :
        iopts.run_fix_all()
        
    # and then a bit more output
    if iopts.is_verbose :
        print("")
        print("{:35s} : {}".format( 'Do try to fix all',
                                    int(iopts.fix_all)))
        if iopts.fix_make_changes :
            print("{:35s} : {}".format( 'Do need changes', 
                                        int(iopts.fix_make_changes) ))
        if iopts.fix_out_prefix :
            print("{:35s} : {}".format( 'Output (fixed) dset', 
                                        iopts.fix_out_prefix ))
        if iopts.fix_out_vox_dim_str :
            print("{:35s} : {}".format( 'Output voxel dims (mm)', 
                                        iopts.fix_out_vox_dim_str ))
        if iopts.fix_out_mat_dim_str :
            print("{:35s} : {}".format( 'Output matrix dims', 
                                        iopts.fix_out_mat_dim_str ))

    sys.exit(0)



