#!/bin/tcsh

tcsh -ef do_00_brick_SKoff_cp.tcsh
tcsh -ef do_01_brick_SKon_adwarp.tcsh
tcsh -ef do_02_brick_SKweight_blurinmask.tcsh 
tcsh -ef do_03_brick_Bmask_wbmask.tcsh
tcsh -ef do_04_brick_GCmask_gminfl.tcsh
tcsh -ef do_05_combo_scale.tcsh

echo "++ Done with making new @SSwarper template."

exit 0