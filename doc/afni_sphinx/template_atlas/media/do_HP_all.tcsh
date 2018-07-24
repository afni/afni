#!/bin/tcsh

tcsh -ef do_HP_00_brick_SKoff_*.tcsh
tcsh -ef do_HP_01_brick_SKon_*.tcsh
tcsh -ef do_HP_02_brick_SKweight_*.tcsh 
tcsh -ef do_HP_03_brick_Bmask_*.tcsh
tcsh -ef do_HP_04_brick_GCmask_*.tcsh
tcsh -ef do_HP_05_combo_scale.tcsh

echo "++ Done with making new 'HP' @SSwarper template."

exit 0
