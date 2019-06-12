#!/bin/tcsh

# ver = 1.1 ; date = April 2, 2019
# + [PA Taylor] Change looks of output: nicer without "-boxes" for
#   shapes, and with "-sepscl" to scale each row independently.
# + [PA Taylor] Also, use 1d_tool to calculate enorm (for consistency).
#
# ======================================================================


set ifile  = "$1"
set ofile  = "$2"
set ojpg   = "$3"

if ( "$ojpg" == "" ) then
    echo "** Missing at least one arg: need 3"
    exit 1
endif

# make file name for enorm based on the $ofile
set eee    = "${ofile:e}"
set rrr    = "${ofile:r}"
set oenorm = "${ofile:r}_enorm"
if ( "${eee}" != "" ) then
    set oenorm = "${oenorm}.${eee}"
endif

python do_read_tort_transformations.py                  \
    "$ifile"                                            \
    "$ofile"
    
1d_tool.py                                              \
    -overwrite                                          \
    -infile "${ofile}" -set_nruns 1                     \
    -derivative  -collapse_cols euclidean_norm          \
    -write "${oenorm}"
    
1dplot                                                  \
    -DAFNI_1DPLOT_COLOR_01=green                        \
    -DAFNI_1DPLOT_COLOR_02=green                        \
    -DAFNI_1DPLOT_COLOR_03=green                        \
    -DAFNI_1DPLOT_COLOR_04=cyan                         \
    -DAFNI_1DPLOT_COLOR_05=cyan                         \
    -DAFNI_1DPLOT_COLOR_06=cyan                         \
    -DAFNI_1DPLOT_COLOR_07=black                        \
    -jpg "$ojpg" -overwrite -sep  -sepscl               \
    -xlabel 'volume index'                              \
    -ynames 'Tx (mm)' 'Ty (mm)' 'Tz (mm)'               \
            'Rx (deg)' 'Ry (deg)' 'Rz (deg)' 'enorm' -  \
    -plabel "Motion estimates (solid body params)"      \
    "$ofile" "${oenorm}"
