#!/bin/tcsh

set ifile = "$1"
set ofile = "$2"
set ojpg  = "$3"

if ( "$ojpg" == "" ) then
    echo "** Missing at least one arg: need 3"
    exit 1
endif

python do_read_tort_transformations.py \
    "$ifile"                           \
    "$ofile"

1dplot                                                 \
    -DAFNI_1DPLOT_COLOR_01=blue                         \
    -DAFNI_1DPLOT_COLOR_02=blue                       \
    -DAFNI_1DPLOT_COLOR_03=blue                       \
    -DAFNI_1DPLOT_COLOR_04=red                      \
    -DAFNI_1DPLOT_COLOR_05=red                      \
    -DAFNI_1DPLOT_COLOR_06=red                      \
    -DAFNI_1DPLOT_COLOR_07=green                     \
    -jpg "$ojpg" -overwrite -sep     -box                  \
    -xlabel 'volume index'                             \
    -ynames 'Tx (mm)' 'Ty (mm)' 'Tz (mm)'              \
            'Rx (deg)' 'Ry (deg)' 'Rz (deg)' 'enorm' - \
    -plabel "Motion estimates (solid body params)"     \
    "$ofile"
