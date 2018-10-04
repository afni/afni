#!/bin/tcsh

set ifile = "$1"
set ofile = "$2"
set ojpg  = "$3"

python do_read_tort_transformations.py \
    "$ifile"                           \
    "$ofile"

1dplot                                               \
    -jpg "$ojpg" -overwrite -sep                     \
    -ynames 'Tx (mm)' 'Ty (mm)' 'Tz (mm)'            \
            'Rx (deg)' 'Ry (mm)' 'Rz (mm)' 'enorm' - \
    "$ofile"
