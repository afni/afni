#! /bin/bash
./cartesian 15 15 0 0 | ../../examples/coarsen -n 100 -v 2> /tmp/tutu > /dev/null
if awk 'BEGIN {area = 0.} { if ($4 == "area:") { if (area) { if (area == $5) exit 1; else exit 0;} else area = $5;}}' < /tmp/tutu; then
    exit 1
fi
exit 0

