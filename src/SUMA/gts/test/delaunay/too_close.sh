#! /bin/bash
if ../../examples/delaunay -o < too_close.gts > /dev/null; then
    exit 0
fi
exit 1
