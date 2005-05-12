#! /bin/bash
if ../../examples/delaunay < two_segments.gts > /dev/null; then
    exit 0
fi
exit 1
