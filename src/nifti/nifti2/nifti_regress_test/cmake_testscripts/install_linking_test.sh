#!/bin/sh
set -e

# Get the build tool executable
if [ $# -lt 1 ]
then
    echo Missing build tool, guessing that make is used
    export BUILD_TOOL=make
else
    export BUILD_TOOL=$1
fi

# Set variables for local install
export DESTDIR=installed
export PATH="$PWD/$DESTDIR/usr/local/bin:$PATH"

# Install and execute tools to see if linkage failures have occurred
$BUILD_TOOL install
nifti_tool 1>/dev/null
nifti1_tool 1>/dev/null
nifti_stats 1>/dev/null
echo Success
