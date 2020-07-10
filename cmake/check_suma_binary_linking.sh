#! /usr/bin/env bash
# This script used as part of AFNI's cmake build.
# It provides a basic sanity check for linking in the suma binary
set -e
export suma_binary=$1
SHLIB_EXT=.so
error_message="ERROR: The suma binary should dynamically link  Xm (Motif) and Xt (in that order). The --no-as-needed flag should be passed to the linker/ Please report this"

rm -f suma_binary_check_success.txt
objdump -p $suma_binary | grep libX > suma_x_libraries.txt
libs=$(cat suma_x_libraries.txt)

for lib in $libs;
do
    if echo "$lib"| grep -q libXm$SHLIB_EXT;
    then
        echo  'Xm (Motif) found before Xt in the dynamic header section. Unpleasant linking error in suma avoided'
        echo success > suma_binary_check_success.txt
        exit 0
    elseif echo "$lib"| grep -q libXt$SHLIB_EXT;
        echo $error_message
        exit 1
    fi
done

# Also an error if the end of the file is reached
echo $error_message
exit 1
