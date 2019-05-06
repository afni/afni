#!/bin/bash
# Exit on any errors.
set -e


# Get afni root directory
POSSIBLE_SCRIPT_DIR="${BASH_SOURCE%/*}" || exit
if [ $POSSIBLE_SCRIPT_DIR == run_tests.sh ];then
    POSSIBLE_SCRIPT_DIR=$PWD
fi
echo Test script directory used: $POSSIBLE_SCRIPT_DIR


##### Add functionality to implement readlink -f in a more general way as described here:
# https://stackoverflow.com/questions/1055671/how-can-i-get-the-behavior-of-gnus-readlink-f-on-a-mac
realpath() {
    canonicalize_path "$(resolve_symlinks "$1")"
}

resolve_symlinks() {
    local dir_context path
    path=$(readlink -- "$1")
    if [ $? -eq 0 ]; then
        dir_context=$(dirname -- "$1")
        resolve_symlinks "$(_prepend_path_if_relative "$dir_context" "$path")"
    else
        printf '%s\n' "$1"
    fi
}

_prepend_path_if_relative() {
    case "$2" in
        /* ) printf '%s\n' "$2" ;;
         * ) printf '%s\n' "$1/$2" ;;
    esac
}

canonicalize_path() {
    if [ -d "$1" ]; then
        _canonicalize_dir_path "$1"
    else
        _canonicalize_file_path "$1"
    fi
}

_canonicalize_dir_path() {
    (cd "$1" 2>/dev/null && pwd -P)
}

_canonicalize_file_path() {
    local dir file
    dir=$(dirname -- "$1")
    file=$(basename -- "$1")
    (cd "$dir" 2>/dev/null && printf '%s/%s\n' "$(pwd -P)" "$file")
}
####################


# Run tests.
AFNI_ROOT=$(realpath "$POSSIBLE_SCRIPT_DIR/../../..")

pytest $AFNI_ROOT/tests/scripts # --workers 4

cd $AFNI_ROOT/src
gcov *.c
codecov
