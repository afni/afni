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

# Download data.
if [ ! -d $AFNI_TEST_DATA_PATH ]; then
  git clone https://github.com/nih-fmrif/afni_test_data $AFNI_TEST_DATA_PATH
fi
if [ ! -d $AFNI_TEST_DATA_PATH/AFNI_data6 ]; then
  echo "Downloading AFNI data 6 ..."
  curl -fsSL https://afni.nimh.nih.gov/pub/dist/edu/data/AFNI_data6.tgz | tar xzC $AFNI_TEST_DATA_PATH
fi