#!/bin/tcsh

# ======================================================================

# variables controlling what gets done; modified by input flags
set gen_all_opts = ( "-phelp" )
set DO_BUILD    = 0
set DO_PUSH     = 0

# ======================================================================

if ( $#argv == 0 ) goto SHOW_HELP

set ac = 1
while ( $ac <= $#argv )
    # terminal options
    if ( ("$argv[$ac]" == "-h" ) || ("$argv[$ac]" == "-help" )) then
        goto SHOW_HELP
    endif

    # required
    if ( "$argv[$ac]" == "-build" ) then
        set DO_BUILD = 1

    else if ( "$argv[$ac]" == "-push" ) then
        set DO_PUSH = 1

    else if ( "$argv[$ac]" == "-gen_all_afni" ) then
        set gen_all_opts = ( $gen_all_opts "-afni" )

    else if ( "$argv[$ac]" == "-gen_all_suma" ) then
        set gen_all_opts = ( $gen_all_opts "-suma" )

    else
        echo "\n\n** ERROR: unexpected option #$ac = '$argv[$ac]'\n\n"
        goto BAD_EXIT
    endif

    @ ac += 1
end
 
# ======================== check input opts =========================

if ( "$DO_BUILD" == "0" ) then
    echo "** ERROR: you at least need to want to '-build' docs!"
    goto BAD_EXIT
endif

# =================== set paths: need AFNI installed ====================

# find where AFNI binaries are
set adir      = ""
which afni >& /dev/null
if ( $status ) then
    echo "** Cannot find 'afni' (?!)."
    goto BAD_EXIT
else
    set aa   = `which afni`
    set adir = $aa:h
    echo "++ AFNI binaries located at: $adir"
endif

setenv PYTHONPATH $adir

set here = $PWD
set thedate = `date +%Y_%m_%d`
set backup_dir = htmldoc.auto_backup.$thedate

echo "Backup directory called: $backup_dir"

# =================== do build ====================

### Make preliminary stuff from helpfiles: will open both AFNI and SUMA
### this way
tcsh @gen_all $gen_all_opts

# ---------------------------------
cd python_help_scripts

# Make list of All Program Helps
python help2sphinx.py -OutFolder ../programs

echo "python path: $PYTHONPATH"

# Make classified/groupings stuff
set fieldfile = list_STYLED_NEW.txt
python convert_list_to_fields_pandas.py        \
    list_AFNI_PROGS_classed.txt                \
    $fieldfile
python convert_fields_to_rst.py                \
    $fieldfile                                 \
    ../educational/classified_progs.rst

# make AFNI startup tips RST
python make_file_of_startup_tips.py            \
    all_startup_tips.txt                       \
    ../educational/startup_tips.rst

# [PT: Apr 25, 2018] make AFNI colorbars RST
python make_file_of_all_afni_cbars.py          \
    ../educational/media/cbars                 \
    ../educational/all_afni_cbars.rst
    
cd ..
# ---------------------------------

### Build Sphinx.
# sudo make html
make html

### move old documentation to a backupdir
#mv  /mnt/afni/var/www/html/pub/dist/doc/htmldoc     \
#    /mnt/afni/var/www/html/pub/dist/doc/$backup_dir

exit

### new documentation ----> slow to RSYNC!
# sudo rsync -av --delete _build/html/ /mnt/afni/pub/dist/doc/htmldoc
rsync -av --delete _build/html/ 	\
      afni.nimh.nih.gov:/fraid/pub/dist/doc/htmldoc

# OLD
#rsync -av --delete _build/html/                              \
#    /mnt/afni/var/www/html/pub/dist/doc/htmldoc



exit



### make a tarball for the new documentation?  Is this really so much
### faster than rsync?
echo "++ Make tarball of directory"
cd _build/
tar -cf html.tar html/
gzip html.tar
echo "++ Copy the tarball to the AFNI server"
mv html.tar.gz /mnt/afni/var/www/html/pub/dist/doc/.
cd /mnt/afni/var/www/html/pub/dist/doc/
echo "++ Unwrap the tarball and put it in the right location (-> a couple min)"
tar -xf html.tar.gz
mv html htmldoc


### If all is well, can delete the backupdir
cat << EOF
    If all went well, which can be verified by checking and clicking 
    around on the website:

    firefox -new-window https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/index.html

    then you should be able to remove the backup directory:

        /mnt/afni/var/www/html/pub/dist/doc/$backup_dir

EOF


SHOW_HELP:
cat << EOF
-------------------------------------------------------------------------

Build and push AFNI et al. documentation. Umcomment/comment as you
desire, and run using: 

To just *build* docs locally:
    do_doc_build_and_copy.tcsh -build

To just build docs locally *and* push:
    do_doc_build_and_copy.tcsh -build -push

A backup doc dir from the server can be removed later.

-------------------------------------------------------------------------
EOF

    goto GOOD_EXIT

BAD_EXIT:
    exit 1

GOOD_EXIT:
    exit 0
