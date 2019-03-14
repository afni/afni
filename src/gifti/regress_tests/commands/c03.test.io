
set ddir = ~/data/sample/image.formats/gifti/caret/caret_gifti_files/RIGHT_HEM/gzip_base64

if ( ! -d $ddir ) then
    echo "** c03.test.io: missing Caret data directory"
    exit
endif

cd $ddir
gifti.test.io -gt_compare Human.*.gii
cd -

# and again with SUMA files
set ddir = ~/data/sample/image.formats/gifti/suma/SUMA_GIFTI

if ( ! -d $ddir ) then
    echo "** c03.test.io: missing SUMA data directory"
    exit
endif

cd $ddir
gifti.test.io -gt_compare afni.*.gii
cd -

# and again with SUMA files
