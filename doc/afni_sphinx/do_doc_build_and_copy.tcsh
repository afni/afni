#!/bin/tcsh

# Build and push AFNI et al. documentation. Run with sudo

set here = $PWD
set thedate = `date +%Y_%m_%d`
set backup_dir = htmldoc.auto_backup.$thedate

echo "Backup directory called: $backup_dir"

# Make preliminary stuff from helpfiles: will open both AFNI and SUMA
# this way
#@gen_all -phelp -suma -afni

# Build Sphinx.
#make html

# move old documentation to a backupdir
#mv  /mnt/afni/var/www/html/pub/dist/doc/htmldoc     \
#    /mnt/afni/var/www/html/pub/dist/doc/$backup_dir

# new documentation ----> slow to RSYNC!
#rsync -av _build/html/                              \
#    /mnt/afni/var/www/html/pub/dist/doc/htmldoc


# make a tarball for the new documentation?  Is this really so much
# faster than rsync?
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


# If all is well, can delete the backupdir
cat << EOF
    If all went well, which can be verified by checking and clicking 
    around on the website:

    firefox -new-window https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/index.html

    then you should be able to remove the backup directory:

        /mnt/afni/var/www/html/pub/dist/doc/$backup_dir

EOF
