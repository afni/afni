#!/usr/bin/env tcsh

set topdir = afni_boot_packages
set checkfile = afni_boot_sha256sums.txt
set web_data_root = 'https://afni.nimh.nih.gov/pub/dist/edu/data'

# ------------------------------------------------------------
# install downloaded data where we are sitting, but look for
# previous installs

# ------------------------------------------------------------
# create or enter $topdir, under which CD should be located
if ( -d $topdir ) then
   echo "-- entering $topdir"
   cd $topdir
else if ( -d CD ) then
   echo "-- already in valid data root"
else
   echo "-- no $topdir directory yet, will create and work in one"
   \mkdir -p $topdir
   cd $topdir
endif

# make sure there is a CD directory
\mkdir -p CD

# ------------------------------------------------------------
# get checksum file and list to check

# always get the current shasum file
echo "-- getting checksum file, $checkfile"
\rm -f $checkfile
curl -O $web_data_root/$checkfile >& /dev/null
set st = $status
echo ""

if ( $st ) then
   echo "** failed to download $checkfile"
   exit 1
endif

#  awk!
set flist = ( `awk '{print $2}' $checkfile` )
echo "-- will check $#flist files"
echo ""

# ------------------------------------------------------------
# first note what needs to be updated
set newfiles = ( )
foreach file ( $flist )
   # is the current file up to date?
   echo "-- checking $file ..."

   grep $file $checkfile | sha256sum --status -c -
   set sss = $status
   if ( ! $sss ) then
      continue
   endif

   echo "++ need to get file: $file"
   set newfiles = ( $newfiles $file )
end

# are we already done?
if ( $#newfiles == 0 ) then
   echo ""
   echo "-- all files are current, yay!"
   echo ""
   exit 0
endif

# ------------------------------------------------------------
# actually download the files

echo ""
echo "------------------------------------------------------------"
echo "++ need to download $#newfiles files: $newfiles"
echo ""

foreach  file ( $newfiles )
   # remove any old one and try to download
   # note any directory and trailing file name
   set fdir = `dirname $file`
   set dfile = $file:t
   if ( -f $file ) \rm -f $file
   if ( -f $dfile ) \rm -f $dfile

   # and try to download, terminating script on failure
   echo "-- downloading $file ..."
   curl -O $web_data_root/$file >& /dev/null
   if ( $status ) then
       echo "** failed to download $file"
       exit 1
   endif

   # success! just make sure the file is in the correct directory
   if ( $fdir != '.' ) then
      \mv $dfile $fdir
   endif
end


# ------------------------------------------------------------
# then install everything...

echo "------------------------------------------------------------"
echo "updated files:\n   $newfiles\n"
echo "now ponder where to install, under \$HOME\?, make option"
echo ""
