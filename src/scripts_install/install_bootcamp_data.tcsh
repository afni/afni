#!/usr/bin/env tcsh

@global_parse `basename $0` "$*" ; if ($status) exit 0

# main variables
set topdir = afni_boot_packages
set hash_prog = sha256sum
set checkfile = afni_boot_$hash_prog.txt
set web_data_root = 'https://afni.nimh.nih.gov/pub/dist/edu/data'
set progname = `basename $0`

# user controlled variables
set do_download = yes
set do_install = no
set install_root = "$HOME"

# ===========================================================================
# help - can only reach via goto SHOW_HELP
goto SKIP_HELP
SHOW_HELP:
    cat << EOF

$progname    - install AFNI bootcamp data packages   ~1~

   Make installing the AFNI bootcamp data slightly more simple.
   The files will be based on the contents of the relevant checksum file.

   1. create a new data directory to hold tgz files ($topdir)
      if it does not exist

   2. download a list of data files needed for the bootcamp
      into $topdir
      - use "-do_download no" to skip the download step

   3. for each needed package that does not exist or is not current :
          download that (usually tgz) file into $topdir

   4. if requested, extract the tgz packages under the -install_root directory
      - use "-do_install yes" to extract the tgz packages
      - use -install_root to specify where to install the packages
        default: -install_root \$HOME

   ----------------------------------------------------------------------
   examples:                                                         ~1~

      1. do everything (download packages and install them)
         ('-do_download yes' is set by default)

            install_bootcamp_data.tcsh -do_install yes

      2. do everything, but install them under ~/my_data

            install_bootcamp_data.tcsh -do_install yes -install_root ~/my_data

      3. only download the packages (anything that is not already current)

            install_bootcamp_data.tcsh -do_download yes

      4. only install pre-downloaded packages

            install_bootcamp_data.tcsh -do_download no -do_install yes

   ----------------------------------------------------------------------
   terminal options:                                                 ~1~

      -help                : show this help
      -hist                : show the program modification history
      -ver                 : show the program version

   main options:                                                     ~1~

      -do_download yes/no  : specify whether to download new packages

            default: -do_download yes
            example: -do_download no

         Use this option to control whether package (tgz) files are
         downloaded from the AFNI site.

      -do_install  yes/no  : specify whether to install new packages

            default: -do_install no
            example: -do_install yes

         Use this option to control whether package (tgz) files are
         extracted into the install_root directory.

         See -install_root.

      -install_root  IDIR  : specify where to install the packages

            default: -install_root \$HOME
            example: -install_root ~/my_afni_data

         Use this option to control which directory packages are extracted
         into.  By default, it is the users \$HOME directory.

   ----------------------------------------------------------------------
   R Reynolds, Jan, 2026

EOF
exit
SKIP_HELP:

# ===========================================================================
# hist - can only reach via goto SHOW_HIST
goto SKIP_HIST
SHOW_HIST:
  cat << EOF

  history:
     0.0   8 Jan 2026 - initial work

EOF
exit
SKIP_HIST:

# ----- corresponding version
set VERSION = "0.0 January 8, 2026"


# ===========================================================================
# begin main program

# ------------------------------------------------------------
# process options (controllable variables are initialized at the top)

# if no args, just show the help
if ( $#argv < 1 ) then
   goto SHOW_HELP
endif

set ac = 1
while ( $ac <= $#argv )
   set arg = "$argv[$ac]"

   # check terminal options, first
   if ( "$arg" == "-help" ) then
      goto SHOW_HELP
   else if ( "$arg" == "-hist" ) then
      goto SHOW_HIST
   else if ( "$arg" == "-ver" ) then
      echo $VERSION
      exit 0

   # main options

   # without this, nothing happens
   else if ( "$arg" == "-do_download" ) then
      if ( $ac > $#argv ) then
         echo "** missing parameter for option '-do_download'"
         exit 1
      endif

      @ ac += 1
      set do_download = "$argv[$ac]"
      if ( "$do_download" != yes && "$do_download" != no ) then
         echo "** -do_download requires yes/no, have $argv[$ac]"
         exit 1
      endif

   else if ( "$arg" == "-do_install" ) then
      if ( $ac > $#argv ) then
         echo "** missing parameter for option '-do_install'"
         exit 1
      endif

      @ ac += 1
      set do_install = "$argv[$ac]"
      if ( "$do_install" != yes && "$do_install" != no ) then
         echo "** -do_install requires yes/no, have $argv[$ac]"
         exit 1
      endif

   else if ( "$arg" == "-install_root" ) then
      if ( $ac > $#argv ) then
         echo "** missing parameter for option '-install_root'"
         exit 1
      endif

      @ ac += 1
      set install_root = "$argv[$ac]"
      if ( ! -d "$install_root" ) then
         echo "** -install_root does not seem to be a directory: $argv[$ac]"
         exit 1
      endif

   else
      echo "** unknown option $ac : '$argv[$ac]'"
      exit 1

   endif

   @ ac += 1
end

# ------------------------------------------------------------
# install downloaded data where we are sitting, but look for
# previous installs

if ( $do_download != yes ) then
   echo "-- skipping package download at user request"
   echo ""
   goto DO_INSTALL
endif

# ------------------------------------------------------------
# make sure we have the hash program, or we will need an option
# for a different one
which $hash_prog >& /dev/null
if ( $status ) then
   echo "** missing executable $hash_prog, cannot proceed"
   exit 1
endif

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
if ( $status ) then
   echo "** failed to download $checkfile"
   exit 1
endif
if ( ! -f $checkfile ) then
   echo "** missing download file $checkfile"
   exit 1
endif

echo ""

#  get list of files to run the checksum on
set flist = ( `awk '{print $2}' $checkfile` )
echo "-- will check $#flist files"
echo ""

# ------------------------------------------------------------
# first note what needs to be updated
set newfiles = ( )
foreach file ( $flist )
   # is the current file up to date?
   echo "   checking $file ..."

   # only check if file exists, just for visual cleanliness
   if ( -f $file ) then
      \grep $file $checkfile | $hash_prog --status -c -
      if ( ! $status ) then
         continue
      endif
   endif

   echo "++ need to get file: $file"
   set newfiles = ( $newfiles $file )
end

# are we already done?
if ( $#newfiles == 0 ) then
   echo ""
   echo "-- all files are current, yay!"
   echo ""

   # installation might still be requested
   goto DO_INSTALL
endif

# ------------------------------------------------------------
# actually download the files

echo ""
echo "------------------------------------------------------------"
echo "++ need to download $#newfiles package files:"
foreach file ( $newfiles )
   echo "   $file"
end
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
# actually install everything, if requested...
DO_INSTALL:

if ( $do_install != yes ) then
   echo "------------------------------------------------------------"
   echo "-- no actual install of downloaded tgz packages requested"
   echo ""
   exit 0
endif

echo "-- installing all data packages under:"
echo "   $install_root"
echo ""

# just verify that we have the $checkfile
if ( ! -f $checkfile ) then
   echo "** missing $checkfile, consider '-do_download yes'"
   echo ""
   exit 1
endif

# note the packages - we will install only tgz packages
set packlist = ( `awk '/tgz/ {print $2}' $checkfile` )

# note the directory root for all tgz packages
set packagedir = `pwd`

# then go to the install_root and get to work
cd "$install_root"

# first remove existing directories
set remove_dirs = ()
foreach tgzfile ( $packlist )
   set tfile = $tgzfile:t
   set tdir = $tfile:r
   if ( -d $tdir ) then
      set remove_dirs = ( $remove_dirs $tdir )
   endif
end

# delete the old results
if ( $#remove_dirs > 0 ) then
   echo "-- removing old directories..."
   foreach rdir ( $remove_dirs )
      echo "   removing $rdir ..."
      \rm -fr $rdir
   end
   echo ""
else
   echo "-- no old directories to remove"
   echo ""
endif

# install the packages
echo "++ installing $#packlist packages..."
foreach tgzfile ( $packlist )
   echo "   extracting $tgzfile ..."
   tar xfz $packagedir/$tgzfile
end
echo ""

