#!/usr/bin/perl -w
#3dPARtoANALYZE  somefile.par

# This file is a modified version of this file
# http://www.cdfi.uab.edu/cdfi/Documents/6740FF06-1993-4EAE-8CF7-18CBCA3D78B6.html

use strict;
use Getopt::Std;
use Cwd;

my %Options = ();
my $ok = getopts("hsnvg24o:a", \%Options);

sub usage(){

  print "3dPAR2ANFI\n";
  print "Command line Options:\n";
  print "-h     This help message.\n";
  print "-v     Be verbose in operation.\n";
  print "-s     Skip the outliers test when converting 4D files\n";
  print "       The default is to perform the outliers test.\n";
  print "-n     Output NIfTI files instead of HEAD/BRIK.\n";
  print "       The default is create HEAD/BRIK files.\n";
  print "-a     Output ANALYZE files instead of HEAD/BRIK.\n";
  print "-o     The name of the directory where the created files should be\n";
  print "       placed.  If this directory does not exist the program exits\n";
  print "       without performing any conversion.\n";
  print "       The default is to place created files in the same directory\n";
  print "       as the PAR files.\n";
  print "-g     Gzip the files created.\n";
  print "       The default is not to gzip the files.\n";
  print "-2     2-Byte-swap the files created.\n";
  print "       The default is not to 2 byte-swap.\n";
  print "-4     4-Byte-swap the files created.\n";
  print "       The default is not to 4 byte-swap.\n\n";

  print "Sample invocations:\n";
  print "3dPAR2AFNI subject1.PAR\n";
  print "       Converts the subject1.PAR file to subject1+orig.{HEAD,BRIK}\n";
  print "3dPAR2AFNI -s subject1.PAR\n";
  print "       Same as above but skip the outlier test\n";
  print "3dPAR2AFNI -n subject1.PAR\n";
  print "       Converts the file subject1.PAR file to subject1.nii\n";
  print "3dPAR2AFNI -n -s subject1.PAR\n";
  print "       Same as above but skip the outlier test\n";
  print "3dPAR2AFNI -n -s -o ~/tmp subject1.PAR\n";
  print "       Same as above but skip the outlier test and place the\n";
  print "       created NIfTI files in ~/tmp\n";
  print "3dPAR2AFNI -n -s -o ~/tmp *.PAR\n";
  print "       Converts all the PAR/REC files in the current directory to\n";
  print "       NIfTI files, skip the outlier test and place the created\n";
  print "       NIfTI files in ~/tmp\n";

  exit;
}

sub convertPar($) {
  $,=" ";
  $\="\n";
  my $verbose = $Options{"v"};
  my $skipOutliers = $Options{"s"};
  my $nifti = $Options{"n"};
  my $analyze = $Options{"a"};
  my $gzip =  $Options{"g"};
  my $twoSwap =  $Options{"2"};
  my $fourSwap =  $Options{"4"};
  my $outputDirectory =  $Options{"o"};
  my $outliers="";
  my $parFile = shift;
  my ($rootname, $extension, %fov, %angulation, %origin, $slices, $tr, $volumes, $bitdepth, %reconres, $prefixargs);
  my ($timeargs, $totalslices, $anterior, $posterior, $foot, $head, $right, $left, $FOVargs, $filespec, $session);

  if ($outputDirectory){
    if ( ! -d $outputDirectory ) {
      print "$outputDirectory does not exist. Exiting!\n";
      exit;
    }
    $outputDirectory = Cwd::abs_path($outputDirectory);
  }

  if ($verbose) {
    print "Skipping outliers test\nOutput file will be NIfTI format\n";
  }

  if ($parFile =~ /([A-Za-z_0-9]+)\.([Pp][Aa][Rr])$/) {
    $rootname=$1;
    $extension=$2;
  }

  # parse PAR file.
  my $lineCount=0;

  open(PFH, $parFile) || die "Unable to open $parFile for reading: $!.\n";
  my @buffer = ();

  while (<PFH>) {
    chomp;                      # strip record separator

    push (@buffer, $_);
    if ($#buffer > 2){
      shift @buffer;
    }

    if ($lineCount <= 91 ) {
      # file version
      if (/# CLINICAL TRYOUT             Research image export tool     V([0-9])/) {
        my $fileVersion = $1;
        if ($fileVersion != 4 ) {
          print "This program can only handle Version 4 PAR/REC files. Exiting.";
          exit;
        }
      }

      # field of view
      if (/FOV \((..),(..),(..)\) \[.*\].*:\ *([0-9]+\.[0-9]+)\ *([0-9]+\.[0-9]+)\ *([0-9]+\.[0-9]+)/) {
        $fov{$1}=$4;
        $fov{$2}=$5;
        $fov{$3}=$6;
      }
      # matrix size

      # angulation
      if (/Angulation midslice\((..),(..),(..)\)\[.*\].*:\ *(-?[0-9]+\.[0-9]+)\ *(-?[0-9]+\.[0-9]+)\ *(-?[0-9]+\.[0-9]+)/) {
        $angulation{$1}=$4;
        $angulation{$2}=$5;
        $angulation{$3}=$6;
      }
      # distance from origin of FOV center
      if (/Off Centre midslice\((..),(..),(..)\) \[.*\].*:\ *(-?[0-9]+\.[0-9]+)\ *(-?[0-9]+\.[0-9]+)\ *(-?[0-9]+\.[0-9]+)/) {
        $origin{$1}=$4;
        $origin{$2}=$5;
        $origin{$3}=$6;
      }

      # number of slices
      if (/number of slices.*:\ *([0-9]+)/) {
        $slices=$1;
      }

      # TR
      if (/Repetition time.*:\ *([0-9]+\.[0-9]+)/) {
        $tr=$1;
      }
      # number of volumes
      if (/number of dynamics.*:\ *([0-9]+)/) {
        $volumes=$1;
      }

      if (/\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*([0-9]*)\s*[0-9]*\s*([0-9]*)\s*([0-9]*).*/) {
        $bitdepth=$1;
        $reconres{"x"}=$2;
        $reconres{"y"}=$3;
      }
    }
    $lineCount++;
  } # end of while (<PFH>)

  if (shift(@buffer) =~ /\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*[0-9]*\s*([0-9]*).*/){
    my $numberOfImages = $1 + 1;
    if ($verbose) {
      print "*** Number of images is $numberOfImages";
    }

    if ($numberOfImages / $slices != $volumes) {
      print "*** WARNING: The number of volumes from the PAR header does not match the number of slices times the number of images!";
      $volumes = $numberOfImages / $slices;
      print "*** WARNING: Resetting the number of volumes to be the number of slices times the number of images ($volumes).";
    }
  }

  close(PFH);

  if ($verbose) {
    print "$rootname.$extension";
    print "** Field of View ***";
    print "ap=$fov{ap}";
    print "fh=$fov{fh}";
    print "rl=$fov{rl}";
    print "*** Reconstruction Matrix ***";
    print "x=$reconres{x}";
    print "y=$reconres{y}";
    print "*** Angulation ***";
    print "ap=$angulation{ap}";
    print "fh=$angulation{fh}";
    print "rl=$angulation{rl}";
    print "*** Origin ***";
    print "ap=$origin{ap}";
    print "fh=$origin{fh}";
    print "rl=$origin{rl}";
    print "*** Slices = $slices ***";
    print "*** Bit Depth = $bitdepth ***";
    print "*** TR = $tr ***";
  }

  # -----------calculate values and args for AFNI to3d to convert REC file
  #
  if ($outputDirectory) {
    $session = "-session $outputDirectory/";
  }
  else {
    $session = "";
  }

  if ($nifti) {
    $prefixargs = "-prefix $rootname.nii";
  }
  else {
    $prefixargs = "-prefix $rootname";
  }

  my $swap = "";
  if ($twoSwap) {
    $swap = "-2swap";
  }
  elsif ($fourSwap) {
    $swap = "-4swap";
  }

  # generate time args for func or anat
  $timeargs = "";
  $totalslices = $slices;

  if ($volumes > 1) {
    if ($verbose) {
      print "$volumes timepoints";
    }
    $timeargs = "-time:tz $volumes $slices ${tr}ms zero";
    #    $timeargs = "-time:zt $slices $volumes ${tr}ms zero";
    $totalslices=$volumes*$slices;
    if ($skipOutliers) {
      $outliers="-skip_outliers";
    }
  }

  #calculate FOV args
  $anterior = abs($origin{ap}-$fov{ap}/2);
  $posterior= abs($origin{ap}+$fov{ap}/2);
  $foot     = abs($origin{fh}-$fov{fh}/2);
  $head     = abs($origin{fh}+$fov{fh}/2);
  $right    = abs($origin{rl}-$fov{rl}/2);
  $left     = abs($origin{rl}+$fov{rl}/2);

  $FOVargs = "-xFOV ${right}R-${left}L -yFOV ${anterior}A-${posterior}P -zFOV ${foot}I-${head}S";

  #file specification
  my $recextension="rec";
  if ($extension =~ /PAR/) {
    $recextension="REC";
  }
  $filespec = "3D:-1:0:$reconres{x}:$reconres{y}:$totalslices:$rootname.$recextension";
  my $fileSize = -s "$rootname.$recextension";

  if ( $fileSize > 0 ) {
    # to3d command
    print my $command="to3d $session $swap $outliers $prefixargs $timeargs $FOVargs $filespec";
    system $command;
    if ($analyze) {
        print my $command="3dAFNItoANALYZE $rootname $rootname+orig.HEAD";
        system $command;
        print $command="rm -f  $rootname+orig.{HEAD,BRIK}";
        system $command;
    }
    if ($gzip) {
      if ($nifti) {
        $command="gzip -9v $rootname.nii";
      }
      elsif ($analyze) {
        $command="gzip -9v $rootname.img";
      }
      else {
        $command="gzip -9v $rootname+orig.BRIK";
      }
      print $command;
      system $command;
    }
  }
  else {
    print "Skipping $parFile: The corresponding REC file is 0 bytes in length\n";

  }
}

sub main() {
  my $argCount = $#ARGV + 1;
  usage() if $Options{"h"} or $argCount < 1;

  foreach my $parFile (@ARGV) {
    convertPar($parFile);
  }
}

main();
