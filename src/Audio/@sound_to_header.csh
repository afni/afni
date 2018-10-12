#!/bin/tcsh

if( $#argv < 2 )then
  echo
  echo "@sound_to_header.csh infile duration"
  echo
  echo "Uses 'sox' to convert a sound file to a C header file,"
  echo "for use in AFNI."
  echo " * infile is something that 'sox' can read"
  echo " * duration is the desired output file duration in seconds"
  echo " * some manual editing may be needed when done"
  echo
  exit 0
endif

set infile = $argv[1]
set outdur = $argv[2]

if( ! -f $infile )then
  echo "** Can't read file $infile" ; exit 1
endif

# strip suffix off infile

set prefix = `echo $infile | sed -e 's/\..*$//'`

# convert to 16000 samples per sec (AFNI) to find input duration
# note use of '-L' because this is for use on an Intel system

echo " = converting $infile"

sox $infile -b 16 -e signed-integer -r 16000 -t raw -L $infile.raw

if( ! -f $infile.raw )then
  echo "** Didn't resample convert to $infile.raw" ; exit 1
endif

# find number of samples (2 bytes each) in this file

@ nsam = `wc -c < $infile.raw` / 2

# conversion temp factor to get the desired output duration

set temp = `ccalc "${nsam}/(16000*${outdur})"`

\rm -f $infile.raw

# re-convert it, so it has the desired duration

echo " = tempo conversion to get desired output duration = $temp"

sox $infile -b 16 -e signed-integer -r 16000 -t raw -L $infile.raw tempo $temp

if( ! -f $infile.raw )then
  echo "** Didn't tempo convert to $infile.raw" ; exit 1
endif

# How many samples now?

@ nsam = `wc -c < $infile.raw` / 2

# This is how many we want for the given duration

set nsss = `ccalc -int "int(16000*${outdur}+0.444)"`

# if we have too many now, truncate from the beginning

if( $nsss < $nsam )then
  @ nskip = $nsam - $nsss
else
  set nskip = 0
  set nsss  = $nsam
endif

# output in 10 columns, so we also truncate to a multiple of 10

@ ny   = $nsss / 10
@ nsss = $ny * 10

# convert output shorts to a text file  (1dcat)
# tranpose that file                    (1dtranspose)
# remove unneeded blanks and add commas (sed)

1dcat 3D:${nskip}:0:10:${ny}:1:$infile.raw     \
  | 1dtranspose stdin:                         \
  | sed -e 's/      / /g' -e 's/  / /g'        \
        -e 's/  / /g' -e 's/  / /g'            \
        -e 's/  / /g' -e 's/  / /g'            \
        -e 's/  / /g' -e 's/  / /g'            \
        -e 's/  / /g' -e 's/  / /g'            \
        -e 's/^ *//' -e 's/ /,/g' -e 's/$/,/'    > $infile.raw.txt

\rm $infile.raw

# add C header stuff at the beginning and end

echo "#define ${prefix}_nsam $nsss"                             > $prefix.h
echo "static char *${prefix}_name = " '"'"$prefix"'" ;'        >> $prefix.h
echo "static short ${prefix}_data[] = { /* $infile @ 16K */"   >> $prefix.h

cat $infile.raw.txt                                            >> $prefix.h

echo "0 } ; /* $infile */"                                     >> $prefix.h

\rm $infile.raw.txt

echo "== Output file = $prefix.h"
echo
