#!/bin/bash
# \author Hans J. Johnson
#
# A script to assist with removing trailing whitespace from file lines
# This script searches for tabs or spaces at the end of file lines
# and removes them.

# strip trailing whitespaces from filenames sent to stdin or as args

if which tempfile &>/dev/null; then
  TEMPMAKER=tempfile
elif which mktemp &>/dev/null; then
  TEMPMAKER=mktemp
else
  echo "Cannot find tempfile program." 2>&1
  exit 1
fi

MYTEMP=$($TEMPMAKER)
trap 'rm -f $MYTEMP' SIGINT SIGTERM

stripit() {
  echo "stripping $1"
  sed 's/[ 	][ 	]*$//' "$1" > $MYTEMP
  cp $MYTEMP "$1"
}

if [ $# -gt 0 ]; then
  while [ "$1" != "" ]; do
    stripit $1
    shift
  done
else
  while read -t 2; do
    stripit $REPLY
  done
fi

rm $MYTEMP

exit $?

#### Suggested way to strip all whitespace from all sourcecode files
if [[ 0 -eq 1 ]]; then
cd ${MYSRC}

for ff in $(find . -name "*.c" -o -name "*.h" -o -name "*.[cht]xx" -o -name "*.cpp"); do
   ITK/Utilities/ITKv5Preparation/strip-trailing-whitespace $ff;
done

fi
