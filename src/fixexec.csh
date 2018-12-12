#!/bin/tcsh

## this script is to fix the executable status for files in the AFNI binaries,
## since some of them end up not being readable/runnable by the world
## -- RWCox 12 Dec 2018

if( $#argv < 1 )then
  set dir = $HOME/abin
else
  set dir = $argv[1]
endif

if( ! -d $dir )then
  echo "**ERROR: can't find directory $dir" ; exit 1
endif

pushd $dir

find . -executable -exec chmod -v ugo+rx {} \;

popd

exit 0
