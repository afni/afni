#!/bin/tcsh

if( $#argv < 1 )then
  set dir = $HOME/abin
else
  set dir = $argv[1]
endif

if( ! -d $dir )then
  echo "**ERROR: can't find directory $dir" ; exit 1
endif

pushd $dir

find . -type f -executable -exec chmod -v ugo+rx {} \;

popd

exit 0
