#!/bin/tcsh

setenv AFNI_ENVIRON_WARNINGS NO

find ~/abin -depth 1 -name '[13]d*' > qhelp.txt
find ~/abin -depth 1 -name '*.py'  >> qhelp.txt
find ~/abin -depth 1 -name '@*'    >> qhelp.txt

set plist = ( `sort qhelp.txt | uniq` )

if( -f AFNI.help.txt ) \rm AFNI.help.txt
touch AFNI.help.txt

foreach fred ( $plist )

  if( ! -x $fred ) continue

  if( -f qhelp.txt ) \rm qhelp.txt

  $fred -help >& qhelp.txt

  set nn = `wc -l < qhelp.txt`
  if( $nn < 20 ) continue

  cat qhelp.txt >> AFNI.help.txt
  echo ''     >> AFNI.help.txt

end


mpage -1H AFNI.help.txt > AFNI.help.ps

\rm AFNI.help.txt
if( -f qhelp.txt ) \rm qhelp.txt

exit 0
