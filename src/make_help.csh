#!/bin/tcsh

### Script to put all afni program -help outputs
###   into one big PostScript file AFNI.help.ps
###   and thence into AFNI.help.pdf
### Uses the mpage utility for formatting,
###   and then gs utility for conversion to PDF

setenv AFNI_ENVIRON_WARNINGS NO

# make the list of programs to run

find ~/abin -depth 1 -name '[13]d*' > qhelp.txt
find ~/abin -depth 1 -name '*.py'  >> qhelp.txt
find ~/abin -depth 1 -name '@*'    |  grep -v '@Install_' | grep -v 'OLD' >> qhelp.txt

# cast out duplicates

set plist = ( `sort qhelp.txt | uniq` )

if( -f AFNI.help.txt ) \rm AFNI.help.txt
touch AFNI.help.txt

foreach fred ( $plist )

# not executable?

  if( ! -x $fred ) continue

# skip .R programs
  set bbb = `basename $fred`
  set ccc = `basename $bbb .R`
  if( $bbb != $ccc ) continue

  if( -f qhelp.txt ) \rm qhelp.txt

  echo "++ running $fred -help"
# make a help text file
  $fred -help >& qhelp.txt

# skip it if is too short
  set nn = `wc -l < qhelp.txt`
  if( $nn < 20 ) continue

# put it into a big text file, along with a page feed
  cat qhelp.txt >> AFNI.help.txt
  echo ''     >> AFNI.help.txt

  echo " + `wc -l < AFNI.help.txt`" " lines of help so far"

end

# convert big text file to the PostScript output
mpage -1H AFNI.help.txt > AFNI.help.ps

# convert to PDF
gs -sDEVICE=pdfwrite -sOutputFile=AFNI.help.pdf -dNOPAUSE -q - < AFNI.help.ps

# take out the trash
if( -f qhelp.txt ) \rm qhelp.txt

exit 0
