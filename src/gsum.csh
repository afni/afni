#!/bin/tcsh

## This script counts up who is to 'blame' for lines of code/text in the AFNI
## files. It runs for a long time, running 'git blame' over 1000+ files, and
## grepping out counts for over a dozen co-conspirators.
## -- RWCox -- Sep 2019

# set list of source files to query
# stuff from outside sources (e.g., eispack) is not included here

if ( 1 ) then
  set flist = ( af*.[ch] mri*.[ch] thd*.[ch] cs*.[ch] 3d*.[ch] edt*.[ch] suma*.[ch]     \
                plug*.c niml/niml*.[ch] coxplot/*.[ch] SUMA/*.[ch] 1d*.[ch] model*.c    \
                display.[ch] imseq.[ch] bbox.[ch] xim.[ch] xutil.[ch] xutil_webber.[ch] \
                nifti_statlib.[ch] `find nifti -name '*.[ch]'`                          \
                rickr/*.[ch] ptaylor/*.[ch]                                             \
                `git ls-tree --name-only -r master | grep scripts_install`              \
                python_scripts/afni_python/*.py R_scripts/*.R                             )
else
# for testing
  set flist = ( afni.c imseq.c )
endif

# run the Count Lines Of Code script, if present

which cloc-1.64.pl >& /dev/null
if ( $status == 0 ) then
  cloc-1.64.pl --quiet $flist
endif

# list of authors with only one alias

set alist = ( Cox Craddock discoraj Froehlich Gang
              Gaudes Glen Hammett Kaczmarzyk
              Molfese Oosterhof Rick Schwabacher Warren )

# list of authors with two aliases (i.e., troublemakers)

set blist1 = ( Nielson      Saad Taylor  )
set blist2 = ( shotgunosine ziad mrneont )

# tsum = total sum of lines
set tsum = 0

# nn = number of files processed
set nn   = 0

# counts for the alist
set anum = $#alist
set aqq  = ( `count -dig 1 1 $anum` )
set asum = ( )
foreach uuu ( $alist )
  set asum = ( $asum 0 )
end

# counts for the blist
set bnum = $#blist1
set bqq  = ( `count -dig 1 1 $bnum` )
set bsum = ( )
foreach uuu ( $blist1 )
  set bsum = ( $bsum 0 )
end

# loop over source files, plus README documents

foreach fff ( $flist ../doc/README/README.* )
 # lines in this file
  set aa = `cat $fff | wc -l` ; @ tsum += $aa
 # get the list of blamees for this file
  git blame $fff > gsum.junk.txt
 # loop over the alist and grep out count for each one
  foreach qq ( $aqq )
    set aa = `grep -i $alist[$qq] gsum.junk.txt | wc -l` ; @ asum[$qq] += $aa
#    echo "user $alist[$qq]  file $fff  lines = $aa  sum = $asum[$qq]"
  end
 # loop over the blist, and get their counts
  foreach qq ( $bqq )
    set aa = `grep -i -e $blist1[$qq] -e $blist2[$qq] gsum.junk.txt | wc -l` ; @ bsum[$qq] += $aa
#    echo "user $blist1[$qq]  file $fff  lines = $aa  sum = $bsum[$qq]"
  end
 # prints a progress pacifier
  @ nn ++
  if( $nn % 20 == 0 ) printf "{$fff} "
end
printf "\n"
\rm -f gsum.junk.txt

# format the final report

echo "Lines of Source: total = $tsum"
echo "---------------"

foreach qq ( $aqq )
  if ( $asum[$qq] > 0 ) then
    set perc  = `ccalc -form '%5.2f%%' "100*$asum[$qq]/$tsum"`
    printf " %12s  %6s  %s\n" $alist[$qq] $asum[$qq] $perc
  endif
end

foreach qq ( $bqq )
  if ( $bsum[$qq] > 0 ) then
    set perc  = `ccalc -form '%5.2f%%' "100*$bsum[$qq]/$tsum"`
    printf " %12s  %6s  %6s\n" $blist1[$qq] $bsum[$qq] $perc
  endif
end
