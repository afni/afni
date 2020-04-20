#!/bin/tcsh

## This script counts up who is to 'blame' for lines of code/text in the AFNI
## files. It runs for a long time (30+ minutes), running 'git blame' over
## 1000+ files, and grepping out counts from each one for over a dozen
## co-conspirators.
## -- RWCox -- Sep 2019

# set list of source files to query
# stuff from outside sources (e.g., eispack, sonnets.h) is not included here

if ( 1 ) then
  set flist = ( af*.[ch] mri*.[ch] thd*.[ch] cs*.[ch] 3d*.[ch] edt*.[ch] suma*.[ch]     \
                plug*.c niml/niml*.[ch] coxplot/*.[ch] SUMA/*.[ch] 1d*.[ch] model*.c    \
                display.[ch] imseq.[ch] bbox.[ch] xim.[ch] xutil.[ch] xutil_webber.[ch] \
                nifti_statlib.[ch] `find nifti -name '*.[ch]'`                          \
                rickr/*.[ch] ptaylor/*.[ch] gifti/*.[ch] svm/*.[ch]                     \
                `git ls-tree --name-only -r master | grep scripts_install`              \
                `find discoraj -type f` `find shiny -type f -name '*.R'`                \
                python_scripts/afnipy/*.py R_scripts/*.R                           \
                ../tests/scripts/*.py ../tests/scripts/utils/*.py                         )
else
# for quicker testing
  set flist = ( afni.c imseq.c )
endif

# run the Count Lines Of Code script, if present (this is fast)

which cloc-1.64.pl >& /dev/null
if ( $status == 0 ) then
  cloc-1.64.pl --quiet $flist
endif

# list of authors needing only one alias (not case sensitive)

set alist = ( Cox Craddock discoraj Froehlich Gang        \
              Gaudes Glen Hammett Kaczmarzyk LeeJ3        \
              Laconte Lisinski Clark Johnson Julia        \
              Molfese Oosterhof Rick Schwabacher Warren Markello )

# list of authors needing two aliases (i.e., troublemakers)

set blist1 = ( Nielson      Saad Taylor  afniHQ )
set blist2 = ( shotgunosine ziad mrneont Ubuntu )

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

# grep option to remove known authors, to count unknowns

set gunk = ( -v -i )
foreach uuu ( $alist $blist1 $blist2 )
  set gunk = ( $gunk -e $uuu )
end

# will acccumulate all unknown lines in to one file
if( -f gsum.unk.txt ) \rm -f gsum.unk.txt
touch gsum.unk.txt

# loop over source files, plus README documents

printf "\nblaming "

set glist = ( $flist ../doc/README/README.* )
foreach fff ( $glist )

 # get the list of blamees for this file (grep out blank lines)
  git blame $fff | grep -v '[0-9]) $' > gsum.junk.txt

 # lines in this file
  set aa = `wc -l < gsum.junk.txt` ; @ tsum += $aa

 # loop over the alist and grep out count for each one
  foreach qq ( $aqq )
    set aa = `grep -i "$alist[$qq]" gsum.junk.txt | wc -l` ; @ asum[$qq] += $aa
  end

 # loop over the blist, and get their counts
  foreach qq ( $bqq )
    set aa = `grep -i -e "$blist1[$qq]" -e "$blist2[$qq]" gsum.junk.txt | wc -l` ; @ bsum[$qq] += $aa
  end

 # accumulate the lines with unknown authors
  grep $gunk gsum.junk.txt >> gsum.unk.txt

 # print a progress pacifier
  @ nn ++ ; if( $nn % 20 == 0 ) printf "%d/%d " $nn $#glist

end

# cleanup after loop over files

printf "... total line count = %d \n" $tsum
\rm -f gsum.junk.txt
touch gsum.junk.txt

# count total number of unknown lines now

set aa = `wc -l < gsum.unk.txt` ; @ unksum = $aa

# format lines for the final report

foreach qq ( $aqq )
  if ( $asum[$qq] > 0 ) then
    set perc  = `ccalc "100*$asum[$qq]/$tsum"`
    printf " %12s  %6s  %5.2f%%\n" "$alist[$qq]" $asum[$qq] $perc >> gsum.junk.txt
  endif
end

foreach qq ( $bqq )
  if ( $bsum[$qq] > 0 ) then
    set perc  = `ccalc "100*$bsum[$qq]/$tsum"`
    printf " %12s  %6s  %5.2f%%\n" "$blist1[$qq]" $bsum[$qq] $perc >> gsum.junk.txt
  endif
end

if ( $unksum > 0 ) then
  set perc  = `ccalc "100*$unksum/$tsum"`
  printf " %12s  %6s  %5.2f%%\n" Unknown $unksum $perc >> gsum.junk.txt
endif

# header lines to the final report

echo " Contributor   Lines   %-age"    > gsum.out.txt
echo " ------------  ------  ------"  >> gsum.out.txt

# sort output lines by second column, put in final report

sort -n -r --key=2 gsum.junk.txt      >> gsum.out.txt

echo
cat gsum.out.txt

# toss out the junk

\rm -f gsum.junk.txt

exit 0
