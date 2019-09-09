#!/bin/tcsh

#################################################################################
## This script counts up who is to 'blame' for lines of code/text in the AFNI
## files. It runs for a long time (30+ minutes), running 'git blame' over
## 1000+ files, and grepping out counts from each one for over a dozen
## co-conspirators.
## Output files:
##  gitsum.out.txt    = author line counts (also cat-ed to stdout)
##  gitsum.unkown.txt = lines that had unknown authors (for further research)
##
## -- RWCox -- Sep 2019
#################################################################################

# set list of source files to query
# stuff from outside sources (e.g., eispack, sonnets.h) is not included here

if ( 0 ) then
# most things
  echo "Finding files"
  set qlist = ( `git ls-tree --name-only -r HEAD | grep -v /`               \
                `git ls-tree --name-only -r HEAD | grep svm/`               \
                `git ls-tree --name-only -r HEAD | grep gifti/`             \
                `git ls-tree --name-only -r HEAD | grep rickr/`             \
                `git ls-tree --name-only -r HEAD | grep scripts_install/`   \
                `git ls-tree --name-only -r HEAD | grep ptaylor/`           \
                `git ls-tree --name-only -r HEAD | grep discoraj/`          \
                `git ls-tree --name-only -r HEAD | grep shiny/`             \
                `git ls-tree --name-only -r HEAD | grep python_scripts/`    \
                `git ls-tree --name-only -r HEAD | grep R_scripts/`         \
                `git ls-tree --name-only -r HEAD | grep tests/`             \
                `git ls-tree --name-only -r HEAD | grep pkundu/`            \
                `git ls-tree --name-only -r HEAD | grep coxplot/`           \
                `git ls-tree --name-only -r HEAD | grep niml/`              \
                `git ls-tree --name-only -r HEAD | grep SUMA/`              \
                `git ls-tree --name-only -r HEAD | grep nifti/`             \
                `git ls-tree --name-only -r HEAD | grep scripts_src/`       \
                `git ls-tree --name-only -r HEAD ../tests`                     )
else if ( 1 ) then
# everything minus the excludes (which aren't by anyone in SSCC)
  echo "Finding files"
  set flist = ( `git ls-tree --name-only -r HEAD ..` )
  set exclude = ( -v -e qhulldir/ -e jpeg-6b/ -e mpeg_encodedir/ -e faces/ -e eispack/   \
                     -e f2cdir/ -e matlab/ -e volpack/ -e maple/ -e poems/ -e gifsicledir/ -e XmHTML/ )
  set qlist = ( `echo $flist | xargs -n1 echo | grep $exclude` )
  unset flist
else
# for quicker testing
  set qlist = ( afni.c imseq.c suma_datasets.c pbar*.[ch] )
endif

# make sure list doesn't have duplicates or other undesired files

set flist = ( `echo $qlist | xargs -n1 echo |                                       \
              grep -v -i -e '\.jpg' -e '\.jpeg' -e '\.png' -e '\.html' -e '\.pdf' | \
              sort | uniq` )

echo "File count = $#flist"

# run the Count Lines Of Code script, if present (this is fast)

# which cloc-1.64.pl >& /dev/null
# if ( $status == 0 ) then
#   cloc-1.64.pl --quiet $flist
#   echo
# endif

# list of authors needing only one alias (not case sensitive)
# - anyone whose alias has spaces in it is out of luck

set alist = ( Cox Craddock discoraj Froehlich Gang  \
              Gaudes Glen Hammett Kaczmarzyk Lee    \
              Laconte Lisinski Clark Johnson Julia  \
              Molfese Oosterhof Rick Schwabacher    \
              Vincent Warren Markello Halchenko     \
              Vovk Zosky Torres Schmidt                )

# list of authors needing two aliases (i.e., troublemakers)
# - anyone who has three aliases is out of luck

set blist1 = ( Nielson      Saad Taylor  afniHQ Vinai )
set blist2 = ( shotgunosine ziad mrneont Ubuntu V..R  )

# tsum = total sum of lines thus far
set tsum = 0

# nn = number of files processed thus far
set nn   = 0

# setup counts for the alist
set anum = $#alist
set aqq  = ( `count -dig 1 1 $anum` )
set asum = ( )
foreach uuu ( $alist )
  set asum = ( $asum 0 )
end

# setup counts for the blist
set bnum = $#blist1
set bqq  = ( `count -dig 1 1 $bnum` )
set bsum = ( )
foreach uuu ( $blist1 )
  set bsum = ( $bsum 0 )
end

# grep command option to remove known authors, for counting unknowns

set gunk = ( -v -i )
foreach uuu ( $alist $blist1 $blist2 )
  set gunk = ( $gunk -e $uuu )
end

# will acccumulate all unknown lines in to one file, for later research
if( -f gitsum.unknown.txt ) \rm -f gitsum.unknown.txt
touch gitsum.unknown.txt

# loop over source files, plus README documents

printf "start blaming "

foreach fff ( $flist )

 # skip directories or non-existing files or non-ASCII files
  if ( ! -f $fff || -z $fff ) continue
  set aa = `file --mime $fff | grep ascii | wc -l`
  if( $aa == 0 ) continue

 # get and save the list of blamees for this file (grep out blank lines)
  git blame $fff | grep -v '[0-9]) $' > gitsum.junk.txt

 # count total lines in this file, sum them up
  set aa = `wc -l < gitsum.junk.txt` ; @ tsum += $aa

 # loop over the alist and grep out count for each author, sum it up
  foreach qq ( $aqq )
    set aa = `grep -i -e "$alist[$qq]" gitsum.junk.txt | wc -l` ; @ asum[$qq] += $aa
  end

 # loop over the blist and get their counts, sum them up
  foreach qq ( $bqq )
    set aa = `grep -i -e "$blist1[$qq]" -e "$blist2[$qq]" gitsum.junk.txt | wc -l` ; @ bsum[$qq] += $aa
  end

 # save all the lines with unknown authors into a separate file
  grep $gunk gitsum.junk.txt >> gitsum.unknown.txt

 # print a progress pacifier
  @ nn ++ ; if( $nn % 20 == 0 ) printf "%d/%d " $nn $#flist

end

# cleanup after loop over files

printf "... total line count = %d \n" $tsum
\rm -f gitsum.junk.txt
touch gitsum.junk.txt

# count total number of unknown lines now

set aa = `wc -l < gitsum.unknown.txt` ; @ unksum = $aa

# format lines for the final report into a temp file

foreach qq ( $aqq )
  if ( $asum[$qq] > 0 ) then
    set perc  = `ccalc "100*$asum[$qq]/$tsum"`
    printf " %12s  %7s  %6.2f%%\n" "$alist[$qq]" $asum[$qq] $perc >> gitsum.junk.txt
  endif
end

foreach qq ( $bqq )
  if ( $bsum[$qq] > 0 ) then
    set perc  = `ccalc "100*$bsum[$qq]/$tsum"`
    printf " %12s  %7s  %6.2f%%\n" "$blist1[$qq]" $bsum[$qq] $perc >> gitsum.junk.txt
  endif
end

if ( $unksum > 0 ) then
  set perc  = `ccalc "100*$unksum/$tsum"`
  printf " %12s  %7s  %6.2f%%\n" Unknown $unksum $perc >> gitsum.junk.txt
endif

# Put header lines into the final report

echo   " Contributor    Lines    %-age"              > gitsum.out.txt
echo   " ------------  -------  -------"            >> gitsum.out.txt
printf " %12s  %7s  %6.2f%%\n" Everyone $tsum 100 >> gitsum.out.txt

# sort output lines by second column, put in final report

sort -n -r --key=2 gitsum.junk.txt      >> gitsum.out.txt

# let the user see the results

echo
cat gitsum.out.txt

# toss out the junk

\rm -f gitsum.junk.txt

exit 0
