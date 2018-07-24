#!/bin/tcsh

set here = $PWD
set odir = $here/PY2to3_TEST

# ----------------------------------------------------------------

if ( $#argv == 0 ) then
    echo "** ERROR before we even start! need a test number!"
    exit 1
endif

\mkdir -p $odir

set gonum = $1

set fail_mess = " **************** BAD ********************* "
set pass_mess = " :)  :)  :)  :)  :)  :)  :)  :)  :)  :)  :) "

goto NUM_${gonum}

# ===================================================================
# ===================================================================
# ===================================================================


NUM_3:

set fpy2   = @djunct_combine_str.py
# -------------------------------------------
set fff    = ${fpy2:gas/.py//}
set fpy3   = ${fff}_2to3.py

# compare three results/variations
set opy22    = "python $fpy2"
set opy23    = "python $fpy3"
set opy33    = "python3 $fpy3"

set allruns  = ( "$opy22" "$opy23" "$opy33" )
set allcodes = ( "22"     "23"     "33"     )

set ggg      = `printf "%03d" $gonum`
set otest    = $odir/test_${ggg}_${fff}
set orep23   = $odir/rep23_${ggg}_${fff}
set orep33   = $odir/rep33_${ggg}_${fff}
set oreps    = ( "" $orep23 $orep33 )

# prep output dir and report
\mkdir -p $otest
foreach k ( `seq 2 1 $#oreps` )
    printf "" > "$oreps[$k]"
end
# -------------------------------------------

set idx   = 0
# Make all tests:  need any integers >1
foreach ii ( `seq 2 1 101` ) 
    set iii    = `printf "%05d" $ii`
    @ idx      = $idx + 1
    set iiidx  = `printf "%05d" $idx`

    # need a file of excised numbers
    set maxind = `count -digits 1 10 200 R1`
    @ maxnum   = $maxind - 1

    # make a random number of files to combine
    set nrand  = `count -digits 1 2 5 R1`
    set my_str = ( )
    foreach k ( `seq 1 1 $nrand` ) 
        python test_help.py \
            $otest/numfile_${iiidx}_${k}.dat \
            $maxind                          \
            0.7
        set sss = `cat $otest/numfile_${iiidx}_${k}.dat`
        set my_str = ( "$my_str" "$sss" ) 
    end
    echo "MY STR: $my_str"
    foreach jj ( `seq 1 1 $#allruns` )
        set rr = "${allruns[$jj]}"
        set cc = "${allcodes[$jj]}"

        # actually execute stuff!
        $rr $otest/ofile_${iiidx}_${cc}.txt $maxind $my_str
    end
end

echo "++ Done testing: going to check now"

goto CHECK_IT
























# ===================================================================

NUM_2:

set fpy2   = @djunct_select_str.py
# -------------------------------------------
set fff    = ${fpy2:gas/.py//}
set fpy3   = ${fff}_2to3.py

# compare three results/variations
set opy22    = "python $fpy2"
set opy23    = "python $fpy3"
set opy33    = "python3 $fpy3"

set allruns  = ( "$opy22" "$opy23" "$opy33" )
set allcodes = ( "22"     "23"     "33"     )

set ggg      = `printf "%03d" $gonum`
set otest    = $odir/test_${ggg}_${fff}
set orep23   = $odir/rep23_${ggg}_${fff}
set orep33   = $odir/rep33_${ggg}_${fff}
set oreps    = ( "" $orep23 $orep33 )

# prep output dir and report
\mkdir -p $otest
foreach k ( `seq 2 1 $#oreps` )
    printf "" > "$oreps[$k]"
end
# -------------------------------------------

set idx   = 0
# Make all tests:  need any integers >1
foreach ii ( `seq 2 1 101` ) 
    set iii    = `printf "%05d" $ii`
    @ idx      = $idx + 1
    set iiidx  = `printf "%05d" $idx`

    # need a file of excised numbers
    set maxind = `count -digits 1 10 200 R1`
    @ maxnum   = $maxind - 1

    @ hind   = $maxind / 2
    set nrand  = `count -digits 1 0 $hind R1`
    if ( $nrand ) then
        set alldig = `count -digits 1 0 $maxnum R$nrand`
    else
        set alldig = ""
    endif

    set nfile  = $otest/numfile_${iiidx}.dat
    printf "" > $nfile

    foreach xx ( $alldig )
        echo $xx >> $nfile
    end

    foreach jj ( `seq 1 1 $#allruns` )
        set rr = "${allruns[$jj]}"
        set cc = "${allcodes[$jj]}"

        # actually execute stuff!
        $rr $nfile $maxind $otest/ofile_${iiidx}_${cc}.txt
    end
end

echo "++ Done testing: going to check now"

goto CHECK_IT

# ===================================================================

NUM_1:

set fpy2   = @djunct_calc_mont_dims.py
# -------------------------------------------
set fff    = ${fpy2:gas/.py//}
set fpy3   = ${fff}_2to3.py

# compare three results/variations
set opy22    = "python $fpy2"
set opy23    = "python $fpy3"
set opy33    = "python3 $fpy3"

set allruns  = ( "$opy22" "$opy23" "$opy33" )
set allcodes = ( "22"     "23"     "33"     )

set ggg      = `printf "%03d" $gonum`
set otest    = $odir/test_${ggg}_${fff}
set orep23   = $odir/rep23_${ggg}_${fff}
set orep33   = $odir/rep33_${ggg}_${fff}
set oreps    = ( "" $orep23 $orep33 )

# prep output dir and report
\mkdir -p $otest
foreach k ( `seq 2 1 $#oreps` )
    printf "" > "$oreps[$k]"
end
# -------------------------------------------

set idx = 200
goto CHECK_IT

set idx   = 0
# Make all tests:  need any integers >1
foreach ii ( `seq 2 1 201` ) 
    set iii    = `printf "%05d" $ii`
    @ idx      = $idx + 1
    set iiidx  = `printf "%05d" $idx`

    foreach jj ( `seq 1 1 $#allruns` )
        set rr = "${allruns[$jj]}"
        set cc = "${allcodes[$jj]}"

        # actually execute stuff!
        $rr $ii $otest/ofile_${iiidx}_${cc}.txt
    end
end

echo "++ Done testing: going to check now"

goto CHECK_IT

# ===================================================================
# ===================================================================
# ===================================================================

CHECK_IT:

echo "++ start check"

# Hopefully works for all text outputs?
set tally_bad = ( 0 ) 
foreach jj ( `seq 2 1 $#allruns` )
    set tally_bad = ( $tally_bad 0 ) 
end

foreach ii ( `seq 1 1 $idx` ) 
    set iii    = `printf "%05d" $ii`

    set c0 = "${allcodes[1]}"
    foreach jj ( `seq 2 1 $#allruns` )
        set cc = "${allcodes[$jj]}"

        set OOO = `comm -3                            \
                    $otest/ofile_${iiidx}_${c0}.txt   \
                    $otest/ofile_${iiidx}_${cc}.txt`

        # count bads
        if ( $#OOO ) then
            echo "FAIL $iii: $OOO" >> "${oreps[$jj]}"
            @ tally_bad[$jj] = $tally_bad[$jj] + 1
        else
            echo "PASS $iii" >> "${oreps[$jj]}"
        endif

    end
end
echo "++ UGH"

set FFFED   = 0
set checked = ( )
set checkct = ( )
foreach jj ( `seq 2 1 $#allruns` )
    if ( $tally_bad[$jj] > 0 ) then
        set FFFED   = 1
        set checked = ( "$checked" "grep -H FAIL ${oreps[$jj]} | wc" )
    else
        set checked = ( "$checked" "grep -H PASS ${oreps[$jj]} | wc" )
    endif

    set checkct = ( $checkct $tally_bad[$jj] )
end

if ( $FFFED ) then
    set omess = "$fail_mess"
    goto DO_EXIT
else
    set omess = "$pass_mess"
    goto DO_EXIT
endif

# ===================================================================
# ===================================================================
# ===================================================================

DO_EXIT:

cat << EOF



"$omess"

TEST  = $gonum
PROG  = $fpy2

TALLY = $checkct
NTEST = $idx

EOF

foreach k ( `seq 1 1 $#checked` )
    echo "$checked[$k]\n"
end

echo "\n\n" 
echo "$omess"
echo "\n\n" 

exit 0
