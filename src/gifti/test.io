#!/bin/tcsh

# either include the names of the gifti files on the command line,
# or be sitting in the directory with them

set skip_ts = 0

# if the user specifies -nots, skip the time series dataset
set narg = $#argv
set base = 1
if ( $narg >= 1 ) then
    if ( "$argv[1]" == "-help" ) then
        echo "usage: test.io [-nots] [files...]"
        echo ""
        echo "option:"
        echo "    -nots         : do not process *time_series*.gii"
        echo ""
        echo "examples:"
        echo "    test.io"
        echo "    test.io *.gii"
        echo "    test.io -nots"
        echo "    test.io -nots *.gii"

        exit
    else if ( "$argv[1]" == "-nots" ) then
        @ narg --
        set base = 2
        set skip_ts = 1
    endif
endif
 
if ( $narg >= 1 ) then
    set files = ( $argv[$base-] )
else
    set files = ( gifti.*.gii )
endif

foreach file ( $files )
    echo -n "$file : "

    if ( $skip_ts && "$file" =~ *time_series*.gii ) then
        echo "skipping ..."
        continue
    endif

    gifti_test -infile $file -gfile new.gii
    if( $status ) then
        echo "****** FAILURE ********"
    else
        cmp $file new.gii
        if( ! $status ) echo okay
    endif
end
