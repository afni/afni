#!/bin/tcsh

# either include the names of the gifti files on the command line,
# or be sitting in the directory with them
 
if ( $#argv > 1 ) then
    set files = ( $argv )
else
    set files = ( gifti.*.gii )
endif

foreach file ( $files )
    echo -n "$file : "
    gifti_test -infile $file -gfile new.gii
    if( $status ) then
        echo "****** FAILURE ********"
    else
        cmp $file new.gii
        if( ! $status ) echo okay
    endif
end
