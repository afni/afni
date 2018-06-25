#!/bin/tcsh

### Simple adjunct program-- don't think this is even necessary to
### include:
#@global_parse `basename $0` "$*" ; if ($status) exit 0

######################################################
## 03/06/2017 Justin Rajendra
## Get volumes from pngs for Paul's function
## will append to the output file
##
## 03/30/2017 Justin Rajendra
## added select and remove
##
## 06/25/2018 PA Taylor
## tcsh-ized, because of MACS...
##

if ( $#argv < 3 ) then
   echo "usage: $0 DWI PNG OUTFILE"
   exit 0
endif

## original input dwi
set dwi = $1
# dwi="dwi.nii"

## png output from fat_proc functions
set png = $2
# png="PREF_sepscl.axi.png"

## output file
set prefix = $3

set tmp_coors = "_coords_temp_HJKL.txt"

######################################################
## info about the images

## get the number of volumes
set mdim = `3dinfo -nv $dwi`

## run through Paul's golden ratio to get the number of rows and
## columns
set my_calc_func = "@djunct_calc_mont_dims.py"
$my_calc_func $mdim temp_ZSE.dat
set padpars = `grep -v "#" temp_ZSE.dat`
set Ncol = ${padpars[3]} 
set Nrow = ${padpars[4]}

## clean up
\rm temp_ZSE.dat

## get dimensions of the png in pixels
set x_png = `3dinfo -ni $png`
set y_png = `3dinfo -nj $png`

######################################################
## get the coordinates

## open afni
afni -yesplugouts $png &
sleep 5

## fix the crosshairs and orientation
plugout_drive                                   \
    -com "SET_XHAIRS SINGLE"                    \
    -com "SETENV AFNI_ORIENT RAI"               \
    -com "OPEN_WINDOW A.axialimage ifrac=1"     \
    -quit

## if there was something there already, print it out
touch ${prefix}
echo volumes selected:
cat ${prefix}

## get the coordinates
## loop until done
while ( 1 )

    ## dialog to confirm coordinate or stop recording
    set check_out = `prompt_popup -message \
    "Select volume in viewer with left-click.\nClick a button:\n  'Bad-vol'    to add an index to the bad-list;\n  'Unbad' to remove an index from the bad-list;\n  'Finish' and exit.\n\n** Check terminal output for running tally. **\n\nNB1: type 'a' to resize window if images\n     appear stretched/squashed.\nNB2: popup follows mouse after button clicks\n     (in case you move quick-quick)." \
    -b Bad-vol -b Unbad -b Finish`

    ## save the current crosshair coordinate to the file with the prefix name
    ## (will be created in you current folder)
    if ( "$check_out" == "1" ) then

        ## write out the coodinates
        plugout_drive -com 'QUIET_PLUGOUTS' \
                      -com "SET_OUTPLUG ${tmp_coors}" \
                      -com 'GET_DICOM_XYZ' -quit

        ## get the x/y coordinates from the temp file
        set x_sel = `cat "${tmp_coors}" | cut -d' ' -f3`
        set y_sel = `cat "${tmp_coors}" | cut -d' ' -f4`

        ## get the index of the selected cell counting from zero
        set x_ind = `ccalc -cint -eval "((${x_sel} + $x_png/2) * $Ncol / $x_png) - 1"`
        set y_ind = `ccalc -cint -eval "((${y_sel} + $y_png/2) * $Nrow / $y_png) - 1"`

        ## calculate the volume number from the indices
        set vol_sel = `ccalc -int -eval "$y_ind * $Ncol + $x_ind"`

        ## check if the volume was already selected
        unset grep_check
        set grep_check = `grep "\b${vol_sel}\b" ${prefix}`

        ## save out volume numbers to text file 
        ## ... think this works in tcsh syntax to count num of chars in str
        set gc_len = `echo -n ${grep_check} | wc -c`
        if ( "$gc_len" == "0" ) then
            echo $vol_sel >> ${prefix}
        endif

        ## print to terminal
        echo "volumes selected:"
        cat ${prefix}

        ## clean up
        \rm "${tmp_coors}"

    else if ( "$check_out" == "2" ) then

        ## write out the coodinates
        plugout_drive -com 'QUIET_PLUGOUTS' \
                      -com "SET_OUTPLUG ${tmp_coors}" \
                      -com 'GET_DICOM_XYZ' -quit

        ## get the x/y coordinates from the temp file
        set x_sel = `cat "${tmp_coors}" | cut -d' ' -f3`
        set y_sel = `cat "${tmp_coors}" | cut -d' ' -f4`

        ## get the index of the selected cell counting from zero
        set x_ind = `ccalc -cint -eval "((${x_sel} + $x_png/2) * $Ncol / $x_png) - 1"`
        set y_ind = `ccalc -cint -eval "((${y_sel} + $y_png/2) * $Nrow / $y_png) - 1"`

        ## calculate the volume number from the indices
        set vol_sel = `ccalc -int -eval "$y_ind * $Ncol + $x_ind"`

        ## find the volume to remove, remove and write to new file and
        ## rename file
        set tmp_grep = "_temp_vols_ASDF.txt"
        grep -v "\b${vol_sel}\b" ${prefix} > "$tmp_grep"
        \mv "$tmp_grep" "${prefix}"

        ## print to terminal
        echo "volumes selected:"
        cat ${prefix}

        ## clean up
        \rm "${tmp_coors}"

    else if ( "$check_out" == "3" ) then
        ## comment out if you want to leave AFNI open when done
        plugout_drive -com 'QUITT' -quit
      break
    endif
end
