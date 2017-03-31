#!/bin/bash

######################################################
## 03/06/2017 Justin Rajendra
## Get volumes from pngs for Paul's function
## will append to the output file
## 03/30/2017 Justin Rajendra
## added select and remove

## go where you want
# cd /Users/discoraj/research/pick_slice

## stupid macs
#export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace

## original input dwi
dwi=$1
# dwi="dwi.nii"

## png output from fat_proc functions
png=$2
# png="PREF_sepscl.axi.png"

## output file
prefix=$3

tmp_coors="_coords_temp_HJKL.txt"

######################################################
## info about the images

## get the number of volumes
mdim=(`3dinfo -nv $dwi`)

## run through Paul's golden ratio to get the number of rows and
## columns
my_calc_func="@djunct_calc_mont_dims.py"
$my_calc_func $mdim temp_ZSE.dat
padpars=(`grep -v "#" temp_ZSE.dat`)
Ncol=${padpars[2]}
Nrow=${padpars[3]}

## clean up
\rm temp_ZSE.dat

## get dimensions of the png in pixels
x_png=`3dinfo -ni $png`
y_png=`3dinfo -nj $png`

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
while [ TRUE ];
do
    ## dialog to confirm coordinate or stop recording
    check_out=`prompt_popup -message \
    "Select volume in viewer with left-click.\nClick a button to:\n  'Add'    an index to the list;\n  'Remove' an index from the list;\n  'Finish' and exit.\n\n** Check terminal output for running tally. **\n\nNB1: type 'a' to resize window if images\n     appear stretched/squashed.\nNB2: popup follows mouse after button clicks\n     (in case you move quick-quick)." \
    -b Add -b Remove -b Finish`

    ## save the current crosshair coordinate to the file with the prefix name
    ## (will be created in you current folder)
    if [ "$check_out" -eq "1" ]; then

        ## write out the coodinates
        plugout_drive -com 'QUIET_PLUGOUTS' \
                      -com "SET_OUTPLUG ${tmp_coors}" \
                      -com 'GET_DICOM_XYZ' -quit

        ## get the x/y coordinates from the temp file
        x_sel=`cat "${tmp_coors}" | cut -d' ' -f3`
        y_sel=`cat "${tmp_coors}" | cut -d' ' -f4`

        ## get the index of the selected cell counting from zero
        x_ind=`ccalc -cint -eval "((${x_sel} + $x_png/2) * $Ncol / $x_png) - 1"`
        y_ind=`ccalc -cint -eval "((${y_sel} + $y_png/2) * $Nrow / $y_png) - 1"`

        ## calculate the volume number from the indices
        vol_sel=`ccalc -int -eval "$y_ind * $Ncol + $x_ind"`

        ## check if the volume was already selected
        unset grep_check
        grep_check=`grep "\b${vol_sel}\b" ${prefix}`

        ## save out volume numbers to text file
        if [ -z "${grep_check}" ]; then
            echo $vol_sel >> ${prefix}
        fi

        ## print to terminal
        echo volumes selected:
        cat ${prefix}

        ## clean up
        \rm "${tmp_coors}"

    elif [ "$check_out" -eq "2" ]; then

        ## write out the coodinates
        plugout_drive -com 'QUIET_PLUGOUTS' \
                      -com "SET_OUTPLUG ${tmp_coors}" \
                      -com 'GET_DICOM_XYZ' -quit

        ## get the x/y coordinates from the temp file
        x_sel=`cat "${tmp_coors}" | cut -d' ' -f3`
        y_sel=`cat "${tmp_coors}" | cut -d' ' -f4`

        ## get the index of the selected cell counting from zero
        x_ind=`ccalc -cint -eval "((${x_sel} + $x_png/2) * $Ncol / $x_png) - 1"`
        y_ind=`ccalc -cint -eval "((${y_sel} + $y_png/2) * $Nrow / $y_png) - 1"`

        ## calculate the volume number from the indices
        vol_sel=`ccalc -int -eval "$y_ind * $Ncol + $x_ind"`

        ## find the volume to remove, remove and write to new file and
        ## rename file
        tmp_grep="_temp_vols_ASDF.txt"
        grep -v "\b${vol_sel}\b" ${prefix} > "$tmp_grep"
        \mv "$tmp_grep" "${prefix}"

        ## print to terminal
        echo volumes selected:
        cat ${prefix}

        ## clean up
        \rm "${tmp_coors}"

    elif [ "$check_out" -eq "3" ]; then
        ## comment out if you want to leave AFNI open when done
        plugout_drive -com 'QUITT' -quit
      break
    fi
done
