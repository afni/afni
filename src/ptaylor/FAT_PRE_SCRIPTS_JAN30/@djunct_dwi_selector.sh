#!/bin/bash

######################################################
## 03/06/2017 Justin Rajendra
## Get volumes from pngs for Paul's function
## will append to the output file

## go where you want
#cd /Users/discoraj/research/pick_slice

## stupid macs
#export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace

## original input dwi
dwi=$1 #"dwi.nii"

## input a png output from Paul's functions
png=$2 # "PREF_sepscl.axi.png"

## output file
file_out=$3 #"volumes_selected.txt"

######################################################
## get the coordinates

## open afni
afni -yesplugouts $png &
sleep 5

## fix the crosshairs and orientation
plugout_drive                           \
    -com "SET_XHAIRS SINGLE"            \
    -com "SETENV AFNI_ORIENT	RAI"      \
    -quit

## get the coordinates
## loop until done
while [ TRUE ];
do
	 ## dialog to confirm coordinate or stop recording
	 check_out=`prompt_user -pause "Select volume in viewer and click "OK" to save.\n Click cancel to stop."`
    
	 ## save the current crosshair coordinate to the file coords.txt
	 ## (will be created in you current folder)
	 if [ "$check_out" -eq "1" ]; then 
		  plugout_drive                     \
            -com 'SET_OUTPLUG coords.txt' \
            -com 'GET_DICOM_XYZ' -quit
	 elif [ "$check_out" -eq "0" ]; then 
        ## comment out if you want to leave AFNI open when done
		  plugout_drive -com 'QUIT' -quit   
		  break
	 fi
done

######################################################
## calculate the volumes selected

## get the number of volumes
mdim=(`3dinfo -nv $dwi`)

## run through Paul's golden ratio to get the number of rows and
## columns
my_calc_func="@djunct_calc_mont_dims.py"
$my_calc_func $mdim temp.dat
padpars=(`grep -v "#" temp.dat`)
Ncol=${padpars[2]}
Nrow=${padpars[3]}

## clean up
\rm temp.dat

## get dimensions of the png in pixels
x_png=`3dinfo -ni $png`
y_png=`3dinfo -nj $png`

## read in the coordinates file and get the x,y
while read L
do
    x_sel[$i]=$(echo "$L" | cut -d' ' -f3)
    y_sel[$i]=$(echo "$L" | cut -d' ' -f4)
    ((i++))
done < coords.txt

## clean up
\rm -v coords.txt

## loop through the selected points and find the volume number
for i in $(eval echo {0..$(( ${#y_sel[@]} - 1 ))})
do
	 ## get the index of the selected cell counting from zero
	 x_ind=`ccalc -cint -eval "((${x_sel[$i]} + $x_png/2) * $Ncol / $x_png) - 1"`
	 y_ind=`ccalc -cint -eval "((${y_sel[$i]} + $y_png/2) * $Nrow / $y_png) - 1"`

	 ## calculate the volume number from the indices
	 vol_sel=`ccalc -int -eval "$y_ind * $Ncol + $x_ind"`

	 ## save out volume numbers to text file
    ## *append* to what is there, in case on is preexisting
	 echo $vol_sel >> $file_out
done
