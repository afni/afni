#!/bin/tcsh


# -----------------------------------------------------------------
# Multipurpose, driven viewer.  Meant to be used for auto QC in
# scripts. 
# 
# This function constructed by: PA Taylor (NIMH, NIH, USoA)
# 
# Dec, 2016:
#     + conception and start
#
# Jan, 2017:
#     + add I/O
#

# current ver
set ver = 1.0

# For more info about opts/fields, see:
#
# https://afni.nimh.nih.gov/pub/dist/doc/program_help/AFNI.afnirc.html
#
# https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html

# ----------------- default/initialized environments --------------

# GUI settings for simplicity
setenv AFNI_NOSPLASH          YES
setenv AFNI_SPLASH_MELT       NO
setenv AFNI_ENVIRON_WARNINGS  NO
setenv AFNI_COMPRESSOR        NONE

# -------------------------------------------------------------------

# ------------------- default/initialized variables -----------------

set ulay = ""                    # inp underlay 
set olay = ""                    # inp overlay  
set see_olay = "+"               # turn *off* olay disp

set ftype = PNG                  # output file format

set impref  = "image_file"       # prefix of output; other things added
set odir    = "."                # default output directory is "here"

set my_cbar = "Plasma"           # colorbar, if necessary
set bufac   = "2"                # blowup factor
set frange  = "0"                # either this (autorange), or user puts in
set USER_FRANGE = 0              # internal toggle only
set thrnew  = "0."               # threshold value (zero default)
set thrflag = "*"                # threshold flags

set crossh = "OFF"               # crosshairs OFF by default
set opac   =  6                  # opacity of overlay
set delsag = -1                  # (int) slice space in sag orient
set delaxi = -1                  # (int) slice space in axi orient
set delcor = -1                  # (int) slice space in cor orient
set mx     =  3                  # 'x' dim of montage
set my     =  3                  # 'y' dim of montage

# for SET_PBAR_ALL
set pbar_sign = "-"              # + is pos. only, - is both (switch for both)
set ncolors   = "99"             # default number of colors
set topval    = 1                # upper scale value in colorbar

# for SET_FUNC_ALPHA
set alpha_par    = "Off"         # 
set alpha_floor  = ""            # 
set alpha_edgize_no = 0          # =1 -> SETENV AFNI_EDGIZE_OVERLAY NO

set do_quit = "QUITT"            # quick-quit; not subject to change

# -------------------------------------------------------------------
# ----------------------------- ugh ---------------------------------

# needed to deal with orientation permutability : AIL, LAI, PSR, etc.

set listori = ( 'R' 'L' 'A' 'P' 'I' 'S' )
set listind = (  1   1   2   2   3   3  )

# -------------------------------------------------------------------
# ------------------- process options, a la rr ----------------------

if ( $#argv == 0 ) goto SHOW_HELP

set ac = 1
while ( $ac <= $#argv )
    # terminal options
    if ( ("$argv[$ac]" == "-h" ) || ("$argv[$ac]" == "-help" )) then
        goto SHOW_HELP
    endif
    if ( "$argv[$ac]" == "-ver" ) then
        goto SHOW_VERSION
    endif

    # required
    if ( "$argv[$ac]" == "-ulay" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set ulay = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-olay" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set olay = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-olay_off" ) then
        set see_olay = "-"

    else if ( "$argv[$ac]" == "-prefix" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set impref = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-outdir" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set odir = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-cbar" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set my_cbar = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-blowup" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set bufac = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-set_xhairs" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set crossh = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-opacity" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set opac = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-del_slice_sag" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set delsag = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-del_slice_axi" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set delaxi = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-del_slice_cor" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set delcor = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-thr_olay" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set thrnew = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-thrflag" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set thrflag = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-func_range" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set frange = "$argv[$ac]"
        set USER_FRANGE = 1
        
    else if ( "$argv[$ac]" == "-func_range_perc" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set frange_perc = "$argv[$ac]"
        set USER_FRANGE = 2


    else if ( "$argv[$ac]" == "-pbar_posonly" ) then
        set pbar_sign = "+"
        
    else if ( "$argv[$ac]" == "-cbar_ncolors" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set ncolors = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-cbar_topval" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set topval = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-jpg" ) then
        set ftype = "JPEG"
        
    else if ( "$argv[$ac]" == "-montx" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set mx = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-monty" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set my = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-alpha_par" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set alpha_par = "$argv[$ac]"

    else if ( "$argv[$ac]" == "-alpha_floor" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        set alpha_floor = "$argv[$ac]"
        
    else if ( "$argv[$ac]" == "-alpha_edgize_no" ) then
        setenv AFNI_EDGIZE_OVERLAY NO

    else if ( "$argv[$ac]" == "-zerocolor" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        setenv AFNI_IMAGE_ZEROCOLOR  "$argv[$ac]"

    # AFNI_IMAGE_LABEL_MODE      = 1     // draw labels in upper left of Image windows
    # AFNI_IMAGE_LABEL_SIZE      = 2     // size of labels in Image windows
    # AFNI_IMAGE_LABEL_COLOR     = white // color of labels in Image windows
    # AFNI_IMAGE_LABEL_SETBACK   = 0.01  // distance from edges for labels
    else if ( "$argv[$ac]" == "-label_mode" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        setenv AFNI_IMAGE_LABEL_MODE  "$argv[$ac]"

    else if ( "$argv[$ac]" == "-label_size" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        setenv AFNI_IMAGE_LABEL_SIZE  "$argv[$ac]"

    else if ( "$argv[$ac]" == "-label_color" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        setenv AFNI_IMAGE_LABEL_COLOR  "$argv[$ac]"

    else if ( "$argv[$ac]" == "-label_setback" ) then
        if ( $ac >= $#argv ) goto FAIL_MISSING_ARG
        @ ac += 1
        setenv AFNI_IMAGE_LABEL_SETBACK  "$argv[$ac]"

   else
      echo "** unexpected option #$ac = '$argv[$ac]'"
      exit 2

   endif
   @ ac += 1
end

# -------------------------------------------------------------------
# ------------------ check about auxiliary progs --------------------

# from Bob's @snapshot* progs

set nerr = 0
set errm = "** ERROR:"

# (Ubuntu hassle ->) not using: pamcomp
set plist = ( Xvfb djpeg cjpeg pnmcat pbmtext  \
                pamstretch pbmtopgm )
foreach pppp ( $plist )
  set wwww = `which $pppp`
  if( $status != 0 )then
    @ nerr++
    set errm = "$errm $pppp"
  endif
end
if( $nerr > 0 )then
  echo "$errm -- not found in path -- program fails"
  goto BAD_EXIT
endif

# -------------------------------------------------------------------
# -------------------------- check INPUTS ---------------------------

echo "\n------------------ start of optionizing ------------------\n" 

# need more cowbell
if ( ($ulay == '') ) then
    echo "** ERROR: need to have an underlay, at least!!"
    goto BAD_EXIT
endif

# need more cowbell
if (( "$olay" == "" ) && ( "$see_olay" == "+" )) then
    echo "** ERROR: you did NOT provide and overlay, and yet"
    echo "          you did NOT turn off the overlay with '-olay_off'"
    echo "      --> so I am confused and exiting." 
    goto BAD_EXIT
endif

# check blowup factor
set check_list = ( `seq 1 1 4` )
set innit = 0
foreach i ( $check_list )
    if ( $bufac == ${check_list[${i}]} ) then
        echo "++ Using blowup factor: $bufac"
        set innit = 1
        break
    endif
end
if ( $innit == 0 ) then
    echo "+* BAD blowup factor:  $bufac!"
    echo "   ---> needs to be (exactly) one of:  $check_list\n\n"
    goto BAD_EXIT
endif

# check opacity
set check_list = ( `seq 1 1 9` )
set innit = 0
foreach i ( $check_list )
    if ( $opac == ${check_list[${i}]} ) then
        echo "++ Using opacity:  $opac"
        set innit = 1
        break
    endif
end
if ( $innit == 0 ) then
    echo "+* BAD opacity:  $opac!"
    echo "   ---> needs to be (exactly) one of:  $check_list\n\n"
    goto BAD_EXIT
endif

# IF we don't want to see the olay, then set it to be the ulay, for
# later simplicity with the automontaging
if ( "$see_olay" == "-" ) then
    echo "++ No overlay viewing"
    set olay = ""
    # a silly thing, in case we don't want to view olay
    set lolay = "$ulay" 
else
    set lolay = "$olay"

    # set frange, if user hasn't, based on data:
    if ( $USER_FRANGE == 0 ) then
        ### -> this is 'old' method
        #set mval    = `3dBrickStat -max $olay`
        #set cval    = `3dClipLevel $olay`
        #set tval    = `ccalc "min($cval,$mval/4)"`
        #set frange  = `ccalc "max(3*$cval,0.666*$mval)"`

        set vv = `3dBrickStat -percentile 98 1 98 $olay`
        set frange = $vv[2]

        set maxolay = `3dinfo -max $olay`
        set minolay = `3dinfo -min $olay`
        echo "++ No user-entered function range ('-frange ...') value."
        echo "   --> Ergo, calculating based on 98th %ile, getting: $frange"
        echo "   ----> ... noting that range of values is: [$minolay, $maxolay]"
    else if ( $USER_FRANGE == 2 ) then
        set vv = `3dBrickStat -percentile $frange_perc 1 $frange_perc $olay`
        set frange = $vv[2]
        set maxolay = `3dinfo -max $olay`
        set minolay = `3dinfo -min $olay`
        echo "++ User-entered percentile value (${frange_perc}%)"
        echo "   --> Ergo, calculating FUNC_RANGE, getting: $frange"
        echo "   ----> ... noting that range of values is: [$minolay, $maxolay]"
    endif
endif

# Calculate numbers of slices in each direction, if not given by user.
# Take number of mont wins plus a couple to calculate.
@  Nwin  = $mx * $my
set Dim  = `3dinfo -n4 $ulay`
# silly stuff to deal with orientation
set ori  = `3dinfo -orient $ulay`
set ori0 = `echo $ori | awk '{print substr($0,1,1)}'`
set ori1 = `echo $ori | awk '{print substr($0,2,1)}'`
set ori2 = `echo $ori | awk '{print substr($0,3,1)}'`
set all_ori = ( $ori0 $ori1 $ori2 )
set order = ()
foreach oo ( $all_ori )
    foreach i ( `seq 1 1 ${#listori}` )
        if ( $oo == "$listori[${i}]" ) then
            set order = ( $order ${listind[$i]} )
            break
        endif
    end
end
echo "++ Cryptic info: $ori -> $all_ori -> $order"
echo "++ Dimensions (xyzt): $Dim"
if ( $delsag <= 0 ) then
    @ delsag = ${Dim[${order[1]}]} / ( $Nwin + 2 ) 
    # reset to 1 if rounding to 0 somehow
    if ( $delsag <= 0 ) then
        set delsag = 1
    endif
    echo "++ Every '$delsag' sagittal slice will be shown."
endif
if ( $delcor <= 0 ) then
    @ delcor = ${Dim[${order[2]}]} / ( $Nwin + 2 )
    # reset to 1 if rounding to 0 somehow
    if ( $delcor <= 0 ) then
        set delcor = 1
    endif
    echo "++ Every '$delcor' coronal slice will be shown."
endif
if ( $delaxi <= 0 ) then
    @ delaxi = ${Dim[${order[3]}]} / ( $Nwin + 2 )
    # reset to 1 if rounding to 0 somehow
    if ( $delaxi <= 0 ) then
        set delaxi = 1
    endif
    echo "++ Every '$delaxi' axial slice will be shown."
endif

echo "\n------------------- end of optionizing -------------------\n" 

# -------------------------------------------------------------------
# ------------------------- Make virtual frame ----------------------

# start the X virtual frame buffer on display #xdisplay, a la bob

set ranval = `count -dig 1 1 999999 R1`

if( $?xdisplay == 0 )then
  set killX     = 1
  set ntry      = 1
  set Xnotfound = 1
  while( $Xnotfound )
    set xdisplay = `count -dig 1 3 999 R1`
    if( -e /tmp/.X${xdisplay}-lock ) continue
    echo " -- trying to start Xvfb :${xdisplay}"
    Xvfb :${xdisplay} -screen 0 1024x768x24 &
    sleep 1
    jobs > zzerm.$ranval.txt
    grep -q Xvfb zzerm.$ranval.txt
    set Xnotfound = $status
    \rm -f zzerm.$ranval.txt
    if( $Xnotfound == 0 )break ;
    @ ntry++
    if( $ntry > 99 )then
      echo "** ERROR: can't start Xvfb -- exiting"
      goto BAD_EXIT
    endif
  end
endif

setenv DISPLAY :${xdisplay}

# -------------------------------------------------------------------
# ---------------- The actual, driven command! ----------------------

afni -noplugins -no_detach                                         \
     -com "SWITCH_UNDERLAY ${ulay}"                                \
     -com "SWITCH_OVERLAY  ${lolay}"                               \
     -com "SET_PBAR_ALL ${pbar_sign}${ncolors} $topval $my_cbar"   \
     -com "SET_FUNC_RANGE ${frange}"                               \
     -com "SET_THRESHNEW $thrnew $thrflag"                         \
     -com "SEE_OVERLAY ${see_olay}"                                \
     -com "SET_FUNC_ALPHA $alpha_par $alpha_floor"                 \
     -com "SET_XHAIRS  ${crossh}"                                  \
     -com "OPEN_WINDOW sagittalimage opacity=${opac} mont=${mx}x${my}:${delsag}"   \
     -com "OPEN_WINDOW coronalimage  opacity=${opac} mont=${mx}x${my}:${delcor}"   \
     -com "OPEN_WINDOW axialimage    opacity=${opac} mont=${mx}x${my}:${delaxi}"   \
     -com "SAVE_${ftype} sagittalimage ${odir}/${impref}.sag blowup=${bufac}" \
     -com "SAVE_${ftype} coronalimage  ${odir}/${impref}.cor blowup=${bufac}" \
     -com "SAVE_${ftype} axialimage    ${odir}/${impref}.axi blowup=${bufac}" \
     -com "${do_quit}"                                                \
     "${ulay}" "${olay}"

# A la bob:  stop Xvfb if we started it ourselves
if( $?killX ) kill %1

goto GOOD_EXIT

# ===========================================================================

# various FAIL condition labels follow

SHOW_VERSION:

    echo "\n\n++ Version: $ver\n\n"

    goto GOOD_EXIT

# ---------------------------------- 

SHOW_HELP:

cat <<EOF

 ++ help file a-comin' (someday).

    minimal usage:  

        $ viewer_afni.tcsh -ulay FILE_ULAY -olay FILE_OLAY
 
EOF

    goto GOOD_EXIT

# ---------------------------------- 

FAIL_MISSING_ARG:
   echo "** missing parameter for option $argv[$ac]"
   goto BAD_EXIT

# ---------------------------------- 

BAD_EXIT:
    exit 1

# ---------------------------------- 

GOOD_EXIT:
    exit 0

