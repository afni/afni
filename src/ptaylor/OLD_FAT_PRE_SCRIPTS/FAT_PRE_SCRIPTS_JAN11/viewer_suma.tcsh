#!/bin/tcsh

# GUI settings for simplicity
#setenv AFNI_IMAGE_LABEL_MODE  5
#setenv AFNI_IMAGE_LABEL_SIZE  2



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
  exit 1
endif

# --------------------------------------------------------------------

set odir    = "IMAGES"
set impref  = "image_file"
set my_cbar = "Plasma"
set bufac   = "2"
set frange  = "0.333"
set thrnew  = "0."

set opac   = 4
set delsag = 30
set delaxi = 30
set delcor = 30

set pnum = 4
set lensleep = "500ms"
set imviews = ( 'ctrl+up' 'ctrl+shift+up' 'ctrl+left' 'ctrl+right' )
set imkeys  = ( 'A'       'C'             'L'         'R'  )
set do_quit = "kill_suma"

set my_suma_cmd = "-vol DWI.nii"   # whatever the user wants to look
                                   # at: vol, tract, i_gii, etc.

suma -npb $pnum                                           \
    -onestate -dev                                        \
    $my_suma_cmd                                   &   

# snapshots from different views
foreach i ( `seq 1 1 4` )

    set upsamp = ( )
    foreach j ( `seq 1 1 $bufac` )
        set upsamp = ( $upsamp '-key alt+r' )
    end
    if ( $1 > 1 ) then
        set upsamp = ( )
    endif
    echo "\n\n\n i=$i ---->  $upsamp" 

    DriveSuma                                                       \
        -npb $pnum                                                  \
        -com viewer_cont                                            \
            -autorecord "${odir}"/"${impref}.${imkeys[$i]}.jpg"     \
            $upsamp '-key r' \
            -key "${imviews[$i]}"                                   \
        -com "sleep ${lensleep}"   #                                 \
#        -com viewer_cont  -key 'ctrl+r'
end

if ( "${do_quit}" != "" ) then
    echo "++ Bye!"
    DriveSuma                                                  \
        -npb $pnum                                             \
        -com "${do_quit}"
endif






echo "\n\n++ Done saving images (hopefully successfully).\n\n"

exit 0
