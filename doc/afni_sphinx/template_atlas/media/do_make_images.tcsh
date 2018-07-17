#!/bin/tcsh

set here = $PWD
set odir = $here/SSW_IMAGES

set allfile = `\ls *_SSW.nii.gz`

\mkdir -p $odir

set idx = 0 

foreach ff ( $allfile )

    # base
    set bb = `3dinfo -prefix_noext "$ff"`
    # nvol
    set NN = `3dinfo -nvi "$ff"`
    set nv = `3dinfo -nv "$ff"`

    foreach ii ( `seq 0 1 $NN` )

        set iii = `printf "%03d" $ii`

        if ( $ii > 2 ) then
            set UMIN = "0"
            set UMAX = "1"
        else
            set UMIN = "2%"
            set UMAX = "98%"
        endif

        @chauffeur_afni                     \
            -ulay    "${ff}[$ii]"           \
            -ulay_range "$UMIN" "$UMAX"     \
            -prefix  $odir/${bb}_$iii       \
            -montx 1 -monty 1               \
            -set_dicom_xyz 2 18 18          \
            -delta_slices 25 25 25          \
            -set_xhairs OFF                 \
            -label_mode 1 -label_size 3     \
            -do_clean  

    end

    set rgb = ( 93 220 89 )
    if ( $idx > 0 ) then
        set rgb = ( 204 0 153 )
    endif

    # should still be ordered correctly when expanded...
    imcat                            \
        -echo_edu                    \
        -gap 5                       \
        -gap_col $rgb                \
        -nx $nv                      \
        -ny 3                        \
        -prefix $odir/ALL_${bb}.jpg  \
        $odir/${bb}*sag* $odir/${bb}*cor* $odir/${bb}*axi*
        

    @ idx = $idx + 1
end

echo "++ DONE!"

echo 0

