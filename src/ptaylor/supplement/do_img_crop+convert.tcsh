#!/bin/tcsh

# Loop over images output by LibreOffice Impress, and do so work on
# each one:
# + trim outer parts
# + pad slightly
# + convert to different image formats
# 
# auth: PA Taylor (SSCC, NIMH, NIH)
# --------------------------------------------------------------------------
# IMAGEMAGICK
# 
# For more about ImageMagick-convert's cropping, see the wonderful page:
#     https://legacy.imagemagick.org/Usage/crop/
# 
# ... and here for more general options:
#     https://imagemagick.org/script/command-line-options.php
#
# --------------------------------------------------------------------------

set base_pres = 

# get all images---annoyingly, they have spaces
set all_img = ( ${base_pres}*.png )

set all_type = ( png tif jpg )

foreach ii ( `seq 1 1 ${#all_img}` )
    set iii   = `printf "%03d" $ii`
    set img   = "${all_img[$ii]}"

    set obase = FIG_${iii}

    echo $img
    foreach type ( ${all_type} )
        convert                                            \
            "${img}"                                       \
            -quality 100                                   \
            -trim                                          \
            -matte -bordercolor white -border 10           \
            ${obase}.${type}
    end
end
