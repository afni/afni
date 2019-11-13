#!/bin/tcsh

# Script to help prepping for Bootcamp.  Script takes 1 argument:
# where to put the stuff.

# ver = 1.0
# ==========================================================================

set loc = $1

if ( $loc == "" ) then
    echo "\n\n"
    echo "** Need to provide a location for unpacking everything!"
    echo "\n\n"
    exit 1
endif

cd $loc

set all_binaries = ( macos_10.12_local.tgz  \
                     linux_ubuntu_16_64.tgz \
                     linux_openmp_64.tgz    \
                     linux_centos_7_64.tgz  \
                     linux_xorg7_64.tgz )

set all_tgz_demos = ( FATCAT_DEMO2.tgz      \
                      MACAQUE_DEMO.tgz  )

# Should be copy of set of atlases in standard abin/.
# If teaching to a more specialized group, maybe add other
# atlases (e.g., infant, macaque, etc.)
set all_atlases   = ( atlases_latest.tgz )

# --------------------------------------------------------------------

echo "\n\n++ ----------------- getting CD\n"
curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
echo "\n\n++ ----------------- opening CD\n"
tar -xf CD.tgz

# Yeah, doesn't need to be done separately, but I do.  
echo "\n\n++ ----------------- getting handouts\n"
afni_open -aw afni_handouts.tgz

foreach bb ( $all_binaries )
    echo "\n\n++ ----------------- getting binaries: $bb\n"
    curl -O https://afni.nimh.nih.gov/pub/dist/tgz/${bb}
end

foreach bb ( $all_tgz_demos )
    echo "\n\n++ ----------------- getting demos: $bb\n"
    curl -O https://afni.nimh.nih.gov/pub/dist/tgz/${bb}
end

foreach bb ( $all_atlases )
    echo "\n\n++ ----------------- getting atlases: $bb\n"
    curl -O https://afni.nimh.nih.gov/pub/dist/atlases/${bb}
end



cd -

echo "\n\n++ ----------------- "
echo "       Done happily.\n"

echo "\n\n++ (Do you want to copy TORTOISE files over now, too?)\n"

exit 0
