
# ----------------------------------------------------------------------
# set up a machine for building AFNI
# the last admin step : installing R
#
# this applies to ARM (for R package)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# fail if this is an Intel CPU
if [[ `uname -m` == x86_64 ]] then
cat << EOF
** error: this script is meant for an ARM CPU system,
          but this one appears to be Intel

   Please run the installation instructions for the appropriate chip type.
EOF
   exit 1
else
   echo "++ Beginning R installation on this ARM system"
fi


# ----------------------------------------------------------------------
# install R - currently 4.3.1

curl -O https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.3.1-arm64.pkg
sudo installer -pkg R-4.3.1-arm64.pkg -target /


# ----------------------------------------------------------------------
# and reboot : sudo reboot
# ----------------------------------------------------------------------
echo ""
echo "  ... please reboot ..."
echo ""
