
# ----------------------------------------------------------------------
# set up a machine for building AFNI
# the last admin step : installing R
#
# this applies to ARM (for R package)
# ----------------------------------------------------------------------

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
