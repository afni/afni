
# ----------------------------------------------------------------------
# set up a machine for building AFNI
# the last admin step : installing R
#
# this applies to Intel x86-64 (for R package)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# fail if this is an Intel CPU
if [[ `uname -m` != x86_64 ]] then
cat << EOF
** error: this script is meant for an Intel CPU system,
          but this one appears to be ARM

   Please run the installation instructions for the appropriate chip type.
EOF
   exit 1
else
   echo "++ Beginning R installation on this Intel system"
fi

# ----------------------------------------------------------------------
# install R - currently 4.3.1

curl -O https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.3.1-x86_64.pkg
sudo installer -pkg  R-4.3.1-x86_64.pkg -target /


# ----------------------------------------------------------------------
# and reboot : sudo reboot
# ----------------------------------------------------------------------
echo ""
echo "  ... please reboot ..."
echo ""
