#!/usr/bin/env tcsh

set ver = '2.0'
# [PT: Oct 17, 2019] 
# + include check for pre-existing R
# + now, if R exists, user will need to provide an option to continue
#
# ==========================================================================

set push_on = $1

echo ""
echo "++ Start modern R installer"
echo "++ Installer ver = ${ver}\n"

set HAVE_R   = `which R`
set STATUS_R = $status

if ( $STATUS_R ) then
    echo "++ No pre-existing R found.  Will continue on here."
    echo ""
else
    set MY_R = `R --version`
    echo "+* Warning! Existing R version detected:"
    echo ""
    echo "       ${MY_R[1-4]}"
    echo ""

    if ( "${push_on}" == "-overwrite" ) then
        echo "++ ... User electing to carry on, overwriting earlier R version."
        echo ""
    else
        echo "---------------------------------------------------------------"
        echo "** If you still want to carry on with this installation,"
        echo "   you MUST use the following option with this command:"
        echo "       -overwrite"
        echo "   If you have any questions, please ask on the AFNI Message"
        echo "   Board."
        echo "---------------------------------------------------------------"
        echo ""
        exit 1
    endif
endif

# ------------------------------

# Check your ubuntu system name for its codename (e.g., 'trusty',
# 'utopic', 'vivid', 'wily') with the following command: 
set ubuntu_code = `awk -F= '/CODENAME/ {print $2}' /etc/lsb-release`
printf "\n\n++ Linux codename is: ${ubuntu_code}\n\n"

# Add the following default repository name to your sources list (if
# your system's `codename` is different than 'trusty', then use that in
# the place of 'trusty' in the following command):
printf "++ Adding R-cran mirror to repository list.\n"
echo "deb https://cloud.r-project.org/bin/linux/ubuntu ${ubuntu_code}-cran35/" \
    | sudo tee --append /etc/apt/sources.list > /dev/null
       
#apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
#from: https://cran.r-project.org/bin/linux/ubuntu/#secure-apt
printf "++ Adding keys.\n"
apt-key adv                                                   \
    --keyserver keyserver.ubuntu.com                          \
    --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# Then run the following to get the latest R (the first command is
# included in case you already have installed an older R version; it is
# likely not a problem if that fails):
printf "++ Going to install new R now, removing any old one first.\n"
apt-get remove -y r-base r-base-core r-base-dev
apt-get update
apt-get install -y r-base-dev
apt-get -f install

# update any pre-existing R packages
echo '' > @update_rcran.R
echo 'print("++ Start updating R packages if necessary...")' >> @update_rcran.R
echo 'update.packages(installed.packages(priority="NA"), checkBuilt=TRUE, ask=FALSE)' >> @update_rcran.R
echo 'print("++ ... done updating R packages, if it were necessary.")' >> @update_rcran.R

Rscript @update_rcran.R

echo "++ DONE updating R.  Have installed this version:"
echo "--------------------------------------------------------------------"
R --version
echo "--------------------------------------------------------------------"
