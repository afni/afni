#!/bin/tcsh

# [PT: Apr 21, 2019] Updated to get to R version 3.5.*

# Check your ubuntu system name for its codename (e.g., 'trusty',
# 'utopic', 'vivid', 'wily') with the following command: 
set ubuntu_code = `awk -F= '/CODENAME/ {print $2}' /etc/lsb-release`
printf "\n\n++ Linux codename is: ${ubuntu_code}\n\n"

# Add the following default repository name to your sources list (if
# your system's `codename` is different than 'trusty', then use that in
# the place of 'trusty' in the following command):
printf "++ Adding R-cran mirror to repository list.\n"
#echo "deb http://cran.cnr.berkeley.edu/bin/linux/ubuntu/ ${ubuntu_code}/" \
#       | sudo tee --append /etc/apt/sources.list > /dev/null
echo "deb https://cloud.r-project.org/bin/linux/ubuntu/ ${ubuntu_code}-cran35/" \
       | sudo tee --append /etc/apt/sources.list > /dev/null

apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# NOTE: It is possible to select a different repository from the full
# R-cran mirror list <https://cran.r-project.org/mirrors.html>
# if necessary. For example, you might pick one that is geographically
# closer or within country for speed of downloads.

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
