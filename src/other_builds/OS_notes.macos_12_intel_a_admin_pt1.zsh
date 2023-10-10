
# ----------------------------------------------------------------------
# set up a machine for building AFNI
#
# this assumes 'zsh' is the login shell
# this applies to Intel x86-64 (for R package)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# install homebrew   - and python (3.11 currently), X11, netpbm, cmake
# (command uses bash syntax, not just in script)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# prep multiple shells for brew (.bashrc seems safer for bash)
(echo; echo 'eval "$(/usr/local/bin/brew shellenv)"') >> $HOME/.zprofile
(echo; echo 'eval "$(/usr/local/bin/brew shellenv bash)"') >> $HOME/.bashrc
(echo; echo 'eval "$(/usr/local/bin/brew shellenv tcsh)"') >> $HOME/.login
eval "$(/usr/local/bin/brew shellenv)"

brew analytics off
brew install python netpbm cmake gfortran
brew install --cask xquartz

# install build packages
brew install libpng jpeg expat freetype fontconfig openmotif  \
             libomp gsl glib pkg-config gcc libiconv autoconf \
             libxt mesa mesa-glu libxpm

# ----------------------------------------------------------------------
# put python in path and install matplotlib via pip (multiple shells)

export PATH=${PATH}:/usr/local/opt/python/libexec/bin
echo 'export PATH=${PATH}:/usr/local/opt/python/libexec/bin' >> ~/.zshrc
echo 'export PATH=${PATH}:/usr/local/opt/python/libexec/bin' >> ~/.bashrc
echo 'setenv PATH ${PATH}:/usr/local/opt/python/libexec/bin' >> ~/.login
pip install matplotlib

# ----------------------------------------------------------------------
# install R - currently 4.3.1

curl -O https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.3.1-x86_64.pkg
sudo installer -pkg  R-4.3.1-x86_64.pkg -target /


# ----------------------------------------------------------------------
# and reboot : sudo reboot
# ----------------------------------------------------------------------
