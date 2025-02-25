
# ----------------------------------------------------------------------
# set up a machine for building AFNI
# (it should only have 1 or 2 early sudo's, for brew and xquartz)
#
# this prepares for zsh, bash or tcsh login shells
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
   echo "++ Beginning homebrew installation on this ARM system"
fi

# ----------------------------------------------------------------------
# install homebrew   - and python (3.11 currently), X11, netpbm, cmake
# (command uses bash syntax, not just in script)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# prep multiple shells for brew (.bashrc seems safer for bash)
(echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"') >> $HOME/.zprofile
(echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv bash)"') >> $HOME/.bashrc
(echo; echo 'eval `/opt/homebrew/bin/brew shellenv tcsh`') >> $HOME/.login
eval "$(/opt/homebrew/bin/brew shellenv)"

brew analytics off
brew install python netpbm cmake gfortran

echo ""
echo "== installing XQuartz (needs sudo)"
echo ""
brew install --cask xquartz

# install build packages
brew install libpng jpeg expat freetype fontconfig openmotif  \
             libomp gsl glib pkg-config gcc libiconv autoconf \
             libxt mesa mesa-glu libxpm

# ----------------------------------------------------------------------
# put python in path and install matplotlib via pip (multiple shells)

export PATH=${PATH}:/opt/homebrew/opt/python/libexec/bin
echo 'export PATH=${PATH}:/opt/homebrew/opt/python/libexec/bin' >> ~/.zshrc
echo 'export PATH=${PATH}:/opt/homebrew/opt/python/libexec/bin' >> ~/.bashrc
echo 'setenv PATH ${PATH}:/opt/homebrew/opt/python/libexec/bin' >> ~/.login

# go through conda:
# pip install matplotlib

# ----------------------------------------------------------------------
# continue with R in the next script (another sudo)
