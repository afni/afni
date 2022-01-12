# Quick build setup script 1/3.
# From bash shell:
#   sudo bash OS_notes.linux_ubuntu_20_64_a_admin.txt 2>&1 | tee o.ubu_20_a.txt
# From tcsh shell:
#   sudo bash OS_notes.linux_ubuntu_20_64_a_admin.txt |& tee o.ubu_20_a.txt

sudo add-apt-repository universe

# for R-package brms 
sudo add-apt-repository -y "ppa:marutter/rrutter4.0"
sudo add-apt-repository -y "ppa:marutter/c2d4u3.5"

sudo apt-get update

# main deps
sudo apt-get install -y tcsh xfonts-base libssl-dev       \
                        python-is-python3                 \
                        python3-matplotlib                \
                        gsl-bin netpbm gnome-tweak-tool   \
                        libjpeg62 xvfb xterm vim curl     \
                        gedit evince eog                  \
                        libglu1-mesa-dev libglw1-mesa     \
                        libxm4 build-essential            \
                        libcurl4-openssl-dev libxml2-dev  \
                        libgfortran-8-dev libgomp1        \
                        gnome-terminal nautilus           \
                        gnome-icon-theme-symbolic         \
                        firefox xfonts-100dpi             \
                        r-base-dev

# for R-package brms
sudo apt-get install -y libgdal-dev libopenblas-dev       \
                        libnode-dev libudunits2-dev       \
                        libgfortran4

# for package GSL
sudo ln -s \
        /usr/lib/x86_64-linux-gnu/libgsl.so.23            \
        /usr/lib/x86_64-linux-gnu/libgsl.so.19


echo "++ Done with sudo-required part"
