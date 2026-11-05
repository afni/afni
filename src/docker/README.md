# AFNI Docker V1 (`docker_v1`)

This repository serves as a streamlined, direct approach to building or launching containerized versions of **AFNI (Analysis of Functional NeuroImages)**. It aims to reduce dependency conflicts and installation overhead, making it easier to deploy AFNI in a Docker on different operating systems (various Linux distributions and macOS) and on different CPU architectures (Intel/x86, Apple Silicon, ARM).  Windows is not yet supported.

## Repository Structure

* **`afni_docker_universal/`**  
  Contains the foundational `Dockerfile` configuration and setup scripts needed to assemble a universal AFNI container environment.
* **`launch_afni_docker.sh`**  
  A dedicated shell script wrapper engineered to automate volume mounting, user permission management, and GUI/X11 rendering parameters.

---

# Quick Links
* [Installation on macOS](#installation-on-macos)
* [Installation on Linux](#installation-on-linux)
* [Launching the AFNI Docker (any OS)](#launching-the-afni-docker-any-os)
* [Using the AFNI Docker (any OS)](#using-the-afni-docker-any-os)
* [Technical notes for macOS](#technical-notes-for-macos) 
* [Technical notes for Linux](#technical-notes-for-linux) 
* [Technical notes for Docker](#technical-notes-for-docker-any-os)
* [Technical notes to build locally (any OS)](#technical-notes-to-build-locally-any-os)

---

# Installation on macOS

These items should only need to be done once on your computer to setup the AFNI docker.

1. **Docker**: *The system used for running containers.*  
   Either download and install from the [Docker website](https://docs.docker.com/desktop/setup/install/mac-install/)
   (noting your Mac's architecture: Silicon or Intel),  
   or install using [Homebrew](https://brew.sh) by running:
   ```none
   brew install docker
   ```

2. **XQuartz**: *Required for interactive elements like the AFNI and  SUMA GUIs.*  
   Either download and install from the [XQuartz website](https://www.xquartz.org/),  
   or use Homebrew:
   ```none
   brew install --cask xquartz
   ```

3. **Launcher script**: *The all-in-one executable script that runs the AFNI Docker.*  
   Run the following command to download the script to your home directory:
   ```none
   cd
   curl -O https://raw.githubusercontent.com/afni/docker_v1/refs/heads/main/launch_afni_docker.sh
   ```

---

# Installation on Linux

These items should only need to be done once on your computer to setup the AFNI docker.

1. **Docker**: *The system used for running containers.*  
   Download and install from the [Docker website](https://docs.docker.com/desktop/setup/install/linux/)
   (noting your Linux flavor: Ubuntu, Fedora, RedHat, etc.),

3. **Launcher script**: *The all-in-one executable script that runs the AFNI Docker.*  
   Run the following command to download the script to your home directory:
   ```none
   cd
   curl -O https://raw.githubusercontent.com/afni/docker_v1/refs/heads/main/launch_afni_docker.sh
   ```

**NB:** If you are using Windows Subsystem Linux (WSL), you will also need to install an X-server, 
like [vcXsrv](https://sourceforge.net/projects/vcxsrv/).

---

# Launching the AFNI Docker (any OS)

To launch your **current** version of AFNI via docker, simply run the launcher script:
```none
bash launch_afni_docker.sh -run
```

The first time it is run, the script will check for the latest version of AFNI and download it if necessary.
If you already have a local AFNI Docker, that same version will be used.

To launch the **latest** distributed version of AFNI, run:
```none
bash launch_afni_docker.sh -run -tag latest
```

See [**Using the AFNI Docker**](#using-the-afni-docker-mac-linux), below, about navigating file structure, exiting the container, and more.

---

# Using the AFNI Docker (any OS)

1. Running `bash launch_afni_docker.sh` with no arguments will configure
   and launch the current AFNI docker. 
   * See `bash launch_afni_docker.sh -help` for more info.

2. The afni docker will be launched with the current user's home 
   directory mounted to `/home/external` in the docker container. 
   This allows you to access your files from within the docker 
   container. The Docker program may give you a warning about this, 
   but it is safe to ignore.

3. When using the docker, any new datasets are still simply owned by
   the user and exist on the host file system after existing docker.

4. The user can have a AFNI environment settings file on their
   computer's home directory (`~/.afnirc`), which they can edit as
   usual to control AFNI behavior within the docker environment. This
   file exists outside the docker container, so changes remain even after
   the docker is closed/exited.
   
5. To exit the docker container, type 'exit' or 'Ctrl+d' **TWICE** in 
   the terminal (once to get out user shell, and once to exit the 
   docker container).  If you only type 'exit' or 'Ctrl+d' **ONCE**, 
   you will be returned to the root shell in the docker container.

---

# Technical notes for macOS

There are some settings on XQuartz that may prevent the AFNI/SUMA GUI from 
displaying. The `launch_afni_docker.sh` script will attempt to set the correct 
settings. There will be prompt to allow the script to set these settings. You 
can choose to allow or deny this. The warning messages will include 
instructions on how to manually set these settings if you choose to deny the 
script permission to set them.

## All XQuartz settings: tl;dr
To summarize the macOS notes below, the following commands will **set** the 
XQuartz security settings and **enable** indirect GLX rendering:
```bash
   defaults write org.xquartz.X11.plist nolisten_tcp -bool false
   defaults write org.xquartz.X11 no_auth -boolean true
   defaults write org.xquartz.X11 enable_iglx -bool true
```
Then restart XQuartz and the terminal for the changes to take effect.

To **restore** the default settings, run:
```bash
   defaults write org.xquartz.X11.plist nolisten_tcp -bool true
   defaults write org.xquartz.X11 no_auth -boolean false
   defaults write org.xquartz.X11 enable_iglx -bool false
```
Then restart XQuartz and the terminal for the changes to take effect.

## XQuartz settings: security

There are two security settings that need to be set correctly for the 
AFNI and SUMA GUIs to display: 
1. `Allow connections from network clients`
2. `Authenticate connections`

### Managing XQuartz security settings from the GUI

1. Open XQuartz and go to `XQuartz` > `Preferences` > `Security`.
2. Ensure that the option `Allow connections from network clients` is **CHECKED**.
3. Ensure that the option `Authenticate connections` is **UNCHECKED**.
4. Restart XQuartz for the changes to take effect.

*(Reverse the above steps if you want to restore the default settings).*

### Managing XQuartz security settings from the command line
To **allow** all connections from unauthenticated network clients, run:
```bash        
   defaults write org.xquartz.X11.plist nolisten_tcp -bool true
   defaults write org.xquartz.X11 no_auth -boolean false
```
Then restart XQuartz and the terminal for the changes to take effect.

To **restore** the default settings, run:
```bash
   defaults write org.xquartz.X11.plist nolisten_tcp -bool false
   defaults write org.xquartz.X11 no_auth -boolean true
```
Then restart XQuartz and the terminal for the changes to take effect.

## Xquartz settings: indirect GLX

This setting is needed to enable the SUMA GUI to run properly.

To **enable** indirect GLX rendering, run:
```bash
   defaults write org.xquartz.X11 enable_iglx -bool true
```
Then restart XQuartz and the terminal for the changes to take effect.

To **restore** the default settings, run:
```bash
   defaults write org.xquartz.X11 enable_iglx -bool false
```
Then restart XQuartz and the terminal for the changes to take effect.

---

# Technical notes for Linux

1. On Linux, the user needs to be in the `docker` group to run this script. 
   If you are not in the docker group, the script will exit with an error. 
   You need administrative privileges to create the docker group and add 
   yourself to the group. 

2. You can create the docker group with the following command: 
   `sudo groupadd docker`. 

3. You can add yourself to the docker group with the following command: 
   `sudo usermod -aG docker $USER`.

   You need to restart your computer or log out and log back in for 
   the group changes to take effect. Running the script with `sudo` 
   will not fix this issue. 

4. On some Linux variants, the Docker Desktop may block X11 
   forwarding. If this happens, you can try the `-display` option 
   to set a different display environment variable. However, this 
   may not work and the using Docker engine instead of the the 
   Docker Desktop may be the only way to fix this issue. Please 
   see the Docker documentation for more information.

5. `launch_afni_docker.sh` will set `xhost +SI:localuser:$USER` to allow 
   the docker container to connect to the X11 server. If you want to 
   undo this, you can run `xhost -SI:localuser:$USER` after exiting the 
   docker container.

---

# Technical notes for Docker (any OS)

1. You can find the AFNI Docker Hub repository
   [here](https://hub.docker.com/repository/docker/discoraj/afni_docker_universal/general).
   
3. The docker container will be launched with the current user's 
   UID and GID. This allows you to create and access files in your 
   home directory from within the docker container without 
   permission issues.

4. By default, the `launch_afni_docker.sh` will pull the latest version
   of the AFNI docker image from Docker Hub on first run or with the
   `-tag latest` option. Currently the Docker Hub repository is owned by
   Justin Rajendra (DiscoRaj) from the AFNI Group (SSCC at the NIH).

---

# Technical notes to build locally (any OS)

Most users will run the AFNI docker using the distributed launcher script,
as described [above](launching-the-afni-docker-any-os).

If you prefer to build the docker image directly using the localized source 
files under the afni_docker_universal directory, you can do the following.

1. Get the current GitHub repository for the AFNI docker, with:
   ```bash
   git clone https://github.com/afni/docker_v1.git
   cd docker_v1/afni_docker_universal
   docker build -t afni_universal .
   ```

2. Then start the container by running ...
   * ... this on macOS:
     
     ```bash
     docker run -ti --rm                          \
         -u     root                              \
         -v     "${HOME}:/home/external"          \
         -v     /tmp/.X11-unix:/tmp/.X11-unix     \
         --env  DISPLAY="host.docker.internal:0"  \
         --env  USERID="`id -u`"                  \
         --env  GRPID="`id -g`"                   \
         --env  GRPNAME="`id -gn`"                \
         --env  USERNAME="`id -u -n`"             \
         afni_universal
     ```
   * ... or this on Linux:
     
      ```bash
      docker run -ti --rm                       \
          -u     root                           \
          -v     "${HOME}:/home/external"       \
          -v     /tmp/.X11-unix:/tmp/.X11-unix  \
          --env  DISPLAY="${DISPLAY}"           \
          --env  USERID="`id -u`"               \
          --env  GRPID="`id -g`"                \
          --env  GRPNAME="`id -gn`"             \
          --env  USERNAME="`id -u -n`"          \
          afni_universal
      ```

---
