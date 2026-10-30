#!/bin/bash

#################################################
## 07/2026 Justin Rajendra
## launch the afni docker container on macOS or Linux
## universal build

## get the current program name
prog="launch_afni_docker.sh"

## default docker image settings
do_run=0
show_cmd=0
base_img="discoraj/afni_docker_universal"
base_tag="latest"
dock_img=""
pull="missing"
disp=""
disp_macos="host.docker.internal"
disp_linux="$DISPLAY" ; disp_linux_help='$DISPLAY' # help file shows literal

#################################################
## help!
show_help() {
cat << EOF

   ----------------------------------------------------------------------------
   $prog ~1~

           Helper script to launch the afni docker on macOS and most Linux 
           variants. This script does not need any arguments.  

           REQUIREMENTS: ~2~

           1. This MUST be run on macOS or a flavor of Linux
              (Windows is not yet supported, besides WSL).

           2. Docker MUST be installed and running.

           3. On macOS, Xquartz MUST be installed and running.

           The script will check for all of the above.

           NOTES: ~2~

           1. The afni docker will be launched with the current user's home 
              directory mounted to /home/external in the docker container. 
              This allows you to access your files from within the docker 
              container. The Docker program may give you a warning about this,  
              but it is safe to ignore.

           2. The docker container will be launched with the current user's 
              UID and GID. This allows you to create and access files in your 
              home directory from within the docker container without 
              permission issues.

           3. On some Linux variants, the Docker Desktop may block X11 
              forwarding. If this happens, you can try the -display option 
              to set a different display environment variable. However, this 
              may not work and the using Docker engine instead of the the 
              Docker Desktop may be the only way to fix this issue. Please 
              see the Docker documentation for more information.

           4. On Linux, the user needs to be in the docker group to run 
              this script. If you are not in the docker group, the script 
              will exit with an error. You need administrative privileges
              to create the docker group and add yourself to the group. 
              You can create the docker group with the following command: 
                 sudo groupadd docker
              You can add yourself to the docker group with the following 
              command: 
                 sudo usermod -aG docker \$USER
              You need to restart your computer or log out and log back in for 
              the group changes to take effect. Running the script with sudo 
              will not fix this issue. 

           5. To exit the docker container, type 'exit' or 'Ctrl+d' TWICE in 
              the terminal.  Once to get out user shell and once to exit the 
              docker container.  If you only type 'exit' or 'Ctrl+d' ONCE, 
              you will be returned to the root shell in the docker container.

   -----------------------------------------------------------------------------
   Options: ~1~

      -run            : flag to launch the AFNI docker

      -tag TAG        : Specify which version of AFNI to run, corresponding
                        to any of the available Docker Hub tagged versions.  
                        The value of TAG can be the full AFNI version
                        string or simple version number, like:

                           AFNI_26.0.01
                           26.0.01

                        or TAG can be a keyword:

                           latest   : pull new docker image to get latest 
                                      version of AFNI that is distributed, 
                                      (overwrites any previous AFNI docker
                                      images)

                        If this option is not used, then the AFNI docker
                        version present on the OS will be used (if none exists,
                        the latest version will be pulled from Docker Hub)

      -display "DISP" : Use a different display environment variable for
                        testing purposes.
                        Please surround text in double quotes " ".
                        Default values for DISP are:

                           for macOS : "${disp_macos}"
                           for linux : "${disp_linux_help}"

      -image IMAGE    : The base name of the docker hub image.  Changing
                        this would launch a different docker image, so it
                        does not ever need to be used to keep using the AFNI
                        docker
                        (def : ${base_img})

      -show_cmd       : Display the 'docker run ...' command when launching

      -help           : Show this help (also shown if no options provided)

   -----------------------------------------------------------------------------
   Examples: ~1~

      1. Just launch the afni docker:

           bash ${prog} -run

      2. Launch the afni docker and update the local image:

           bash ${prog} -tag latest

      3. Launch the afni docker and see what the run command is:

           bash ${prog} -run -show_cmd

   -----------------------------------------------------------------------------
   Justin Rajendra 07/2026

EOF
exit 0
}

#################################################
## get arguments or show help

# no args -> show help and exit

if [ "$#" = "0" ]; then
    show_help
fi

# proc args

argv=("")
for arg in "$@"; do
    argv+=("$arg")
done

narg=1 ; amax=$#
while [ $narg -le $amax ]; do
    if [ "${argv[$narg]}" = "-run" ]; then
        do_run=1
    elif [ "${argv[$narg]}" = "-show_cmd" ]; then
        show_cmd=1
    elif [ "${argv[$narg]}" = "-image" ]; then
        ((narg++))
        base_img="${argv[$narg]}"
    elif [ "${argv[$narg]}" = "-tag" ]; then
        ((narg++))
        base_tag="${argv[$narg]}"
    elif [ "${argv[$narg]}" = "-display" ]; then
        ((narg++))
        disp="${argv[$narg]}"
    elif [ "${argv[$narg]}" = "-help" ] || \
         [ "${argv[$narg]}" = "-h" ]; then
        show_help
    else
        show_help
    fi
    ((narg++))
done

#################################################
## did user add -run?

if [[ ${do_run} = 0 ]]; then
    echo "** User entered options but no '-run' flag." 
    echo "   Please add '-run' to launch docker, or use '-help' to see help"
    exit 1
fi

#################################################
## build docker image name

dock_img="${base_img}:${base_tag}"

if [[ "${base_tag}" == "latest" ]]; then
    pull="always"
fi

#################################################
## detect os type and set display variable

if [[ "$OSTYPE" == "darwin"* ]]; then
    # echo "Running on macOS"
    os="macos"
    ## set default display variable if not set
    # add display number after XQuartz starts
    if [[ -z "$disp" ]]; then
        disp="${disp_macos}"
    fi
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # echo "Running on Linux"
    os="linux"
    ## set default display variable if not set
    if [[ -z "$disp" ]]; then
        disp="${disp_linux}"
    fi
else
    echo ; 
    echo "** ERROR: You don't seem to be running on a supported OS."
    echo
    echo "   If you are, there may be a problem with this program..."
    echo "   Please post the error to: https://discuss.afni.nimh.nih.gov"
    echo
    exit 1
fi

#################################################
## check to see if things exist / are installed

## check for docker
docker_bin=`command -v docker`
if [[ -z "$docker_bin" ]]; then
    echo
    echo "** ERROR: Docker not found."
    echo
    echo "   Is Docker installed?"
    if [[ "$os" == "macos" ]]; then
        echo "   Installation instructions for macOS can be found at:"
        echo "   https://docs.docker.com/desktop/setup/install/mac-install/"
    elif [[ "$os" == "linux" ]]; then
        echo "   Installation instructions for Linux can be found at:"
        echo "   https://docs.docker.com/desktop/setup/install/linux/"
    else
        echo "   Unknown OS (?); see generic docker engine instructions:"
        echo "   https://docs.docker.com/engine/"
    fi
    echo
    exit 1
fi

#################################################
## checks for macOS
if [[ "$os" == "macos" ]]; then

    #################################################
    ## check for installed XQuartz
    xquartz_bin=`command -v Xquartz || command -v XQuartz`
    if [[ -z "$xquartz_bin" ]]; then
        ## Fallback in case it is installed but not in PATH
        if [[ ! -d "/Applications/Utilities/XQuartz.app" && \
              ! -d "/Applications/XQuartz.app" ]]; then
            echo
            echo "** ERROR: XQuartz not found." 
            echo
            echo "   Is XQuartz installed and in your PATH?"
            echo "   If not, you can install it from homebrew"
            echo "   or from here: https://www.xquartz.org"
            echo
            exit 1
        fi
    fi   ## end of installed XQuartz check

    #################################################
    ## check for Xquartz security setting
    xquartz_sec=`defaults read org.xquartz.X11.plist nolisten_tcp`
    xquartz_auth=`defaults read org.xquartz.X11 no_auth`

    if [[ "$xquartz_sec" == "1" || "$xquartz_auth" == "0" ]]; then
        cat <<EOF

** ERROR: XQuartz is blocking tcp clients needed for docker.

   You can fix this by entering the following commands with XQuartz quit:

     defaults write org.xquartz.X11.plist nolisten_tcp -bool false
     defaults write org.xquartz.X11 no_auth -boolean true

   OR you can do this in the Security tab of the Settings in XQuartz:

     Uncheck the 'Authenticate connections' box.
     AND
     Check the 'Allow connections from network clients' box.

   OR I can fix this for you now.

     Do you want me to fix this? You only have to do this once.
     XQuartz will need to be quit to do this.

EOF

        read -p "  Should I quit XQuartz and fix this? y/[n]" fix

        if [[ "$fix" == "y" ]]; then
            xquart_pid=`pgrep -i Xquartz`
            if [[ -n "$xquart_pid" ]]; then
                echo "Killing all XQuartz..."
                killall Xquartz
                sleep 2
            fi
            echo
            echo "defaults write org.xquartz.X11.plist nolisten_tcp -bool false"
            defaults write org.xquartz.X11.plist nolisten_tcp -bool false
            echo "defaults write org.xquartz.X11 no_auth -boolean true"
            defaults write org.xquartz.X11 no_auth -bool true
            echo
        else
            echo
            echo "Please fix the XQuartz setting with the above instructions."
            echo
            exit 1
        fi
    fi   ## end check for XQuartz security setting

    #################################################
    ## check for Xquartz indirect GLX setting
    xquartz_iglx=`defaults read org.xquartz.X11 enable_iglx 2>/dev/null`

    if [[ "$xquartz_iglx" != "1" ]]; then
        cat <<EOF

** ERROR: XQuartz indirect GLX is not enabled.

   This setting is needed for OpenGL programs such as SUMA.
   You can fix this by entering the following command with XQuartz quit:

     defaults write org.xquartz.X11 enable_iglx -bool true

   OR I can fix this for you now.

     Do you want me to fix this? You only have to do this once.
     XQuartz will need to be restarted for this change to take effect.

EOF

        read -p "  Should I quit XQuartz and fix this? y/[n]" fix

        if [[ "$fix" == "y" ]]; then
            xquart_pid=`pgrep -i Xquartz`
            if [[ -n "$xquart_pid" ]]; then
                echo "++ Killing all XQuartz..."
                killall Xquartz
                sleep 2
            fi
            echo
            echo "defaults write org.xquartz.X11 enable_iglx -bool true"
            defaults write org.xquartz.X11 enable_iglx -bool true
            echo
        else
            echo
            echo "++ Please fix the XQuartz setting with above instructions."
            echo
            exit 1
        fi
    fi   ## end check for XQuartz indirect GLX setting

    #################################################
    ## check if docker is running and launch it if not

    ## this file should exist if docker is running
    if [[ ! -e "/Users/${USER}/.docker/run/docker.sock" ]]; then
        echo ; echo "+* Warning: Docker daemon is not running."
        echo "++ Launching Docker daemon. Please wait."
        open -a Docker
        sleep 5 ; echo

        while [[ ! -e "/Users/${USER}/.docker/run/docker.sock" ]]; do 
            echo "++ Waiting for Docker daemon..."
            sleep 2
        done
    fi   ## end launch docker if not running

    #################################################
    ## check if XQuartz is running and launch it if not
    xquart_pid=`pgrep -i Xquartz`
    if [[ -z "$xquart_pid" ]]; then
        echo ; echo "+* Warning: Xquartz is not running."
        echo "++ Launching Xquartz. Please wait."
        open -a XQuartz
        sleep 5 ; echo

        while true; do 
            xquart_pid=`pgrep -i Xquartz`
            if [[ -z "$xquart_pid" ]]; then
                echo "++ Waiting for Xquartz..."
                sleep 2
            else
                break
            fi
        done
    fi   ## end launch XQuartz if not running

    # 4. Dynamically determine what port XQuartz is ACTUALLY using right now
    # This reads the port from your user-owned processes, bypassing root restrictions.
    PORT=$(lsof -i -P -s TCP:LISTEN | grep -i X11 | awk -F: '{print $2}' | awk '{print $1}' | head -n 1)
    
    # If XQuartz jumped to 6001 or 6002 because of ghost lock files, 
    # calculate the correct display number ($DISPLAY = port - 6000)
    DISPLAY_NUM=0
    if [ ! -z "$PORT" ]; then
        DISPLAY_NUM=$((PORT - 6000))
    fi
    echo "🚀 Launching Docker with DISPLAY=$disp:$DISPLAY_NUM (Port: $PORT)"
    disp=$disp:$DISPLAY_NUM 
  
fi   ## end of macOS check

#################################################
## checks for linux
if [[ "$os" == "linux" ]]; then

    ## open up the X11 server to allow connections from docker containers
    xhost +SI:localuser:$USER
    
    ######################################################
    ## Check for docker group and if the user is a member.
    docker_grp_exists="`getent group | grep docker`"
    if [ "$docker_grp_exists" = "" ]; then
        cat <<EOF

** ERROR: The docker group does not exist.

   Please create the docker group:

     sudo groupadd docker

   and add yourself to it:

     sudo usermod -aG docker \$USER

   Then restart your computer.

   If you are in the docker group, there may be a problem with this program...
   Please post the error to: https://discuss.afni.nimh.nih.gov

EOF
        exit 1
    fi

    docker_member="`groups $USER | grep docker`"
    if [ "$docker_member" = "" ]; then
cat <<EOF

** ERROR: You don't seem to be in the docker group.
   This may cause issues with permissions when running the docker container.
   If you haven't already, you may want to add yourself to the docker group:

     sudo usermod -aG docker \$USER

   and restart your computer.

   If you are in the docker group, there may be a problem with this program...
   Please post the error to: https://discuss.afni.nimh.nih.gov

EOF

        exit 1
    fi   ## end of docker group check

    #########################################################
    ## check to see if docker is running.
    docker_active="`systemctl is-active docker`"
    docker_desktop_active="`systemctl --user is-active docker-desktop`"
    if [ "$docker_active" = "active" ] || \
       [ "$docker_desktop_active" = "active" ]; then
        echo
        echo "++ Docker is running."
        echo
    else 
        cat <<EOF

** ERROR: Docker is not running.
   Please start the docker service:

     sudo systemctl start docker

   or

     systemctl --user start docker-desktop

   Then try again.
   Please post the error to: https://discuss.afni.nimh.nih.gov

EOF

        exit 1
    fi   ## end of docker running check

fi   ## end of linux check

###########################################################################
## run docker container

cat <<EOF

++ Launching: ${dock_img}

   ...beep boop beep boop...

EOF

# display command if user asked 

if [ ${show_cmd} = 1 ]; then

cat <<EOF
++ Launching AFNI docker with this command:
   ------------------------------------------------
    docker run -ti --rm                          \\
        -u      root                             \\
        -v      "${HOME}:/home/external" \\
        -v      /tmp/.X11-unix:/tmp/.X11-unix    \\
        --env   DISPLAY="${disp}" \\
        --env   USERID="`id -u`"                 \\
        --env   GRPID="`id -g`"                  \\
        --env   GRPNAME="`id -gn`"               \\
        --env   USERNAME="`id -u -n`"            \\
        --pull  "${pull}"                        \\
        "${dock_img}"
   ------------------------------------------------

EOF

fi

# finally, launch the docker

# xhost +SI:localuser:$USER
docker run -ti --rm                          \
    -u      root                             \
    -v      "${HOME}:/home/external"         \
    -v      /tmp/.X11-unix:/tmp/.X11-unix    \
    --env   DISPLAY="${disp}"                \
    --env   USERID="`id -u`"                 \
    --env   GRPID="`id -g`"                  \
    --env   GRPNAME="`id -gn`"               \
    --env   USERNAME="`id -u -n`"            \
    --pull  "${pull}"                        \
    "${dock_img}"

exit 0
