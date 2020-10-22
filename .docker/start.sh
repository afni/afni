#!/bin/bash
# Original code:
# Copyright (c) Jupyter Development Team.
# Distributed under the terms of the Modified BSD License.
# See https://github.com/jupyter/docker-stacks/blob/master/LICENSE.md

set -e

# Exec the specified command or fall back on bash
if [ $# -eq 0 ]; then
    cmd=( "bash" )
else
    cmd=( "$@" )
fi

run-hooks () {
    # Source scripts or run executable files in a directory
    if [[ ! -d "$1" ]] ; then
        return
    fi
    echo "$0: running hooks in $1"
    for f in "$1/"*; do
        case "$f" in
            *.sh)
                echo "$0: running $f"
                source "$f"
                ;;
            *)
                if [[ -x "$f" ]] ; then
                    echo "$0: running $f"
                    "$f"
                else
                    echo "$0: ignoring $f"
                fi
                ;;
        esac
    done
    echo "$0: done running hooks in $1"
}

#run-hooks /usr/local/bin/image_startup.d

# Handle special flags if we're root
if [ $(id -u) == 0 ] ; then

    # Only attempt to change the afni_user username if it exists
    if id afni_user &> /dev/null ; then
        echo "Set username to: $CONTAINER_USER"
        usermod -d /home/$CONTAINER_USER -l $CONTAINER_USER afni_user
    fi

    # Handle case where provisioned storage does not have the correct permissions by default
    # Ex: default NFS/EFS (no auto-uid/gid)
    if [[ "$CHOWN_HOME" == "1" || "$CHOWN_HOME" == 'yes' ]]; then
        echo "Changing ownership of /home/$CONTAINER_USER to $CONTAINER_UID:$CONTAINER_GID with options '${CHOWN_HOME_OPTS}'"
        chown $CHOWN_HOME_OPTS $CONTAINER_UID:$CONTAINER_GID /home/$CONTAINER_USER
    fi
    if [ ! -z "$CHOWN_EXTRA" ]; then
        for extra_dir in $(echo $CHOWN_EXTRA | tr ',' ' '); do
            echo "Changing ownership of ${extra_dir} to $CONTAINER_UID:$CONTAINER_GID with options '${CHOWN_EXTRA_OPTS}'"
            chown $CHOWN_EXTRA_OPTS $CONTAINER_UID:$CONTAINER_GID $extra_dir
        done
    fi

    # handle home and working directory if the username changed
    if [[ "$CONTAINER_USER" != "afni_user" ]]; then
        # changing username, make sure homedir exists
        # (it could be mounted, and we shouldn't create it if it already exists)
        if [[ ! -e "/home/$CONTAINER_USER" ]]; then
            echo "Relocating home dir to /home/$CONTAINER_USER"
            mv /home/afni_user "/home/$CONTAINER_USER" || ln -s /home/afni_user "/home/$CONTAINER_USER"
        fi
        # if workdir is in /home/afni_user, cd to /home/$CONTAINER_USER
        if [[ "$PWD/" == "/home/afni_user/"* ]]; then
            newcwd="/home/$CONTAINER_USER/${PWD:13}"
            echo "Setting CWD to $newcwd"
            cd "$newcwd"
        fi
    fi

    # Change UID:GID of CONTAINER_USER to CONTAINER_UID:CONTAINER_GID if it does not match
    if [ "$CONTAINER_UID" != $(id -u $CONTAINER_USER) ] || [ "$CONTAINER_GID" != $(id -g $CONTAINER_USER) ]; then
        echo "Set user $CONTAINER_USER UID:GID to: $CONTAINER_UID:$CONTAINER_GID"
        if [ "$CONTAINER_GID" != $(id -g $CONTAINER_USER) ]; then
            groupadd -g $CONTAINER_GID -o ${CONTAINER_USER}
        fi
        userdel $CONTAINER_USER
        useradd --home /home/$CONTAINER_USER -u $CONTAINER_UID -g $CONTAINER_GID -G 100 -l $CONTAINER_USER
    fi

    # Enable sudo if requested
    if [[ "$GRANT_SUDO" == "1" || "$GRANT_SUDO" == 'yes' ]]; then
        echo "Granting $CONTAINER_USER sudo access and appending $CONDA_DIR/bin to sudo PATH"
        echo "$CONTAINER_USER ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/notebook
    fi

    # Add $CONDA_DIR/bin to sudo secure_path
    sed -r "s#Defaults\s+secure_path\s*=\s*\"?([^\"]+)\"?#Defaults secure_path=\"\1:$CONDA_DIR/bin\"#" /etc/sudoers | grep secure_path > /etc/sudoers.d/path

    # Exec the command as CONTAINER_USER with the PATH and the rest of
    # the environment preserved
    run-hooks /usr/local/bin/before-notebook.d
    echo "Executing the command: ${cmd[@]}"
    exec sudo -E -H -u $CONTAINER_USER PATH=$PATH XDG_CACHE_HOME=/home/$CONTAINER_USER/.cache  "${cmd[@]}"
else
    if [[ "$CONTAINER_UID" == "$(id -u afni_user)" && "$CONTAINER_GID" == "$(id -g afni_user)" ]]; then
        # User is not attempting to override user/group via environment
        # variables, but they could still have overridden the uid/gid that
        # container runs as. Check that the user has an entry in the passwd
        # file and if not add an entry.
        STATUS=0 && whoami &> /dev/null || STATUS=$? && true
        if [[ "$STATUS" != "0" ]]; then
            if [[ -w /etc/passwd ]]; then
                echo "Adding passwd file entry for $(id -u)"
                cat /etc/passwd | sed -e "s/^afni_user:/container_user:/" > /tmp/passwd
                echo "afni_user:x:$(id -u):$(id -g):,,,:/home/afni_user:/bin/bash" >> /tmp/passwd
                cat /tmp/passwd > /etc/passwd
                rm /tmp/passwd
            else
                echo 'Container must be run with group "root" to update passwd file'
            fi
        fi

        # Warn if the user isn't going to be able to write files to $HOME.
        if [[ ! -w /home/afni_user ]]; then
            echo 'Container must be run with group "users" to update files'
        fi
    else
        # Warn if looks like user want to override uid/gid but hasn't
        # run the container as root.
        if [[ ! -z "$CONTAINER_UID" && "$CONTAINER_UID" != "$(id -u)" ]]; then
            echo 'Container must be run as root to set $CONTAINER_UID'
        fi
        if [[ ! -z "$CONTAINER_GID" && "$CONTAINER_GID" != "$(id -g)" ]]; then
            echo 'Container must be run as root to set $CONTAINER_GID'
        fi
    fi

    # Warn if looks like user want to run in sudo mode but hasn't run
    # the container as root.
    if [[ "$GRANT_SUDO" == "1" || "$GRANT_SUDO" == 'yes' ]]; then
        echo 'Container must be run as root to grant sudo permissions'
    fi

    # Execute the command
    run-hooks /usr/local/bin/before-notebook.d
    echo "Executing the command: ${cmd[@]}"
    exec "${cmd[@]}"
fi
