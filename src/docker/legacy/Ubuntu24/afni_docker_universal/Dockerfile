FROM ubuntu:24.04

## 03/2026 Justin Rajendra
## This Dockerfile is meant to build a docker image with afni pre installed
## and configured for use on arm and x86 platforms
## can be run with "--platform=linux/amd64,linux/arm64" during build 
## to get both platforms at once. Or just omit the --platform for your local 
## machine

## get the target platforms for the different builds
ARG TARGETPLATFORM

## add user afni_user in the docker container and give it a home folder
RUN useradd -m -d /home/afni_user -s /bin/bash afni_user

## remove the default ubuntu user that takes uid 1000 (need to ponder this)
RUN userdel -r ubuntu; usermod -u 1000 afni_user; groupmod -g 1000 afni_user

## copy root scripts for dependencies
ENV ROOT_SCRIPT=create_machine_root.sh
ADD $ROOT_SCRIPT /tmp

## run root script (same script for arm and x86)
RUN bash /tmp/$ROOT_SCRIPT

## clean up
RUN rm -f /tmp/$ROOT_SCRIPT

## copy user scripts for afni install
ENV USER_SCRIPT_ARM=create_machine_ARM_user.sh
ENV USER_SCRIPT_x86=create_machine_x86_user.sh
ADD $USER_SCRIPT_ARM /home/afni_user
ADD $USER_SCRIPT_x86 /home/afni_user

## compiler options
ENV LC_ALL=C

## change to the afni user for the next steps
USER afni_user

## run scripts based on platform (windows is just a placeholder for now)
RUN if [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
        bash /home/afni_user/$USER_SCRIPT_ARM; \
    elif [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
        bash /home/afni_user/$USER_SCRIPT_x86; \
    elif [ "$TARGETPLATFORM" = "windows/amd64" ]; then \
        bash /home/afni_user/$USER_SCRIPT_x86; \
    fi

## clean up
RUN rm -f /home/afni_user/$USER_SCRIPT_ARM
RUN rm -f /home/afni_user/$USER_SCRIPT_x86

## copy .bashrc for root
USER root
RUN cp /home/afni_user/.bashrc /root/

## fix up the user names and ids
## add stuff to the .bashrc for root
RUN echo 'groupadd -gf "${GRPID}" "${GRPNAME}" &>/dev/null' >> /root/.bashrc
RUN echo 'groupmod -n "${GRPNAME}" afni_user &>/dev/null' >> /root/.bashrc
RUN echo 'usermod -l "${USERNAME}" afni_user &>/dev/null' >> /root/.bashrc
RUN echo 'usermod -g "${GRPNAME}" "${USERNAME}" &>/dev/null' >> /root/.bashrc
RUN echo 'usermod -u "${USERID}" "${USERNAME}" &>/dev/null' >> /root/.bashrc
RUN echo 'su "${USERNAME}"' >> /root/.bashrc
RUN echo '    ' >> /root/.bashrc

## go to the host home and bash it
WORKDIR /home/external
CMD ["/bin/bash"]

