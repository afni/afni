# This image is used in place of circleci Virtual machines or docker images.
# This alternative usage allows more portability, and provides convenient local
# execution using the circleci CLI.  If you want to avail of this install the
# client locally: 
#
# https://circleci.com/docs/2.0/local-cli/
#
# Find the names of the various jobs that you can execute by observing the
# fully resolved circleci configuration.  This can be viewed (look for the most
# unindented entries in the jobs section) by running the following command from
# the base AFNI directory:
#
# circleci config process .circleci/config.yml
#
# Finally, to run a job locally:
#
# circleci local execute cmake_build
#
# The above command will download an image from AFNI's dockerhub account
#
FROM alpine:3.12

RUN apk add bash curl docker-cli docker-py git pigz py3-pytest python3 openssh shadow

# The following doesn't work. Root permission is required for local execution
# (both to access the repository stored in a /tmp directory that is mounted in
# from the host as well as to contact the docker socket
# RUN adduser -u 3434 -D circleci && \
# echo "circleci ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/circleci && \
# chmod 0440 /etc/sudoers.d/circleci
#USER circleci

#ENTRYPOINT /bin/bash
# CMD /bin/bash
