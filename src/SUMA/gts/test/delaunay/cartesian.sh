#! /bin/bash
if ./cartesian 10 10 0.1 2> /dev/null > /dev/null; then
    exit 1
fi
exit 0
