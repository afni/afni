#! /bin/bash
rm -f cartesian_speed
if ./cartesian 32 32 0.1 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "1024 $speed" >> cartesian_speed

if ./cartesian 45 45 0.1 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "2025 $speed" >> cartesian_speed

if ./cartesian 64 64 0.1 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "4096 $speed" >> cartesian_speed

if ./cartesian 90 90 0.1 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "8100 $speed" >> cartesian_speed

if ./cartesian 128 128 0.1 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "16384 $speed" >> cartesian_speed

exit 0
