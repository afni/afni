#! /bin/bash
rm -f random_speed
if ./random 1000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "1000 $speed" >> random_speed

if ./random 2000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "2000 $speed" >> random_speed

if ./random 4000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "4000 $speed" >> random_speed

if ./random 8000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "8000 $speed" >> random_speed

if ./random 16000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "16000 $speed" >> random_speed

if ./random 32000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "32000 $speed" >> random_speed

if ./random 64000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "64000 $speed" >> random_speed

if ./random 128000 > /dev/null 2> /tmp/speed; then
    exit 1
fi
speed=`awk '{if ($4 == "speed:") print $5}' < /tmp/speed`
echo "128000 $speed" >> random_speed

exit 0
