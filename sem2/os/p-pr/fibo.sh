#!/bin/bash

if [ "$#" -ne "1" ]; then
    echo "Usage: $0 position"
    exit 1
fi

n=$1

if [ $n -le 0 ]; then
    echo "Position must be greater than 0"
    exit 1
fi

if [ $n -lt 3 ]; then
    echo 1
    exit 0
fi

a=1
b=1

for i in `seq 3 $n`; do
    c=`expr $a + $b`
    a=$b
    b=$c
done

echo $c
