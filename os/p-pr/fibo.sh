#!/bin/bash

n=$1

if [ "$n" -lt "3" ]; then
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
