#!/bin/bash

rm good.out bad.out 2>/dev/null

for i in `seq 1 1000`; do
    echo $i | ./c >> good.out
done
