#!/bin/bash

echo -n > timer.log

# echo Running PY
# echo PY: >> timer.log
# { time `python 1.py > /dev/null`; } 2>> timer.log

echo Running CPP
echo -n CPP: >> timer.log
{ time `./1cpp > /dev/null`; } 2>> timer.log

echo Running ASM
echo -n ASM: >> timer.log
{ time `./1 > /dev/null`; } 2>> timer.log
