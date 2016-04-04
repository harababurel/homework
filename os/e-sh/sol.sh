#!/bin/bash

# RO: Sa se scrie un script shell care citeste de la utilizator (read)
#    nume de fisiere pana cand se introduc consecutiv doua nume de
#    fisiere cu aceeasi dimensiune in bytes si cu drepturi de executie.
#
# EN: Write a shell script that reads from the user input (read) filenames
#     until the user gives two files consecutively that have the same size
#     in bytes and both have execution permissions.


prev=""
curr=""

read prev
while [ ! -f $prev ]; do
	echo "$prev is not a file!"
	read prev
done

while true; do
	read curr

	if [ ! -f $curr ]; then
		echo "$curr is not a file!"
		continue
	fi

	size_prev=`du -b $prev | awk '{ print $1 }'`
	size_curr=`du -b $curr | awk '{ print $1 }'`

	exec_prev=`ls -l $prev | cut -c 4 | grep -o "x" | wc -l`
	exec_curr=`ls -l $curr | cut -c 4 | grep -o "x" | wc -l`

	echo "sizes: $size_prev and $size_curr, executable: $exec_prev and $exec_curr"

	if [ "$size_prev" == "$size_curr" ] && [ "$exec_prev" -gt "0" ] && [ "$exec_curr" -gt "0" ]; then
		echo "Same size in bytes, both files are executable."
		exit 0
	fi

	prev=$curr
done

