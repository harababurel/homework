#!/bin/bash

# 88. b)
# Write a shell script that takes groups of 3 parameters  (a name of a file a
# word and a number k). For each groups, print all the lines from the file that
# contain the word exactly k times.

# Idea:
# 1. Iterate through the lines of the current file.
# 2. Grep the given word and print each match.
# 3. Count the number of matches. If k, print the line

while read filename word k;
do
    while read line;
    do
        occurrences=`echo $line | grep -o "\<$word\>" | wc -l`
        if test $occurrences -eq $k ; then
           echo $line
        fi
    done < $filename
done
