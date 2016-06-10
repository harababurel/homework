#!/bin/bash

# 88. a)
# Write a shell script that continuously reads user names from keyboard and
# for each of them outputs its full name, the number of processes and the
# current running command.

# Idea
# The full name can be found in /etc/passwd, on the same line as the username,
# the fifth column.
#
# The number of running processes can be computed by counting all output lines
# of `ps`, except for the first one (which is the table header).
#
# The current command is the most recent one. `ps` allows changing the output
# format, so we can print only the command name.

while true;
do
    read username

    full_name=`grep "^$username" /etc/passwd | awk -F ":" '{print $5}'`
    echo The full name of $username is $full_name.

    process_count=`ps u --user=$username | tail -n +2 | wc -l`
    echo They have $process_count running processes.

    last_command=`ps --user=$username -o comm | tail -n 1`
    echo "Their last running command is $last_command."
done
