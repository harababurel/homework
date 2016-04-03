#!/usr/bin/python3
import sys

t = 5
n_old = int(input())

if n_old == -1:
    exit(0)

print("1")
sys.stdout.flush()

n_new = int(input())

while n_new >= 0:
    if n_old > n_new:
        n_old = n_new
        print("%i" % t)

        t *= 2
        sys.stdout.flush()
    else:
        n_old = n_new
        t = 5
        print("1")
        sys.stdout.flush()

    n_new = int(input())
