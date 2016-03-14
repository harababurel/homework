#!/usr/bin/python3
import matplotlib.pyplot as plt
from random import randint
import os

n = 30
points = {}

v = []
for i in range(n):
    point = (randint(1, 100), randint(1, 100), i)
    while (point[0], point[1]) in points:
        point = (randint(1, 100), randint(1, 100), i)

    points[(point[0], point[1])] = True

    v.append(point)

with open("i.in", "w") as g:
    g.write("1\n%i " % n)
    for x in v:
        g.write("%i %i " % (x[0], x[1]))
    g.write("\n")


os.system("cat i.in | ./i > i.out")

with open("i.out", "r") as f:
    order = list(map(int, f.readline().split()))


plt.axis([-1,101, -1,101])


#plt.plot([v[order[i]][0] for i in range(n)] + [v[order[0]][0]], [v[order[i]][1] for i in range(n)] + [v[order[0]][1]])
plt.plot([v[order[i]][0] for i in range(n)], [v[order[i]][1] for i in range(n)])

plt.show()
