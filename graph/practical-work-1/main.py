#!/usr/bin/python3
import sys
from models.digraph import DiGraph

sys.setrecursionlimit(2*10**9)

DEBUG = "-d" in sys.argv or "--debug" in sys.argv

G = DiGraph(DEBUG=DEBUG)
G.populateFromFile("samples/digraph-ex1.txt")

"""
G.isEdge(1, 5)
G.removeEdge(1, 5)
G.isEdge(1, 5)
G.addEdge(1, 5, 3)
G.isEdge(1, 5)

G.getWeight(1, 5)

G.removeVertex(1)
"""
components = G.scc()

#print(data['discovery'])
#print(data['finish'])

print("There are %i strongly connected components:" % len(components))
for x in components:
    print(x)
print()

data = G.iterativeDFS()

print(data['visitedVertices'])
print(data['discovery'])
print(data['finish'])
