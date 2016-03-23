#!/usr/bin/python3
import sys
from models.digraph import DiGraph
from tests.test import Test


sys.setrecursionlimit(2*10**9)
DEBUG = "-d" in sys.argv or "--debug" in sys.argv


Test().testConnectedComponents()
exit(0)


G = DiGraph(DEBUG=DEBUG)
G.populateFromFile("samples/graph1k.txt")
#G.populateFromFile("samples/graph1m.txt")

def showMenu():
    print()
    print("1 - get number of vertices")
    print("2 - check if edge exists")
    print("3 - degrees of vertex")
    print("4 - show outbound edges of a vertex")
    print("5 - show inbound edges of a vertex")
    print("6 - add vertex")
    print("7 - remove vertex")
    print("8 - add edge")
    print("9 - remove edge")
    print("0 - exit")
    print()


showMenu()
command = input("> ")
while True:

    if command == '1':
        print("There are %i vertices." % G.numberOfVertices())
    elif command == '2':
        source = int(input("source: "))
        target = int(input("target: "))

        G.isEdge(source, target)
        """
        if G.isEdge(source, target):
            print("Edge exists.")
        else:
            print("Edge doesn't exist.")
        """
    elif command == '3':
        x = int(input("vertex: "))
        print("inDegree: %i" % G.inDegree(x))
        print("outDegree: %i" % G.outDegree(x))
    elif command == '4':
        x = int(input("vertex: "))
        print("out edges: %s" % ', '.join([str(y[0]) for y in G.outboundEdges(x)]))
    elif command == '5':
        x = int(input("vertex: "))
        print("in edges: %s" % ', '.join([str(y[0]) for y in G.inboundEdges(x)]))
    elif command == '6':
        x = int(input("vertex: "))
        G.addNode(x)
    elif command == '7':
        x = int(input("vertex: "))
        G.removeVertex(x)
    elif command == '8':
        source = int(input("source: "))
        target = int(input("target: "))
        weight = int(input("weight: "))
        G.addEdge(source, target, weight)
    elif command == '9':
        source = int(input("source: "))
        target = int(input("target: "))
        G.removeEdge(source, target)
    elif command == '0':
        exit(0)
    else:
        print("bad command.")

    command = input("> ")
    showMenu()


# components = G.scc()
#print(data['discovery'])
#print(data['finish'])

"""
print("There are %i strongly connected components:" % len(components))
for x in components:
    print(x)
print()

data = G.iterativeDFS()

print(data['visitedVertices'])
print(data['discovery'])
print(data['finish'])
"""


