#!/usr/bin/python3
import sys
from models.digraph import DiGraph

DEBUG = "-d" in sys.argv or "--debug" in sys.argv

G = DiGraph(DEBUG=DEBUG)
G.populateFromFile("samples/graph1k.txt")


G.isEdge(1, 5)
G.removeEdge(1, 5)
G.isEdge(1, 5)
G.addEdge(1, 5, 3)
G.isEdge(1, 5)

G.getWeight(1, 5)

G.removeVertex(1)


