#!/usr/bin/python3
import os
from collections import defaultdict

class DiGraph:
    def __init__(self, nodes=set(), outEdges=defaultdict(list), inEdges=defaultdict(list), weight={}):
        self.nodes = nodes
        self.outEdges = outEdges
        self.inEdges = inEdges
        self.weight = weight

    def addNode(self, node):
        self.nodes.add(node)

    def isNode(self, node):
        return node in self.nodes

    def addEdge(self, source, target, weight):
        """
        if not self.isNode(source):
            self.addNode(source)
        if not self.isNode(target):
            self.addNode(target)
        """

        self.outEdges[source].append(target)
        self.inEdges[target].append(source)

        self.weight[(source, target)] = weight

    def populateFromFile(self, filename):
        path = os.path.join(os.getcwd(), filename)
        print("Path is %s" % path)
        try:
            with open(path, 'r') as f:
                n, m = map(int, f.readline().split())

                for node in range(n):
                    self.addNode(node)

                for edge in range(m):
                    source, target, weight = map(int, f.readline().split())
                    self.addEdge(source, target, weight)

        except Exception as e:
            print("Could not open %s for reading." % path)
            print("Reason: %s" % e)
