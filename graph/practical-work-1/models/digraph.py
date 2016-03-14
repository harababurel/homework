#!/usr/bin/python3
import os
from collections import defaultdict

class DiGraph:
    def __init__(self, vertices=set(), outEdges=defaultdict(list), inEdges=defaultdict(list), DEBUG=False):
        self.vertices = vertices
        self.outEdges = outEdges
        self.inEdges = inEdges
        self.DEBUG = DEBUG

    def numberOfVertices(self):
        return len(self.vertices)

    def inDegree(self, node):
        return len(self.inEdges[node])

    def outDegree(self, node):
        return len(self.outEdges[node])

    def addNode(self, node):
        if self.isNode(node):
            if self.DEBUG:
                print("Vertex %i already exists." % node)

        else:
            if self.DEBUG:
                print("Adding vertex %i." % node)
            self.vertices.add(node)

    def isNode(self, node):
        return node in self.vertices

    def outboundEdges(self, node):
        for neighbor in self.outEdges[node]:
            yield neighbor

    def inboundEdges(self, node):
        for neighbor in self.inEdges[node]:
            yield neighbor

    def setWeight(self, source, target, weight):
        for neighbor in self.outboundEdges(source):
            if neighbor[0] == target:
                neighbor = (neighbor[0], weight)        # might be dangerous

                if self.DEBUG:
                    print("Found edge %i -> %i. New weight is %i." % (source, target, weight))
                break

        for neighbor in self.inboundEdges(target):
            if neighbor[0] == source:
                neighbor = (neighbor[0], weight)

                if self.DEBUG:
                    print("Found edge %i -> %i. New weight is %i." % (target, source, weight))
                break

    def getWeight(self, source, target):
        for neighbor in self.outboundEdges(source):
            if neighbor[0] == target:
                if self.DEBUG:
                    print("Edge %i -> %i has weight %i." % (source, target, neighbor[1]))
                return neighbor[1]
        raise Exception("Edge does not exist.")

    def addEdge(self, source, target, weight):
        self.addNode(source)
        self.addNode(target)

        self.outEdges[source].append((target, weight))
        self.inEdges[target].append((source, weight))

        if self.DEBUG:
            print("Added edge %i -> %i with weight %i." % (source, target, weight))

    def removeEdge(self, source, target):
        for i in range(len(self.outEdges[source])):
            if self.outEdges[source][i][0] == target:
                self.outEdges[source][i] = self.outEdges[source][ len(self.outEdges[source])-1 ]
                self.outEdges[source].pop()
                break

        for i in range(len(self.inEdges[target])):
            if self.inEdges[target][i][0] == source:
                self.inEdges[target][i] = self.inEdges[target][ len(self.inEdges[target])-1 ]
                self.inEdges[target].pop()
                break

        if self.DEBUG:
            print("Removed edge %i -> %i." % (source, target))


    def removeVertex(self, node):
        self.outEdges[node] = []
        self.vertices.remove(node)

        for x in self.vertices:
            for i in range(len(self.inEdges[x])):
                if self.inEdges[x][i][0] == node:
                    self.inEdges[x][i] = self.inEdges[x][ len(self.inEdges[x])-1 ]
                    self.inEdges[x].pop()
                    break
        if self.DEBUG:
            print("Removed vertex %i and all adjacent edges." % node)


    def isEdge(self, source, target):
        """
        O(min(outDeg(source), inDeg(target)))
        """

        if len(self.outEdges[source]) < len(self.inEdges[target]):
            for neighbor in self.outEdges[source]:
                if neighbor[0] == target:
                    if self.DEBUG:
                        print("Edge %i -> %i exists." % (source, target))
                    return True

        else:
            for neighbor in self.inEdges[target]:
                if neighbor[0] == source:
                    if self.DEBUG:
                        print("Edge %i -> %i exists." % (source, target))
                    return True
        if self.DEBUG:
            print("Edge %i -> %i does not exist." % (source, target))
        return False


    def populateFromFile(self, filename):
        path = os.path.join(os.getcwd(), filename)
        if self.DEBUG:
            print("Will populate graph from file %s" % path)
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
        if self.DEBUG:
            print("Graph was populated.")

    def dfs(self, node, wasVisited=None, visitedVertices=[], discovery={}, finish={}, currentTime=[1]):
        if wasVisited is None:
            wasVisited = {x: False for x in self.vertices}

        if self.DEBUG:
            print("Discovered vertex %i at time %i." % (node, currentTime[0]))

        if wasVisited[node]:
            return (wasVisited, visitedVertices, discovery, finish, currentTime)


        wasVisited[node] = True
        visitedVertices.append(node)
        discovery[node] = currentTime
        currentTime[0] += 1

        for edge in self.outboundEdges(node):
            neighbor = edge[0]
            if not wasVisited[neighbor]:
                self.dfs(neighbor, wasVisited, visitedVertices, discovery, finish, currentTime)

        finish[node] = currentTime[0]
        currentTime[0] += 1

        if self.DEBUG:
            print("Finished vertex %i at time %i." % (node, currentTime[0]))

        return (wasVisited, visitedVertices, discovery, finish, currentTime)

    def scc(self):
        wasVisited = {x: False for x in self.vertices}
        visitedVertices = []
        discovery = {}
        finish = {}
        currentTime = [0]

        for x in self.vertices:
            if not wasVisited[x]:
                (wasVisited, visitedVertices, discovery, finish, currentTime) = self.dfs(x, wasVisited, visitedVertices, discovery, finish, currentTime)

        orderedVertices = sorted(self.vertices, key=lambda x: finish[x], reverse=True)

        if self.DEBUG:
            print("In order to compute the SCCs, vertices should be processed in the following order:")
            print(" -> ".join([str(x) for x in orderedVertices]))


        wasVisited = None
        for x in orderedVertices:
            exploration = self.dfs(x, wasVisited)

            wasVisited = exploration[0]
            component = exploration[1]
            print(component)


