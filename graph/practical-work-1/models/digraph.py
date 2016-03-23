#!/usr/bin/python3
import os
from collections import defaultdict, deque, OrderedDict
from copy import deepcopy
from math import inf

class DiGraph:
    def __init__(self, vertices=set(), outEdges=defaultdict(list), inEdges=defaultdict(list), undirected=False, DEBUG=False):
        self.vertices = vertices
        self.outEdges = outEdges
        self.inEdges = inEdges
        self.undirected = undirected
        self.DEBUG = DEBUG

    def numberOfVertices(self):
        return len(self.vertices)

    def inDegree(self, node):
        if self.isNode(node):
            return len(self.inEdges[node])
        raise Exception("Node %i is not a node." % i)

    def outDegree(self, node):
        if self.isNode(node):
            return len(self.outEdges[node])
        raise Exception("Node %i is not a node." % i)

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
        for edge in self.outEdges[node]:
            yield edge

    def inboundEdges(self, node):
        for edge in self.inEdges[node]:
            yield edge

    def setWeight(self, source, target, weight):
        for neighbor in self.outboundEdges(source):
            if neighbor[0] == target:
                neighbor = (neighbor[0], weight)        # might be dangerous

                if self.DEBUG:
                    print("Found edge %i -> %i. New weight is %i." % (source, target, weight))
                break

        for neighbor in self.inboundEdges(target):
            if neighbor[0] == source:
                check = (neighbor[0], weight)

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

        # TODO: check if edge exists

        self.outEdges[source].append((target, weight))
        self.inEdges[target].append((source, weight))

        if self.undirected:
            self.outEdges[target].append((source, weight))
            self.inEdges[source].append((target, weight))


        if self.DEBUG:
            if self.undirected:
                print("Added edge %i <-> %i with weight %i." % (source, target, weight))
            else:
                print("Added edge %i -> %i with weight %i." % (source, target, weight))


    def removeEdge(self, source, target):
        if not self.isEdge(source, target):
            return

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
        if not self.isNode(node):
            if self.DEBUG:
                print("Vertex doesn't exist.")
            return

        self.outEdges[node] = []
        self.vertices.remove(node)

        # TOOD: check only vertices that have inbound edge from node

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

                for node in range(1, n+1):
                    self.addNode(node)

                for edge in range(m):
                    data = list(map(int, f.readline().split()))

                    source, target = data[0], data[1]
                    if len(data) == 3:
                        weight = data[2]
                    else:
                        weight = 1

                    self.addEdge(source, target, weight)

        except Exception as e:
            print("Could not open %s for reading." % path)
            print("Reason: %s" % e)
        if self.DEBUG:
            print("Graph was populated.")

    def recursiveDFS(self, node, data=None):
        if data is None:
            data = {
                    'wasVisited': { x: False for x in self.vertices },
                    'visitedVertices': [],
                    'discovery': {},
                    'finish': {},
                    'currentTime': 0,
                    }


        if data['wasVisited'][node]:
            return data

        data['wasVisited'][node] = True
        data['visitedVertices'].append(node)
        data['discovery'][node] = data['currentTime']
        data['currentTime'] += 1

        if self.DEBUG:
            print("Discovered vertex %i at time %i." % (node, data['discovery'][node]))

        for edge in self.outboundEdges(node):
            neighbor = edge[0]
            if not data['wasVisited'][neighbor]:
                self.recursiveDFS(neighbor, data)

        data['finish'][node] = data['currentTime']
        data['currentTime'] += 1

        if self.DEBUG:
            print("Finished vertex %i at time %i." % (node, data['finish'][node]))

        return data

    def iterativeDFS(self, data=None):
        if data is None:
            data = {
                    'wasVisited': { x: False for x in self.vertices },
                    'visitedVertices': [],
                    'discovery': {},
                    'finish': {},
                    'currentTime': 0,
                    }

        for source in self.vertices:
            if data['wasVisited'][source]:
                continue

            S = [source]
            while len(S) > 0:
                current = S.pop()

                data['wasVisited'][current] = True
                data['visitedVertices'].append(current)

                for edge in self.outboundEdges(current):
                    neighbor = edge[0]

                    if not data['wasVisited'][neighbor]:
                        data['wasVisited'][neighbor] = True
                        S.append(neighbor)
        return data

    def bfs(self, source, data=None):
        if data is None:
            data = {
                    'wasVisited': { x: False for x in self.vertices },
                    'visitedVertices': [],
                    }

        S = deque([source])

        while len(S) > 0:
            current = S.popleft()

            data['wasVisited'][current] = True
            data['visitedVertices'].append(current)

            for edge in self.outboundEdges(current):
                neighbor = edge[0]

                if not data['wasVisited'][neighbor]:
                    data['wasVisited'][neighbor] = True
                    S.append(neighbor)
        return data

    def connectedComponents(self):
        data = {
            'wasVisited': { x: False for x in self.vertices },
            'visitedVertices': [],
            }

        components = []
        for x in self.vertices:
            if not data['wasVisited'][x]:
                data['visitedVertices'] = []
                data = self.bfs(x, data)

                components.append(data['visitedVertices'])

        return components

    def scc(self):
        data = {
                'wasVisited': { x: False for x in self.vertices },
                'visitedVertices': [],
                'discovery': {},
                'finish': {},
                'currentTime': 0,
                }

        for x in self.vertices:
            if not data['wasVisited'][x]:
                data = self.recursiveDFS(x, data)

        orderedVertices = sorted(self.vertices, key=lambda x: data['finish'][x], reverse=True)

        # orderedVertices = reversed(self.iterativeDFS()['visitedVertices'])

        if self.DEBUG:
            print("In order to compute the SCCs, vertices should be processed in the following order:")
            print(' -> '.join([str(x) for x in orderedVertices]))

        T = self.transpose()
        data = {
                'wasVisited': { x: False for x in self.vertices },
                'visitedVertices': [],
                'discovery': {},
                'finish': {},
                'currentTime': 0,
                }

        components = []
        for x in orderedVertices:
            if not data['wasVisited'][x]:
                data['visitedVertices'] = []
                data = T.recursiveDFS(x, data)

                components.append(data['visitedVertices'])

        return components


    def transpose(self):
        ret = deepcopy(self)

        for x in ret.vertices:
            ret.inEdges[x], ret.outEdges[x] = ret.outEdges[x], ret.inEdges[x]

        return ret


    def Dijkstra(self, source):
        best = {x: inf for x in self.vertices}

        best[source] = 0
        S = OrderedDict([(best[source], source)])

        while len(S) > 0:
            # TODO: find some ordered data structure
