#!/bin/python3

n, m = map(int, input().split())

edges = []
for _ in range(m):
    a, b, _ = map(int, input().split())
    edges.append((a, b))

nodes = ','.join(["{\"id\":\"%d\"}" % i for i in range(n)])
edges = ','.join(["{\"source\":\"%d\", \"target\":\"%d\"}" % x for x in edges])

out = """
{
  "graph": {
    "nodes": [%s],
    "edges": [%s]
  }
}
""" % (nodes, edges)

print(out)
