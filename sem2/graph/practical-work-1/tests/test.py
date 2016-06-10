from models.digraph import DiGraph

class Test:
    def testConnectedComponents(self):
        G = DiGraph(undirected=True, DEBUG=False)
        G.populateFromFile('samples/connected-components-infoarena.txt')

        assert G.connectedComponents() == [[1, 2, 4], [3, 5], [6]]
