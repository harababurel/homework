from graphviz import Digraph
from dfa import *


class DeterministicFiniteAutomatonDrawer(object):
    def __init__(self):
        pass

    def draw(self, automaton):
        f = Digraph("deterministic_finite_automaton", filename='dfa.gv')

        f.attr(rankdir='LR', size='18')

        for state in automaton.states.values():
            f.attr('node', shape=['circle', 'doublecircle'][state.final])
            f.node(state.name)

        for state in automaton.states.values():
            for (edge_symbol, neighbor_name) in state.edges.items():
                f.edge(state.name, neighbor_name, label=edge_symbol)

        f.render(filename='dfa.gv', directory='/tmp')
