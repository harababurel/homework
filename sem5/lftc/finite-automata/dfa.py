from graphviz import Digraph
from functools import reduce
from state import State
import json
import os.path
import logging


class DFAException(Exception):
    def __init__(self, message):
        super(DFAException, self).__init__(message)


class DeterministicFiniteAutomaton(object):
    def __init__(self, json_encoding):
        body = json.loads(json_encoding)

        self.states = {}
        for (name, properties) in body.items():
            initial = properties.get('initial', False)
            final = properties.get('final', False)
            self.states[name] = State(
                name=name,
                initial=initial,
                final=final,
                edges=properties['edges'])

        self.set_initial_state()
        self.verify_edges()

    def set_initial_state(self):
        initial_states = [
            state for state in self.states.values() if state.initial
        ]
        if len(initial_states) != 1:
            raise DFAException(
                "A DeterministicFiniteAutomaton must have exactly one initial "
                "state.")
        else:
            self.initial_state = initial_states[0]

    def verify_edges(self):
        for state in self.states.values():
            for (symbol, target) in state.edges.items():
                if target not in self.states.keys():
                    raise DFAException(
                        "Edge [%s]--%s-->[%s] is invalid because node [%s] "
                        "does not exist." % (state.name, symbol, target,
                                             target))

    def __str__(self):
        states = ', '.join([str(state) for state in self.states.values()])
        alphabet = ', '.join(self.alphabet())

        ret = "States: %s\n" % states
        ret += "Alphabet: %s\n" % alphabet

        return ret

    def alphabet(self):
        return reduce(
            lambda x, y: x | y,
            [set(state.edges.keys()) for state in self.states.values()], set())

    def get_state(self, name):
        if name in self.states:
            return self.states[name]
        return None

    def longest_accepted_prefix(self, sequence):
        state = self.initial_state
        best_i = 0
        for (i, character) in enumerate(sequence):
            state = self.get_state(state.next(character))

            if state is None:
                break

            if state.final:
                best_i = i

        return sequence[:best_i + 1]

    def accepts(self, sequence):
        return self.longest_accepted_prefix(sequence) == sequence

    def draw(self, filepath='/tmp/dfa.gv'):
        graph = Digraph(format='png')
        graph.attr(rankdir='LR', dpi='209')

        for state in self.states.values():
            graph.attr('node', shape=['circle', 'doublecircle'][state.final])
            graph.node(state.name)

        for state in self.states.values():
            for (edge_symbol, neighbor_name) in state.edges.items():
                graph.edge(state.name, neighbor_name, label=edge_symbol)

        try:
            (directory, filename) = os.path.split(filepath)
            graph.render(filename=filename, directory=directory)
        except Exception as e:
            logging.error("Could not render graph: %s", e)
