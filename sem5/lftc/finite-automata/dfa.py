import json
from functools import reduce
from state import State


class DFAException(Exception):
    def __init__(self, message):
        super(DFAException, self).__init__(message)


class DeterministicFiniteAutomaton(object):
    def __init__(self, json_encoding):
        body = json.loads(json_encoding)

        initial_states = []
        self.states = {}
        for (name, properties) in body.items():
            initial = properties.get('initial', False)
            final = properties.get('final', False)
            self.states[name] = State(
                name=name,
                initial=initial,
                final=final,
                edges=properties['edges'])

            if initial:
                initial_states.append(name)

        if len(initial_states) != 1:
            raise DFAException(
                "A DeterministicFiniteAutomaton must have exactly one initial "
                "state.")
        else:
            self.initial_state = self.get_state(initial_states[0])

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
