import logging


class State(object):
    def __init__(self, *, name, edges, initial=False, final=False):
        self.name = name
        self.edges = edges
        self.initial = initial
        self.final = final

    def next(self, symbol):
        if symbol in self.edges.keys():
            logging.debug("[%s]-----%s----->[%s]" % (self.name, symbol,
                                                     self.edges[symbol]))
            return self.edges[symbol]
        return None

    def __str__(self):
        if self.initial or self.final:
            return "%s(%s%s)" % (self.name, ['', 'i'][self.initial],
                                 ['', 'f'][self.final])
        return self.name
