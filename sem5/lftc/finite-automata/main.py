import argparse
import logging
from dfa import *
from dfa_drawer import *

parser = argparse.ArgumentParser()
parser.add_argument(
    "-l", "--log", help="log level (DEBUG, INFO, WARNING, ERROR, CRITICAL")


def main():
    args = parser.parse_args()
    if args.log:
        logging.basicConfig(level=getattr(logging, args.log.upper()))

    filename = 'data/1.json'
    try:
        with open(filename, 'r') as f:
            encoding = f.read()
    except Exception as e:
        logging.critical("Could not open %s: %s" % (filename, e))
        exit(1)

    try:
        automaton = DeterministicFiniteAutomaton(encoding)
    except DFAException as e:
        logging.error("Could not construct automaton: %s", e)
        exit(1)

    logging.info("Automaton:\n%s", automaton)

    sequence = "bababaaaaba"
    logging.info(automaton.longest_accepted_prefix(sequence))
    logging.info(automaton.accepts(sequence))

    drawer = DeterministicFiniteAutomatonDrawer()
    drawer.draw(automaton)


if __name__ == '__main__':
    main()
