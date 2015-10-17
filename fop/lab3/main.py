"""
    Module imports everything and runs
    the main loop of the application.
"""

from backend import *
from interface import *
from persistence import *


def main():
    history = restoreSession()
    showPrompt()

    while True:
        try:
            history = getInput(history)
        except Exception as e:
            print(e)


if __name__ == '__main__':
    main()
