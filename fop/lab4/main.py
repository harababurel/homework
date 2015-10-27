"""
    Module imports everything and runs
    the main loop of the application.
"""

from backend import *
from interface import *
from persistence import *
from tests import *


def main():
    try:
        testEverything()
        print("All tests passed :).")
    except Exception as e:
        print(e)
        print("Terminating execution.")
        exit(0)

    history = restoreSession()
    showPrompt()

    while True:
        #print("%r" % history)
        try:
            history = getInput(history)
        except Exception as e:
            print(e)


if __name__ == '__main__':
    main()
