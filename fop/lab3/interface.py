"""
    Module provides user interface methods
    (that is, it processes user input and runs
    the needed backend methods, and outputs
    the results).
"""

from backend import *
from persistence import *

def showPrompt():
    print("Please enter a command. Try \"help\".")


def getInput(history):
    """
    Method reads one command and processes it.
    If valid, then its associated functionality is run
    and the new history list is returned.
    Otherwise, an exception is raised.
    """
    print("> ", end="")
    command = input().split()
    argCount = len(command) - 1

    ### NOTHING
    if len(command) == 0:  # no command means
        return history     # nothing to do

    ### INSERT
    if command[0] == 'insert':
        if not argCount in [1, 2]:
            raise(Exception("Error: <insert> takes 1 or 2 arguments (%i given)." % argCount))

        try:
            score = int(command[1])
            assert(0 <= score and score <= 100)
        except:
            raise(Exception("Error: the score must be an integer between 0 and 100."))


        position = None
        if argCount == 2:
            try:
                position = int(command[2])
                assert(1 <= position and position <= len(history[-1]))
            except:
                raise(Exception("Error: position must be an integer between 1 and the total number of participants."))

        try:
            history = add(history, score, position)
        except:
            raise(Exception("Something went wrong :(. Could not add participant."))

    ### REMOVE
    elif command[0] == 'remove':
        if not argCount in [1, 2]:
            raise(Exception("Error: <remove> takes 1 or 2 arguments (%i given)." % argCount))

        try:
            left = int(command[1])
            assert(1 <= left and left <= len(history[-1]))
        except:
            raise(Exception("Error: the position you entered is not valid."))

        right = left
        if argCount == 2:
            try:
                right = int(command[2])
                assert(left <= right and right <= len(history[-1]))
            except:
                raise(Exception("Error: the interval you entered is not valid."))

        try:
            history = remove(history, left, right)
        except:
            raise(Exception("Something went wrong :(. Could not erase participant%s." % ['', 's'][left != right]))

    ### REPLACE
    elif command[0] == 'replace':
        if argCount != 2:
            raise(Exception("Error: <replace> takes exactly 2 arguments (%i given)." % argCount))

        try:
            position = int(command[1])
            assert(1 <= position and position <= len(history[-1]))
        except:
            raise(Exception("Error: position must be an integer between 1 and the total number of participants."))

        try:
            score = int(command[2])
            assert(0 <= score and score <= 100)
        except:
            raise(Exception("Error: the score must be an integer between 0 and 100."))

        try:
            history = replaceScore(history, position, score)
        except:
            raise(Exception("Something went wrong :(. Could not replace score."))

    ### HELP
    elif command[0] == 'help':
        showHelp()

    ### LIST
    elif command[0] == 'list':
        showList(history, [True for i in range(0, len(history[-1]))]) # this mask corresponds to "show all"

    ### LESS
    elif command[0] == 'less':
        if argCount != 1:
            raise(Exception("Error: <less> takes exactly 1 argument (%i) given." % argCount))

        try:
            separator = int(command[1])
        except:
            raise(Exception("Error: the score must be an integer."))

        mask = [x < separator for x in history[-1]]
        showList(history, mask)


    ### EXIT
    elif command[0] == 'exit':
        saveSession(history)
        print("Exiting...")
        exit(0)

    ### EVERYTHING ELSE
    else:
        raise(Exception(("Command not recognized. Try \"help\".")))

    return history # return the updated list


def showHelp():
    """
    Method shows a prompt containing all valid commands.
    """

    print("These are the possible commands:")
    print("    help - displays this prompt")
    print("    insert X - adds a new participant with score X")
    print("    insert X Y - adds a new participant with score X at position Y")
    print("    remove X - removes participant at position X")
    print("    remove X Y - removes participants with positions between X and Y")
    print("    replace X Y - replaces the score of the participant at position X with the score Y")
    print("    list - shows all participants")
    print("    less X - shows participants with score lower than X")
    print("    greater X - shows participants with score greater than X")
    print("    sorted - shows participants in ascending score order")
    print("    exit - saves the current state and closes the program")


def showList(history, mask):
    """
    Method prints all participants and their positions.
    """

    print("Participants:\n    %s" % "\n    ".join([["######", "#%i: %i" % (i+1, x)][mask[i]] for i, x in enumerate(history[-1])]))
