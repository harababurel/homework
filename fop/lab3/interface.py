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

    neededArgs = {
            'insert':  [1, 2],
            'remove':  [1, 2],
            'replace': [2],
            'help':    [0],
            'list':    [0],
            'less':    [1],
            'greater': [1],
            'sorted':  [0],
            'average': [2],
            'min':     [2],
            'max':     [2],
            'mul':     [3],
            'exit':    [0]
            }

    ### NOTHING
    if len(command) == 0:  # no command means
        return history     # nothing to do

    if command[0] in neededArgs:
        if not argCount in neededArgs[command[0]]:
            raise(Exception("Error: <%s> takes %r arguments (%i provided)." % (command[0], neededArgs[command[0]], argCount)))

    ### INSERT
    if command[0] == 'insert':
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

    ### LESS or GREATER
    elif command[0] in ['less', 'greater']:
        try:
            separator = int(command[1])
        except:
            raise(Exception("Error: the score must be an integer."))

        if command[0] == 'less':
            mask = [x < separator for x in history[-1]]
        else:
            mask = [x > separator for x in history[-1]]

        showList(history, mask)

    ### SORTED
    elif command[0] == 'sorted':
        sortedParticipants = [sorted(history[-1])[::-1]]
        showList(sortedParticipants, [True for x in sortedParticipants[0]])

    ### AVERAGE, MIN, MAX
    elif command[0] in ['average', 'min', 'max']:
        try:
            left = int(command[1])
            right = int(command[2])
            assert(1 <= left and left <= right and right <= len(history[-1]))
        except:
            raise(Exception("Error: the interval you provided is not valid."))

        if command[0] == 'average':
            print("The average score is %.2f." % getAverage(history, left, right))
        elif command[0] == 'min':
            print("The lowest score is %i." % getMinScore(history, left, right))
        elif command[0] == 'max':
            print("The highest score is %i." % getMaxScore(history, left, right))

    ### MUL
    elif command[0] == 'mul':
        try:
            k = int(command[1])
            assert k > 0
        except:
            raise(Exception("Error: the first argument of <mul> must be an integer greater than 0."))

        try:
            left = int(command[2])
            right = int(command[3])
            assert 1 <= left and left <= right and right <= len(history[-1])
        except:
            raise(Exception("Error: the interval you provided is not valid."))

        showList(history, getMulMask(history, k, left, right))



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
    print("    average X Y - shows the average score of participants with positions between X and Y")
    print("    min X Y - shows the lowest score of participants with positions between X and Y")
    print("    max X Y - shows the highest score of participants with positions between X and Y")
    print("    mul K X Y - shows scores that are a multiple of K, with positions between X and Y")
    print("    exit - saves the current state and closes the program")


def showList(history, mask):
    """
    Method prints all participants and their positions.
    """

    print("Participants:\n    %s" % "\n    ".join([["######", "#%i: %i" % (i+1, x)][mask[i]] for i, x in enumerate(history[-1])]))
