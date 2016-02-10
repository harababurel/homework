import pickle

participants = None    # list of participants
history = None         # all previous states of the participants list


def showPrompt():
    print("Please enter a command. Try \"help\".")


def restoreSession():
    """
    Method tries to open previously initiated session, saved on disk.
    If there is none, then a new session is created from scratch.
    """
    global participants, history

    print("Restoring previous session.")
    try:
        with open('data.bin', 'rb') as f:
            history = pickle.load(f)
            participants = history[-1]
    except:
        print("Could not restore session. Starting from scratch.")

        participants = []
        history = [[]]
        saveSession()


def saveSession():
    """
    Method saves current state of the application to disk.
    Objects that are saved: history.
    If unable to do so, a message is displayed
    """
    global participants, history

    print("Saving new session to disk.")
    try:
        with open('data.bin', 'wb') as g:
            pickle.dump(history, g)
        print("Session saved.")
    except:
        print("Could not save session to disk. Check file permissions.")


def getInput():
    """
    Method reads one command and processes it.
    If valid, then its associated functionality is run.
    Otherwise, an exception is raised.
    """
    print("> ", end="")
    command = input().split()

    if len(command) == 0:  # no command means
        return             # nothing to do

    ### INSERT
    if command[0] == 'insert':
        argCount = len(command) - 1

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
                assert(1 <= position and position <= len(participants))
            except:
                raise(Exception("Error: position must be an integer between 1 and the total number of participants."))

        try:
            add(score, position)
        except:
            raise(Exception("Something went wrong :(. Could not add participant."))

    ### REMOVE
    elif command[0] == 'remove':
        argCount = len(command) - 1

        if not argCount in [1, 2]:
            raise(Exception("Error: <remove> takes 1 or 2 arguments (%i given)." % argCount))

        try:
            left = int(command[1])
            assert(1 <= left and left <= len(participants))
        except:
            raise(Exception("Error: the position you entered is not valid."))

        right = left
        if argCount == 2:
            try:
                right = int(command[2])
                assert(left <= right and right <= len(participants))
            except:
                raise(Exception("Error: the interval you entered is not valid."))

        try:
            remove(left, right)
        except:
            raise(Exception("Something went wrong :(. Could not erase participant%s." % ['', 's'][left != right]))

    ### REPLACE
    elif command[0] == 'replace':
        argCount = len(command) - 1

        if argCount != 2:
            raise(Exception("Error: <replace> takes exactly 2 arguments (%i given)." % argCount))

        try:
            position = int(command[1])
            assert(1 <= position and position <= len(participants))
        except:
            raise(Exception("Error: position must be an integer between 1 and the total number of participants."))

        try:
            score = int(command[2])
            assert(0 <= score and score <= 100)
        except:
            raise(Exception("Error: the score must be an integer between 0 and 100."))

        try:
            replaceScore(position, score)
        except:
            raise(Exception("Something went wrong :(. Could not replace score."))

    ### HELP
    elif command[0] == 'help':
        showHelp()

    ### LIST
    elif command[0] == 'list':
        showList()

    ### EXIT
    elif command[0] == 'exit':
        saveSession()
        print("Exiting...")
        exit(0)

    ### EVERYTHING ELSE
    else:
        raise(Exception(("Command not recognized. Try \"help\".")))


def add(score, position):
    """
    Method takes the score and position of a new participant
    and adds it to the list.
    """
    global participants

    if position:
        participants.insert(position-1, score)
    else:
        participants.append(score)


def remove(left, right):
    """
    Method takes an interval of consecutive positions
    and removes the participants contained in it.
    """
    global participants

    participants[left-1:right] = []


def replaceScore(position, score):
    """
    Method takes the position of a participant
    and assigns a new score to it.
    """

    participants[position-1] = score


def showList():
    """
    Method prints all participants and their positions.
    """

    print("Participants:\n    %s" % "\n    ".join(["#%i: %i" % (i+1, x) for i, x in enumerate(participants)]))


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
    print("    exit - saves the current state and closes the program")


def main():
    restoreSession()
    showPrompt()

    while True:
        try:
            getInput()
        except Exception as e:
            print(e)


if __name__ == '__main__':
    main()
