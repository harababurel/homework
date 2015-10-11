import pickle

v = []

def showPrompt():
    print("Please enter a command. Try \"help\".")


def getInput():
    print("> ", end="")
    command = input().split()

    if len(command) == 0:
        return

    ### INSERT
    if command[0] == 'insert':
        argCount = len(command) - 1

        if not argCount in [1, 2]:
            raise(Exception("Error: <insert> command takes 1 or 2 arguments (%i given)." % argCount))

        try:
            score = float(command[1])
            assert(0.0 <= score and score <= 10.0)
        except:
            raise(Exception("Error: the score must be a real number between 0.0 and 10.0"))


        position = None
        if argCount == 2:
            try:
                position = int(command[2])
                assert(1 <= position and position <= len(v))
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
            raise(Exception("Error: <remove> command takes 1 or 2 arguments (%i given)." % argCount))

        try:
            left = int(command[1])
            assert(1 <= left and left <= len(v))
        except:
            raise(Exception("Error: the position you entered is not valid."))

        right = left
        if argCount == 2:
            try:
                right = int(command[2])
                assert(left <= right and right <= len(v))
            except:
                raise(Exception("Error: the interval you entered is not valid."))

        try:
            remove(left, right)
        except:
            raise(Exception("Something went wrong :(. Could not erase participant%s." % ['', 's'][left != right]))

    ### HELP
    elif command[0] == 'help':
        showHelp()

    ### LIST
    elif command[0] == 'list':
        showList()

    ### EXIT
    elif command[0] == 'exit':
        #TODO: save current state
        print("Exiting...")
        exit(0)

    ### EVERYTHING ELSE
    else:
        raise(Exception(("Command not recognized. Try \"help\".")))


def add(score, position):
    global v

    if position:
        v.insert(position-1, score)
    else:
        v.append(score)


def remove(left, right):
    global v

    v[left-1:right] = []

def showList():
    global v

    print("Participants:\n    %s" % "\n    ".join(["#%i: %.2f" % (i+1, x) for i, x in enumerate(v)]))


def showHelp():
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
    showPrompt()
    while True:
        try:
            getInput()
        except Exception as e:
            print(e)


if __name__ == '__main__':
    main()
