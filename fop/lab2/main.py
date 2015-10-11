import pickle

v = []

def showPrompt():
    print("Please enter a command. Try \"help\".")

def getInput():
    print("> ", end="")
    command = input().split()

    if len(command) == 0:
        #showPrompt()
        return
    if command[0] == 'insert':
        argCount = len(command) - 1

        if not argCount in [1, 2]:
            raise(Exception("Error: <insert> command takes 1 or 2 arguments (%i given)" % argCount))

        try:
            score = float(command[1])
            assert(0.0 <= score and score <= 10.0)
        except:
            raise(Exception("Error: the score must be a real number between 0.0 and 10.0"))


        position = None
        if argCount == 2:
            try:
                position = int(command[2])
                assert(1 <= position) # TODO: position <= sizeOfList
            except:
                raise(Exception("Error: if you want to pass a custom position, make sure it's an integer between 1 and the total number of participants."))

        try:
            add(score, position)
        except:
            raise(Exception("Something went wrong :(. Could not add participant."))
    elif command[0] == 'help':
        showHelp()
    else:
        print("Command not recognized. Try \"help\".")
        #print("> ", end='')
        return


def add(score, position):
    global v

    if position:
        v.insert(position-1, score)
    else:
        v.append(score)


def showHelp():
    print("These are the possible commands:")
    print("    help - displays this prompt")
    print("    insert X - adds a new participant with score X")
    print("    insert X Y - adds a new participant with score X at position Y")
    print("    remove X - removes participant at position X")
    print("    remove X Y - removes participants with positions between X and Y")
    print("    replace X Y - replaces the score of the participant at position X with the score Y")
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
