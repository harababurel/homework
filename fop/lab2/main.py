import pickle


def showPrompt():
    print("Please enter a command. Try \"help\" if you need it.")
    print("> ", end="")

    command = input().split()

    if len(command) == 0:
        # do nothing
        return
    if command[0] == 'insert':
        argCount = len(command) - 1

        if not argCount in [1, 2]:
            print("Error: <insert> command takes 1 or 2 arguments (%i given)" % argCount)

        try:
            score = float(command[1])
            assert(0.0 <= score and score <= 10.0)
        except:
            raise(Exception("Error: the score must be a real number between 0.0 and 10.0"))


        if argCount == 2:
            try:
                position = int(command[2])
                assert(







def showHelp():
    print("These are the possible commands:")
    print("    insert X - adds a new participant with score X"
    print("    insert X Y - adds a new participant with score X at position Y")
    print("    remove X - removes participant at position X")
    print("    remove X Y - removes participants with positions between X and Y")
    print("    replace X Y - replaces the score of the participant at position X with the score Y")
