import pickle

def restoreSession():
    """
    Method tries to open previously initiated session, saved on disk.
    If there is none, then a new session is created from scratch.

    Returns a list (history) of lists, containing all states
    of the participants list, in cronological order.
    """

    print("Restoring previous session.")
    try:
        with open('data.bin', 'rb') as f:
            history = pickle.load(f)
    except:
        print("Could not restore session. Starting from scratch.")

        history = [[]]
        saveSession(history)

    return history


def saveSession(history):
    """
    Method saves current state of the application to disk.
    Objects that are saved: history.
    If unable to do so, a message is displayed.
    """

    print("Saving new session to disk.")
    try:
        with open('data.bin', 'wb') as g:
            pickle.dump(history, g)
        print("Session saved.")
    except:
        print("Could not save session to disk. Check file permissions.")
