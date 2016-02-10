"""
    Module provides the brains of the application.
    Everything that operates on the user data is
    located here.
"""


def add(history, score, position):
    """
    Parameters:
        - the participants history list
        - the score of a new participant
        - the position of a new participant
    Method takes the score and position of a new participant,
    adds it to the current list of participants, and returns
    the new history.
    """

    history = forgetFuture(history)  # a parallel universe is created
    history = prepareFuture(history) # and time moves forward

    if position:
        history['states'][-1].insert(position-1, score)
    else:
        history['states'][-1].append(score)

    return history


def remove(history, left, right):
    """
    Parameters:
        - the participants history list
        - an interval of consecutive positions
    Method removes the participants contained in the interval
    from the current list of participants, and returns the new
    history.
    """

    history = forgetFuture(history)  # a parallel universe is created
    history = prepareFuture(history) # and time moves forward

    history['states'][-1][left-1:right] = []
    return history


def replaceScore(history, position, score):
    """
    Parameters:
        - the participants history list
        - the position of an existing participant
        - the new score of said participant
    Method updates the score of a certain participant
    and returns the new history list.
    """

    history = forgetFuture(history)  # a parallel universe is created
    history = prepareFuture(history) # and time moves forward

    history['states'][-1][position-1] = score
    return history


def getAverage(history, left, right):
    """
    Parameters:
        - the participants history list
        - an interval of positions
    Method returns the average score of participants
    in given interval.
    """

    candidates = history['states'][-1][left-1:right]
    try:
        average = sum(candidates) / len(candidates)
    except:
        raise(Exception('Error: could not compute average :('))

    return average


def getMinScore(history, left, right):
    """
    Parameters:
        - the participants history list
        - an interval of positions
    Method returns the lowest score of a participant
    in given interval.
    """

    return min(history['states'][-1][left-1:right])


def getMaxScore(history, left, right):
    """
    Parameters:
        - the participants history list
        - an interval of positions
    Method returns the highest score of a participant
    in given interval.
    """

    return max(history['states'][-1][left-1:right])


def getMulMask(history, k, left, right):
    """
    Parameters:
        - the participants history list
        - the "mul" command arguments
    Method returns a mask that filters participants
    which have positions between left and right, and
    the score is a multiple of k.
    """

    mask = [left <= i+1 and i+1 <= right and history['states'][history['now']][i] % k == 0 for i in range(0, len(history['states'][history['now']]))]
    return mask


def getLessMask(history, separator):
    """
    Method returns a mask that filters participants
    which have a score lower than <separator>.
    """
    mask = [x < separator for x in history['states'][history['now']]]
    return mask


def getGreaterMask(history, separator):
    """
    Same as above.
    """
    mask = [x > separator for x in history['states'][history['now']]]
    return mask


def undo(history, steps):
    """
    Method reverts the last <steps> number of operations
    applied to the participants list.
    Returns new history (which is basically same history
    but has an updated "now" field)
    """

    history['now'] = max(0, history['now'] - steps)
    return history


def redo(history):
    """
    Method reapplies the action that cronologically succeeds
    the current state of the application.
    """

    if history['now'] == len(history['states'])-1:
        print("Already at most recent state.")
    else:
        history['now'] += 1

    return history


def forgetFuture(history):
    """
    Method erases all states that exist after the currently indicated one.
    This reduces the history tree to a history chain.
    """

    history['states'] = history['states'][:history['now']+1]
    return history


def prepareFuture(history):
    """
    Method prepares the history for a new state.
    That is, it duplicates the current state,
    and pushes present time one step forward.
    """

    clone = [x for x in history['states'][-1]]

    history['states'].append(clone) # duplicate current state
    history['now'] = len(history['states']) - 1     # set the present time

    return history


def filterList(history, mask):
    """
    Method applies a mask to the history
    and keeps only the values that are not masked.
    Returns updated history.
    """

    history = forgetFuture(history)
    history = prepareFuture(history)

    history['states'][history['now']] = [x for i, x in enumerate(history['states'][history['now']]) if mask[i]]
    return history
