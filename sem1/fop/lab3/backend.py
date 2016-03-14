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

    if position:
        history[-1].insert(position-1, score)
    else:
        history[-1].append(score)

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

    history[-1][left-1:right] = []
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

    history[-1][position-1] = score
    return history


def getAverage(history, left, right):
    """
    Parameters:
        - the participants history list
        - an interval of positions
    Method returns the average score of participants
    in given interval.
    """

    candidates = history[-1][left-1:right]
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

    return min(history[-1][left-1:right])


def getMaxScore(history, left, right):
    """
    Parameters:
        - the participants history list
        - an interval of positions
    Method returns the highest score of a participant
    in given interval.
    """

    return max(history[-1][left-1:right])


def getMulMask(history, k, left, right):
    """
    Parameters:
        - the participants history list
        - the "mul" command arguments
    Method returns a mask that filters participants
    which have positions between left and right, and
    the score is a multiple of k.
    """

    mask = [left <= i+1 and i+1 <= right and history[-1][i] % k == 0 for i in range(0, len(history[-1]))]
    return mask
