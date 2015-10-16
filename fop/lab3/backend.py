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
