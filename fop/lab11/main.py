"""
9. The sequence a = a1, ..., an with distinct integer elements is given.
Determine all subsets of at least two elements with the property:
    The elements in the subset are in increasing order;
    Any two consecutive elements in the subsequence have at least one common digit.
"""
from random import randint
from copy import deepcopy

def valid(stack, x):
    """
    Method checks whether an element `x` can be appended to a
    partial solution `stack`. It checks for
        1) increasing order in the sequence
        2) common digits with the last element
    """
    if len(stack) == 0:             # any element can be added
        return True                 # to an empty list

    if x <= stack[-1]:
        return False

    if len([digit for digit in str(x) if digit in str(stack[-1])]) == 0:
        return False

    return True

def back(v, level, stack, solutions):
    if level == len(v):             # reached the end of the given list
        return                      # nothing else to do

    for x in v:
        if valid(stack, x):
            if len(stack + [deepcopy(x)]) >= 2:
                # print('\t', stack + [deepcopy(x)])
                solutions.append(stack + [deepcopy(x)])

            back(v, level+1, stack + [deepcopy(x)], solutions)

    return solutions

def backIter(v, level, stack, iterSols):
    S = [(v, level, stack)]

    while len(S) > 0:
        top = S.pop()
        v, level, stack = top

        if level == len(v):
            continue

        for x in v:
            if valid(stack, x):
                if len(stack + [deepcopy(x)]) >= 2:
                    # print('\t', stack + [deepcopy(x)])
                    iterSols.append(stack + [deepcopy(x)])

                S.append((v, level+1, stack + [deepcopy(x)]))

    return iterSols


def tester():
    """
    Method generates random testcases and runs both the recursive and
    the iterative backtracking algorithms on them.
    """
    recSols = sorted(back([2346, 654920, 2342, 1511, 654], 0, [], []))
    iterSols = sorted(backIter([2346, 654920, 2342, 1511, 654], 0, [], []))

    assert recSols == iterSols

    recSols = sorted(back([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 0, [], []))
    iterSols = sorted(backIter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 0, [], []))

    for test in range(50):
        n = randint(2, 10)
        v = list(set([(randint(1, 10), randint(10, 100), randint(100, 1000))[randint(0, 2)] for i in range(n)]))

        print("Test #%i: %r: " % (test+1, v), end="")

        recSols = sorted(back(v, 0, [], []))
        iterSols = sorted(backIter(v, 0, [], []))

        assert recSols == iterSols
        print("OK")

if __name__ == '__main__':
    tester()
