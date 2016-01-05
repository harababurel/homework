"""
9. The sequence a = a1, ..., an with distinct integer elements is given.
Determine all subsets of at least two elements with the property:
    The elements in the subset are in increasing order;
    Any two consecutive elements in the subsequence have at least one common digit.
"""
from random import randint
from copy import deepcopy

def valid(stack, x):
    if len(stack) == 0:             # any element can be added
        return True                 # to an empty list

    if x <= stack[-1]:              # increasing order
        return False

    if len([digit for digit in str(x) if digit in str(stack[-1])]) == 0:
        return False                # at least one common digit

    return True


def back(v, level, stack):
    if level == len(v):             # reached the end of the given list
        return                      # nothing else to do

    for x in v:
        if valid(stack, x):
            stack.append(x)

            if len(stack) >= 2:
                print('\t', stack)

            back(v, level+1, stack)
            stack.pop()

def backIter(v, level, stack):
    S = [(v, level, stack)]

    while len(S) > 0:
        top = S.pop()
        v, level, stack = top

        if level == len(v):
            continue

        for x in v:
            if valid(stack, x):
                stack.append(deepcopy(x))

                if len(stack) >= 2:
                    print('\t', stack)

                S.append((deepcopy(v), level+1, deepcopy(stack)))
                stack.pop()


#backIter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 0, [])

print("Rec:")
back([2346, 654920, 2342, 1511, 654], 0, [])
print("Iter:")
backIter([2346, 654920, 2342, 1511, 654], 0, [])

print()
print("Rec:")
back([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 0, [])
print("Iter:")
backIter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 0, [])

for test in range(10):
    n = randint(2, 10)
    v = list(set([(randint(1, 10), randint(10, 100), randint(100, 1000))[randint(0, 2)] for i in range(n)]))

    print("Test #%i: %r" % (test+1, v))

    print("Rec:")
    back(v, 0, [])
    print()

    print("Iter:")
    backIter(v, 0, [])
    print()
