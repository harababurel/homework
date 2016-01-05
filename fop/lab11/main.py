"""
9. The sequence a = a1, ..., an with distinct integer elements is given.
Determine all subsets of at least two elements with the property:
    The elements in the subset are in increasing order;
    Any two consecutive elements in the subsequence have at least one common digit.
"""

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

    for x in v:                     # try to use each element on the current level
        if valid(stack, x):         # if said element is ok
            stack.append(x)

            if len(stack) >= 2:     # print the sequence
                print(stack)

            back(v, level+1, stack) # and continue building the next level
            stack.pop()

back([2346, 654920, 2342, 1511, 654], 0, [])
back([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], 0, [])
