from random import randint

def sorter(xs, compare):
    """
    Method takes a list xs and returns its sorted permutation,
    According to the relation given by the comparison function.

    Algorithm: patience sorting.

    Runtime complexity: O(n^2) with current implementation
                        O(n log n) with the k-merge optimization.
    """

    piles = []                                                # no piles initially
    for x in xs:                                              # take each element
        placed = False                                        # assume that it's not placed in any pile

        for pile in piles:
            if len(pile) >= 1 and compare(x, pile[-1]):       # place it in the leftmost pile that has
                pile.append(x)                                # the top element greater than or equal to x
                placed = True
                break

        if not placed:                                        # if no such pile exists
            piles.append([x])                                 # create a new one with just the current x


    ys = []                                                   # we will construct a merge of all piles
    for i in range(len(xs)):
        minimum = None                                        # at each step, choose the smallest visible x
        chosenPile = 0                                        # and remove it from the pile
        for i, pile in enumerate(piles):
            if len(pile) >= 1 and (minimum is None or (minimum is not None and compare(pile[-1], minimum))):
                minimum = pile[-1]
                chosenPile = i

        ys.append(minimum)
        piles[chosenPile].pop()
    return ys

def filterer(xs, isBueno):
    """
    Method takes a list xs and a validation method isBueno.
    Returns a new list that contains the elements of xs
    that are valid according to the given method.
    """
    return [x for x in xs if isBueno(x)]


"""
print(sorter([6, 4, 3, 8, 0, 1, 7, 5], lambda x, y: x <= y))
print(filterer([6, 4, 3, 8, 0, 1, 7, 5], lambda x: x % 2 == 1))

for test in range(100):
    n = randint(1000, 10000)
    xs = []

    print("Generating test #%i" % test)
    for i in range(n):
        xs.append(randint(-10**9, 10**9))

    print("Testing #%i" % test)
    assert sorter(xs, lambda x, y: x <= y) == sorted(xs)
    print("Testcase #%i: OK" % test)
"""
