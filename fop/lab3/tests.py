"""
    Module provides test functions for the back-end methods
    implemented elsewhere.
"""
from backend import *

def testAdd():
    assert add([[]], 5, None) == [[5]]
    assert add([[100, 54], [100, 54, 32]], 1, 2) == [[100, 54], [100, 1, 54, 32]]
    assert add([[1, 2, 3, 4]], 5, 4) == [[1, 2, 3, 5, 4]]

    try: # some invalid commands
        add([[]], 5, 9)
    except:
        pass

    try:
        add([[]], -5)
    except:
        pass

    try:
        add([[0]], 'asdf')
    except:
        pass


def testRemove():
    assert remove([[1, 2, 3, 4]], 1, 4) == [[]]
    assert remove([[1, 2, 3, 4]], 2, 3) == [[1, 4]]
    assert remove([[1], [1, 100]], 2, 2) == [[1], [1]]

    try:
        remove([[1, 2, 3, 4]], 5, 100)
    except:
        pass

    try:
        remove([[]], 1, 1)
    except:
        pass

    try:
        remove([[], [], []], 'lica', 'samadaul')
    except:
        pass


def testReplaceScore():
    assert replaceScore([[1]], 1, 100) == [[100]]
    assert replaceScore([[2], [2, 2]], 2, 100) == [[2], [2, 100]]
    assert replaceScore([[69, 69, 68, 69]], 3, 69) == [[69, 69, 69, 69]]

    try:
        replaceScore([[]], 1, 13)
    except:
        pass

    try:
        replaceScore([[1, 23]], 3, 100)
    except:
        pass

    try:
        replaceScore([[100]], 1, 101)
    except:
        pass


def testGetAverage():
    assert getAverage([[1, 2, 3, 4]], 1, 4) == 2.5
    assert getAverage([[1, 2, 3]], 1, 3) == 2.0
    assert getAverage([[1], [100, 0]], 1, 2) == 50.0

    try:
        getAverage([[]], 1, 1)
    except:
        pass

    try:
        getAverage([[1]], 'mama', 'tata')
    except:
        pass

    try:
        getAverage([[1, 2, 3, 4, 5, 6]], 1, 7)
    except:
        pass


def testGetMinScore():
    assert getMinScore([[1, 2, 3, 4]], 1, 4) == 1
    assert getMinScore([[1, 2, 3]], 1, 3) == 1
    assert getMinScore([[1], [100, 0]], 1, 2) == 0

    try:
        getMinScore([[]], 1, 1)
    except:
        pass

    try:
        getMinScore([[1, 2, 3]], 0, 2)
    except:
        pass


def testGetMaxScore():
    assert getMaxScore([[1, 2, 3, 4]], 1, 4) == 4
    assert getMaxScore([[1, 2, 3]], 1, 3) == 3
    assert getMaxScore([[1], [100, 0]], 1, 2) == 100

    try:
        getMaxScore([[]], 1, 1)
    except:
        pass

    try:
        getMaxScore([[1, 2, 3]], 0, 2)
    except:
        pass


def testGetMulMask():
    assert getMulMask([[10, 50, 51, 52]], 10, 1, 4) == [True, True, False, False]
    assert getMulMask([[10, 50, 51, 52]], 3, 1, 4) == [False, False, True, False]
    assert getMulMask([[]], 100, 1, 1) == []


def testEverything():
    problem = None

    try:
        testAdd()
    except:
        print("<add> method failed tests :(")
        problem = True


    try:
        testRemove()
    except:
        print("<remove> method failed tests :(")
        problem = True

    try:
        testReplaceScore()
    except:
        print("<replaceScore> method failed tests :(")
        problem = True

    try:
        testGetAverage()
    except:
        print("<getAverage> method failed tests :(")
        problem = True

    try:
        testGetMinScore()
    except:
        print("<getMinScore> method failed tests :(")
        problem = True

    try:
        testGetMaxScore()
    except:
        print("<getMaxScore> method failed tests :(")
        problem = True

    try:
        testGetMulMask()
    except:
        print("<getAverage> method failed tests :(")
        problem = True

    if problem is not None:
        raise(Exception("Some tests failed :("))
