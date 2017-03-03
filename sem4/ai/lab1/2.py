#!/bin/python3
import string
import random
import itertools
import numpy

# 2. Lottery
# We have n balls, p from them are white, and the rest blue. Each ball has a label of
# character type (letter of the alphabet). Check all the possibilities to select k balls, at least
# a of them have to be white, and at least one white ball has to be tagged
# with a vowel.

# p   white
# n-p blue
# k   extracted
# a   should be white


class Lottery:

    def __init__(self, n, p, k, a):
        self.whiteQty = p
        self.blueQty = n - p
        self.extractQty = k
        self.whiteThreshold = a
        self.balls = []

    def createBalls(self):
        for i in range(self.whiteQty):
            self.balls.append(Ball("White", getRandomLetter()))

        for i in range(self.blueQty):
            self.balls.append(Ball("Blue", getRandomLetter()))

    def getRandomSolution(self):
        n = len(self.balls)
        indices = sorted(random.sample(range(n), self.extractQty))
        return tuple(self.balls[i] for i in indices)

    def generateNSolutions(self, n):
        for i in range(n):
            yield self.getRandomSolution()

    def solutionIsValid(self, sol):
        return self.solutionFitness(sol) == 0

    def solutionFitness(self, sol):
        whites = list(filter(lambda ball: ball.isWhite(), sol))
        whitesWithVowels = list(filter(lambda ball: ball.hasVowel(), whites))

        fitness = max(0, self.whiteThreshold - len(whites)) + \
            (1 if whitesWithVowels == [] else 0)
        return fitness

    def computeMeanAndStd(fitnesses):
        arr = numpy.array(fitnesses)

        mean = numpy.mean(arr, axis=0)
        std = numpy.std(arr, axis=0)
        return (mean, std)



class Ball:

    def __init__(self, color, letter):
        self.color = color
        self.letter = letter
    def __repr__(self):
        return "Ball(%5s, '%c')" % (self.color, self.letter)

    def isWhite(self):
        return self.color == "White"

    def hasVowel(self):
        return self.letter in 'aeiou'


def getRandomLetter():
    return random.choice(string.ascii_lowercase)




def main():
    n, p, k, a = map(int, input().split())
    lottery = Lottery(n, p, k, a)
    lottery.createBalls()

    print(lottery.balls)

    fitnesses = []
    for sol in lottery.generateNSolutions(25):
        currentFitness = lottery.solutionFitness(sol)
        print("%5r (fitness = %2i): %r)" %
              (lottery.solutionIsValid(sol), currentFitness, sol))
        fitnesses.append(currentFitness)

    print("\nFitnesses: %s" % ' '.join(map(str, fitnesses)))

    mean, std = Lottery.computeMeanAndStd(fitnesses)
    print("Mean: %.2f" % mean)
    print("Std:  %.2f" % std)


if __name__ == '__main__':
    main()
