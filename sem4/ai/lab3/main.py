#!/usr/bin/python3
from math import pi, sin, exp, sqrt
from random import uniform, random
import itertools


# 14. Cross-in-tray function
#
# Find the optimum point (minimum) for the Cross-in-tray function in the domain
# -10 ≤ x, y ≤ 10. The function is:
# f(x, y) = -0.0001 * (abs(sin x * sin y * exp(100 - sqrt(x**2 + y**2) / pi)) + 1) ** 0.1


def f(x, y):
    return -0.0001 * (abs(sin(x) * sin(y) * exp(100 - sqrt(x**2 + y**2) / pi)) + 1) ** 0.1

def eventWithProbability(p):
    return random() <= p

class Individual:
    def __init__(self, x=None, y=None):
        if x is None:
            x = uniform(Problem.nMin, Problem.nMax)

        if y is None:
            y = uniform(Problem.nMin, Problem.nMax)

        self.x = x
        self.y = y

        self.age = 0

        self.fitness = 0.0
        self.updateFitness()

        assert (Problem.nMin <= x and x <= Problem.nMax)
        assert (Problem.nMin <= y and y <= Problem.nMax)

    def __repr__(self):
        return "Individual(x=%.6f, y=%.5f, fitness=%.5f)" \
                % (self.x, self.y, self.getFitness())

    def updateFitness(self):
        self.fitness = -f(self.x, self.y)

    def getFitness(self):
        return self.fitness

    def mutate(self, probability):
        if eventWithProbability(probability):
            self.x += uniform(-Problem.mutationStep, Problem.mutationStep)
        if eventWithProbability(probability):
            self.y += uniform(-Problem.mutationStep, Problem.mutationStep)

        self.updateFitness()

    def crossover(self, other, probability):
        # if eventWithProbability(probability):
        #     offspring = Individual(self.x, other.y)
        # else:
        #     offspring = Individual(other.x, self.y)

        offspring = None
        if eventWithProbability(probability):
            offspring = Individual((self.x + other.x) / 2, (self.y + other.y) / 2)
            offspring.mutate(Problem.mutationProbability)

        return offspring

    def __lt__(self, other):
        return self.getFitness() < other.getFitness()


class Population:
    def __init__(self, size):
        self.individuals = [Individual() for _ in range(size)]

    def evaluate(self):
        for x in self.individuals:
            x.updateFitness()

    def selection(self, remaining=None, ratioRemaining=None):
        if ratioRemaining is not None:
            remaining = max(2, int(ratioRemaining * self.size()))

        self.individuals = sorted(self.individuals, \
                                  key=lambda x: x.getFitness(), \
                                  reverse=True)[:remaining]

    def bestIndividual(self):
        return max(self.individuals)

    def size(self):
        return len(self.individuals)

    def incrementAge(self):
        for i in range(len(self.individuals)):
            self.individuals[i].age += 1
            if self.individuals[i].age > Problem.lifeExpectancy:
                self.individuals[i] = None

        self.cleanup()

    def cleanup(self):
        self.individuals = [x for x in self.individuals if x is not None]


class Problem:
    nMin = -10
    nMax = 10
    step = 0.0000001
    mutationStep = step * 0.1
    mutationProbability = 0.001
    crossoverProbability = 0.1
    lifeExpectancy = 4

    initialPopulation = 30
    generations = 100
    tests = 30

class Algorithm:
    def __init__(self, n):
        self.population = Population(n)

    def iteration(self):
        pairs = itertools.product(self.population.individuals, \
                                  self.population.individuals)

        self.population.incrementAge()

        newIndividuals = [x.crossover(y, Problem.crossoverProbability) for (x, y) in pairs]

        self.population.individuals.extend([x for x in newIndividuals if x is not None])
        self.population.selection(remaining = 100)

    def run(self, generations):
        for i in range(generations):
            self.iteration()

            # print("Best individual in generation %i: %r" % (i+1, self.population.bestIndividual()))

    def statistics(self, generations, tests):
        for test in range(tests):
            self.population = Population(Problem.initialPopulation)
            self.run(Problem.generations)
            print(self.population.bestIndividual())


def main():
    app = Algorithm(Problem.initialPopulation)
    app.run(Problem.generations)
    app.statistics(Problem.generations, Problem.tests)

if __name__ == '__main__':
    main()


