#!/bin/python3
from random import random, uniform
from math import sin, sqrt, exp, pi
from collections import deque
from random import choice

import matplotlib.pyplot as plt
import logging

"""
https://en.wikipedia.org/wiki/Particle_swarm_optimization

Let S be the number of particles in the swarm, each having a position xi ∈ ℝn in the search-space and a velocity vi ∈ ℝn. Let pi be the best known position of particle i and let g be the best known position of the entire swarm. A basic PSO algorithm is then:

for each particle i = 1, ..., S do
   Initialize the particle's position with a uniformly distributed random vector: xi ~ U(blo, bup)
   Initialize the particle's best known position to its initial position: pi ← xi
   if f(pi) < f(g) then
       update the swarm's best known  position: g ← pi
   Initialize the particle's velocity: vi ~ U(-|bup-blo|, |bup-blo|)
while a termination criterion is not met do:
   for each particle i = 1, ..., S do
      for each dimension d = 1, ..., n do
         Pick random numbers: rp, rg ~ U(0,1)
         Update the particle's velocity: vi,d ← ω vi,d + φp rp (pi,d-xi,d) + φg rg (gd-xi,d)
      Update the particle's position: xi ← xi + vi
      if f(xi) < f(pi) then
         Update the particle's best known position: pi ← xi
         if f(pi) < f(g) then
            Update the swarm's best known position: g ← pi
"""


class Problem:
    functions = {
        'McCormick': {
            'xMin': -1.5,
            'xMax': 4,
            'yMin': -3,
            'yMax': 4,
            'population': 100,
            'maxIterations': 500,
            'omega': 0.01,
            'phiP': 0.06,
            'phiG': 0.03,
            'particleSize': 4,
            'f': lambda p: sin(p[0] + p[1]) + (p[0] - p[1])**2.0 - 1.5 * p[0] + 2.5 * p[1] + 1.0
        },
        'Cross-in-tray': {
            'xMin': -10,
            'xMax': 10,
            'yMin': -10,
            'yMax': 10,
            'population': 100,
            'maxIterations': 10000,
            'omega': 0.005,
            'phiP': 0.025,
            'phiG': 0.025,
            'particleSize': 3,
            'f': lambda p: -0.0001 * (abs(sin(p[0]) * sin(p[1]) * exp(100 - sqrt(p[0]**2 + p[1]**2) / pi)) + 1) ** 0.1
        },
    }

    # config = functions['McCormick']
    config = functions['Cross-in-tray']

    xBound = abs(config['xMax'] - config['xMin'])
    yBound = abs(config['yMax'] - config['yMin'])

    clearBetweenIterations = False
    iterationsShown = 10

    def fitness(position):
        return Problem.config['f'](position)


class Particle:

    def __init__(self, swarm=None):
        self.setRandomPosition()
        self.setRandomVelocity()

        self.bestPosition = self.position
        self.swarm = swarm

    def __repr__(self):
        return "Particle(%.2f, %.2f)" % self.position

    def setRandomPosition(self):
        x = uniform(Problem.config['xMin'], Problem.config['xMax'])
        y = uniform(Problem.config['yMin'], Problem.config['yMax'])

        self.position = (x, y)

    def setRandomVelocity(self):
        dx = uniform(-Problem.xBound, Problem.xBound)
        dy = uniform(-Problem.yBound, Problem.yBound)

        self.velocity = (dx, dy)

    def updateVelocity(self, i, p, g):
        newComponent = Problem.config['omega'] * self.velocity[i] \
            + Problem.config['phiP'] * p * (self.bestPosition[i] - self.position[i]) \
            + Problem.config['phiG'] * g * \
            (self.swarm.bestPosition[i] - self.position[i])

        if i == 0:
            self.velocity = (newComponent, self.velocity[1])
        else:
            self.velocity = (self.velocity[0], newComponent)

    def updatePosition(self):
        newX = max(min(self.position[0] + self.velocity[0],
                       Problem.config['xMax']), Problem.config['xMin'])
        newY = max(min(self.position[1] + self.velocity[1],
                       Problem.config['yMax']), Problem.config['yMin'])

        self.position = (newX, newY)

    def updateBestPosition(self):
        self.bestPosition = self.position


class SwarmPlot:

    def __init__(self, swarm):
        self.swarm = swarm
        self.currentlyPlotted = {
            'iterations': deque([]),
            'text': None
        }

    def clearOldPoints(self):
        iterations = self.currentlyPlotted['iterations']
        if len(iterations) > Problem.iterationsShown:
            iterations[0].remove()
            iterations.popleft()

    def plotCurrentIteration(self):
        plt.axis([Problem.config['xMin'], Problem.config['xMax'],
                  Problem.config['yMin'], Problem.config['yMax']])

        particles = self.swarm.particles
        xs = list(map(lambda particle: particle.position[0], particles))
        ys = list(map(lambda particle: particle.position[1], particles))

        newIteration = plt.scatter(xs, ys, s=Problem.config['particleSize'])
        self.currentlyPlotted['iterations'].append(newIteration)

    def plotText(self, iteration):
        bestPosition = self.swarm.bestPosition
        bestFitness = Problem.config['f'](bestPosition)

        figureText = 'Iteration %i\n' % iteration + \
                     'Best particle: (%.5f, %.5f)\n' % bestPosition + \
                     'Best fitness:  %.6f' % bestFitness

        self.currentlyPlotted['text'] = plt.text(
            Problem.config['xMin'] + 0.1,
            Problem.config['yMax'] - 0.7,
            figureText,
            bbox=dict(facecolor='blue', alpha=0.4),
            fontsize=10,
            family="Ubuntu Mono")

    def clearOldText(self):
        if self.currentlyPlotted['text'] is not None:
            self.currentlyPlotted['text'].remove()

    def plotEverything(self, iteration):
        if Problem.clearBetweenIterations:
            plt.clf()

        self.plotCurrentIteration()
        self.clearOldPoints()
        self.clearOldText()
        self.plotText(iteration)
        plt.pause(0.0001)


class Swarm:

    def __init__(self, population):
        self.particles = [Particle(swarm=self) for _ in range(population)]
        self.bestPosition = min([x.position for x in self.particles],
                                key=lambda x: Problem.fitness(x))
        self.plot = SwarmPlot(self)

    def simulate(self):
        for iteration in range(Problem.config['maxIterations']):
            logging.info("ITERATION #%i" % iteration)

            self.plot.plotEverything(iteration)

            for particle in self.particles:
                for dimension in range(2):
                    p, g = random(), random()
                    particle.updateVelocity(dimension, p, g)
                particle.updatePosition()

                if Problem.fitness(particle.position) < Problem.fitness(particle.bestPosition):
                    particle.updateBestPosition()

                    if Problem.fitness(particle.bestPosition) < Problem.fitness(self.bestPosition):
                        self.bestPosition = particle.bestPosition

            logging.info("CURRENT SOLUTION: (%.3f, %.3f)" % self.bestPosition)


def main():
    swarm = Swarm(Problem.config['population'])
    swarm.simulate()

    logging.info("Found best position = (%.6f, %.6f) " % swarm.bestPosition +
                 "after %i iterations" % Problem.config['maxIterations'])

if __name__ == '__main__':
    main()
